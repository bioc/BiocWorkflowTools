#' Upload a LaTeX project to Overleaf
#' 
#' @param files Character vector of file names to upload.  If the first entry
#' is a zip file this is uploaded directly.  Otherwise the files will be added
#' to a zip archive and then uploaded.
#' @param forceNewProject Logical specifying if a new Overleaf project should
#' be create, even if the function detects this document has already has an
#' associated project.  Default value is FALSE.
#' @param openInBrowser Boolean determining whether to open a browser at the 
#' created Overleaf project or not. Default value is FALSE.
#' @param git Boolean specifying whether to initialize a local clone of the 
#' Overleaf project's git repository
#'
#' @return The URL where the uploaded project can be accessed is printed to the 
#'   screen and invisibly returned from the function. If the argument
#'   \code{openInBrowser} is set to \code{TRUE}, then the Overleaf project page
#'   will automatically open in the default browser.
#' 
#' @examples 
#' example_Rmd <- system.file('examples/f1000_software_example.Rmd', 
#'                            package = "BiocWorkflowTools")
#' output_dir <- file.path(tempdir(), 'example')
#' markdownToLatex(input = example_Rmd, output = output_dir, 
#'                 compress = TRUE)
#' \dontrun{
#' ## don't run this code chunk in the example as we don't want to spam Overleaf
#' zip_file <- paste0(output_dir, '.zip')
#' uploadToOverleaf(files = zip_file, openInBrowser = TRUE)
#' }                
#' 
#' @importFrom stringr str_replace str_replace_all
#' @importFrom utils browseURL zip unzip head
#' @importFrom tools file_ext
#' @importFrom httr POST upload_file content
#' @importFrom git2r fetch init remote_add 
#' 
#' @export
uploadToOverleaf <- function(files = NULL, forceNewProject = FALSE, openInBrowser = FALSE, git = TRUE) {
    if ( is.null(files) )
        stop("No directory or zip file specified")
        
    ## allow only a single directory or zip archive
    if ( length(files)!=1L ) 
      stop("Please provide path to a single file or directory")
  
    files = normalizePath(files)
    
    ## check whether the resource actually exists
    if ( !isTRUE(file.exists(files)) )
      stop("Please provide path to an existing file or directory")
    
    ## check whether the project has been already uploaded before 
    if (!forceNewProject)
      .checkForOverleafProject(files)
    
    is_zip <- file_ext(files)=="zip"
    
    zip_file <- if (is_zip) files else {
        ## zip the files up, even if there's only one
        tf <- tempfile(fileext = ".zip")
        zip(zipfile = tf, files = files)
        tf
    } 
    
    ## this is an irritating two step process. First we upload the zip file
    ## to a free file host. then we pass the URL for this to the overleaf API.
    ## Maybe we can improve this in the future?
    uploaded <- POST(url = 'https://transfer.sh/',
                     body = list(zip_file = upload_file(zip_file)))
    ## strip new line
    zip_url <- str_replace_all(
        httr::content(uploaded, encoding = "UTF-8", as = "parsed"),
        "[\r\n]" , "")
    ## post to overleaf
    overleaf_url <- POST(url = 'https://www.overleaf.com/docs',
                         body = list(zip_uri = zip_url))$url
    
    message("Overleaf project created at:\n\t", overleaf_url)
    
    overleaf_url_file <- file.path( ifelse(is_zip, tempdir(), files), overleaf_file)
    
    writeLines(text = overleaf_url, con = overleaf_url_file)
    
    ## add overleaf_project_url.txt to archive
    if (is_zip)
        zip(zipfile = zip_file, files = overleaf_url_file, flags = "-qj9X")
    ## initialize git repo
    else if ( isTRUE(git) ) {
      repo <- init(files)
      remote_add(repo, "origin", str_replace(overleaf_url, "//www\\.", "//git\\."))
      fetch(repo, "origin")
      ## the following call should eventually use `git2r::reset` once it's
      ## clear how to do this https://github.com/ropensci/git2r/issues/281
      system(sprintf("cd %s; git reset origin/master", files))
      ## ignore the overleaf project url token file
      writeLines(text = c(".gitignore", overleaf_file),
                 con = file.path( files, ".gitignore"))
    }
    
    if (openInBrowser)
        browseURL(url = overleaf_url)
    
    invisible(overleaf_url)
}

#' @noRd
overleaf_file <- "overleaf_project_url.txt"

#' @noRd
.checkForOverleafProject <- function(path) {
    exists <-
    if ( file_ext(path)=="zip" ) {
        if ( overleaf_file %in% unzip(path, list=TRUE)$Name ) {
            path = unzip(path, files=overleaf_file, junkpaths=TRUE, exdir=tempdir())
            TRUE
        }
        else FALSE
    }
    else {
      path = file.path(path, overleaf_file)
      file.exists(path)
    }
    
    if(exists) {
        overleaf_url <- readLines(path)[1L]
        stop('You have already uploaded this directory to an Overleaf project.\n',
             'The project can be found at:\n', overleaf_url, call. = FALSE  )    
    }
}
