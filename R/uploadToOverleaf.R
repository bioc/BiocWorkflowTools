#' Upload a LaTeX project to Overleaf
#' 
#' @param path File path to a directory or a single zip file to be uploaded.
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
#' @importFrom tools file_ext file_path_sans_ext list_files_with_exts
#' @importFrom httr POST upload_file content
#' @importFrom git2r clone discover_repository reset 
#' 
#' @export
uploadToOverleaf <- function(path, forceNewProject = FALSE, openInBrowser = FALSE, git = FALSE) {
    if ( missing(path) )
        stop("No directory or zip file specified")
        
    ## allow only a single directory or zip archive
    if ( length(path)!=1L ) 
      stop("Please provide a path to a single directory or zip file")
  
    path = normalizePath(path)
    
    ## check whether the resource actually exists
    if ( !isTRUE(file.exists(path)) )
      stop("Please provide a path to an existing directory or file")
    
    ## check whether the project has been already uploaded before 
    if (!forceNewProject)
      .checkForOverleafProject(path)
    
    is_zip <- file_ext(path)=="zip"
    
    zip_file <- if (is_zip) path else {
        ## exclude any system directories or files, such as .git
        files_all = list.files(path, all.files=TRUE, full.names=TRUE, no..=TRUE)
        files_lst = list.files(path, full.names=TRUE)
        files_exclude = files_all[ !(files_all %in% files_lst) ]
        
        ## exclude pdf files matching tex sources
        files_tex = list_files_with_exts(path, "tex")
        files_pdf = list_files_with_exts(path, "pdf")
        files_exclude = c(files_exclude, files_pdf[ file_path_sans_ext(files_pdf) %in% file_path_sans_ext(files_tex) ])
        
        ## exclude Overleaf project token file
        files_exclude = c(files_exclude, list.files(path, pattern=sprintf("^%s$", overleaf_file), full.names=TRUE))
        
        ## filter for directories
        extras <- paste0(files_exclude, ifelse(file.info(files_exclude)$isdir, "/\\*", ""))
        extras <- sprintf("-x %s", paste(extras, collapse=" "))
        
        ## zip the files up, even if there's only one
        tf <- tempfile(fileext = ".zip")
        zip(zipfile = tf, files = path, flags = "-qr9X", extras = extras)
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
    
    overleaf_url_file <- file.path( ifelse(is_zip, tempdir(), path), overleaf_file)
    
    writeLines(text = overleaf_url, con = overleaf_url_file)
    
    ## add overleaf_project_url.txt to archive
    if (is_zip)
        zip(zipfile = zip_file, files = overleaf_url_file, flags = "-qj9X")
    ## initialize git repo
    else if ( isTRUE(git) ) {
      if ( is.null(discover_repository(path, ceiling=0L)) ) {
        temp_dir <- tempfile("")
        repo_url <- str_replace(overleaf_url, "//www\\.", "//git\\.")
        
        ## FIXME (AO)
        ## you need the devel version from https://github.com/ropensci/git2r
        ## in order to run the following code
        repo <- clone(repo_url, temp_dir, checkout=FALSE, progress=FALSE)
        
        ## it is necessary to check for the return value of the call to
        ## file.copy because otherwise the commands following it seem to execute
        ## before the files are actually copied
        if ( file.copy(file.path(temp_dir, ".git"), path, recursive=TRUE) ) {
          repo@path <- path
          reset(repo, "")
          ## use .gitignore to disable tracking of files which were not uploaded
          git_ignore <- union(c(".gitignore", overleaf_file), basename(files_exclude))
          writeLines(text = git_ignore, con = file.path( path, ".gitignore"))
        }
        else
          warning("Failed to initialize git repository")
      }
      else
        warning("Project directory already under version control, skipping git initialization.")
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
