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
#'
#' @return No value is returned.  The URL where the uploaded project can be 
#' accessed is printed to the screen.  If the argument \code{openInBrowser} is 
#' set to \code{TRUE}, then the default browser will automatically open at the 
#' Overleaf project page.
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
#' @importFrom stringr str_replace_all
#' @importFrom utils browseURL zip unzip head
#' @importFrom tools file_ext
#' @importFrom httr POST upload_file content
#' 
#' @export
uploadToOverleaf <- function(files = NULL, forceNewProject = FALSE, openInBrowser = FALSE) {
    
    if(is.null(files)) {
        stop("No file(s) specified")
    }    
        
    is_zip <- file_ext(files[1]) == "zip"
    if(!is_zip) {
        
        if(!forceNewProject)
            .checkForOverleafProject(files)
        ## zip the files up, even if there's only one
        tf <- tempfile(fileext = ".zip")
        zip(zipfile = tf, files = files)
        #files[1] <- tf
    } else { ## here we have a zip already
        zip_contents <- unzip(files[1], list = TRUE)[,'Name']
        if(!forceNewProject)
            .checkOverleafProjectCode(zip_contents)
    }

    ## this is an irritating two step process. First we upload the zip file
    ## to a free file host. then we pass the URL for this to the overleaf API.
    ## Maybe we can improve this in the future?
    uploaded <- POST(url = 'https://transfer.sh/',
                     body = list(zip_file = upload_file(tf)))
    ## strip new line
    zip_url <- str_replace_all(
        httr::content(uploaded, encoding = "UTF-8", as = "parsed"),
        "[\r\n]" , "")
    ## post to overleaf
    tmp <- POST(url = 'https://www.overleaf.com/docs',
                body = list(zip_uri = zip_url))
    overleaf_url <- head(tmp)$url
    
    message("Overleaf project created at:\n\t", overleaf_url)
    
    if(is_zip) {
        tf <- file.path(tempdir(), 'overleaf_project_url.txt')
        writeLines(text = overleaf_url, con = tf)
        zip(zipfile = files[1], files = tf)
    } else {
        writeLines(text = overleaf_url, con = file.path(files[1], 'overleaf_project_url.txt'))
    }
    
    if(openInBrowser){
        browseURL(url = overleaf_url)
    }
}

.writeOverleafProjectCode <- function(folder, overleaf_url) {
    
    writeLines(text = overleaf_url, con = file.path(folder, 'overleaf_project_url.txt'))
    
}

.checkForOverleafProject <- function(folder) {
    
    exists <- any(grepl(x = list.files(folder), pattern = 'overleaf_project_url.txt'))
    if(exists) {
        overleaf_url <- readLines(file.path(folder, "overleaf_project_url.txt"))
        stop('You have already uploaded this directory to an Overleaf project.\n',
             'The project can be found at:\n', overleaf_url, call. = FALSE  )    
    }
}
