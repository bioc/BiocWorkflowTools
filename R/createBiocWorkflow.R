#' Create a New Bioconductor Workflow Package
#' 
#' Uses \code{\link{create_package}} to set up a skeleton for a new Bioconductor workflow package.
#' @param path location to create new package. The last component of the path will be used as the package name.
#' @param description list of description values to override default values or add additional values.
#' @param rstudio if \code{TRUE}, creates an RStudio project file.
#' @param open if \code{TRUE}, opens the project in a new RStudio session.
#' @return File path to the R Markdown vignette (invisibly).
#' @examples
#' createBiocWorkflow(file.path(tempdir(), "MyWorkflow"), open = FALSE)
#' @importFrom usethis create_package
#' @importFrom rmarkdown draft
#' @importFrom rstudioapi isAvailable openProject
#' @export
createBiocWorkflow <- function(path, 
                               description = getOption("devtools.desc"), 
                               rstudio = TRUE,
                               open = rstudio) {
  
  create_package(path, c(description, list(Workflow = "true")), rstudio = rstudio, open = FALSE)
  vignette <- file.path(path, "vignettes", paste(basename(path), "Rmd", sep="."))
  dir.create( dirname(vignette) )
  vignette <- draft(vignette,
        create_dir = FALSE,
        template = "f1000_article",
        package = "BiocWorkflowTools",
        edit = FALSE)
  
  if ( isTRUE(open) && isAvailable() )
    openProject(path, newSession = TRUE)
  
  invisible(vignette)
}
