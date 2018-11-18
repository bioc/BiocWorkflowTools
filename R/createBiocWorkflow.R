#' Create a New Bioconductor Workflow Package
#' 
#' Uses \code{\link{create}} to set up a skeleton for a new Bioconductor workflow package.
#' @param path location to create new package. The last component of the path will be used as the package name.
#' @param description list of description values to override default values or add additional values.
#' @param rstudio if \code{TRUE}, creates an RStudio project file.
#' @param ... other arguments passed to \code{\link{create}}.
#' @param open if \code{TRUE}, opens the RStudio project.
#' @return File path to the R Markdown vignete (invisibly).
#' @examples
#' createBiocWorkflow(file.path(tempdir(), "MyWorkflow"), open = FALSE)
#' @importFrom usethis create_package
#' @importFrom rmarkdown draft
#' @importFrom rstudioapi isAvailable openProject
#' @export
createBiocWorkflow <- function(path, 
                               description = getOption("devtools.desc"), 
                               rstudio = TRUE,
                               ...,
                               open = rstudio) {
  
  create_package(path, c(description, list(Workflow = "true")), rstudio = rstudio, ...)
  vignette <- file.path(path, "vignettes", paste(basename(path), "Rmd", sep="."))
  dir.create( dirname(vignette) )
  vignette <- draft(vignette,
        create_dir = FALSE,
        template = "f1000_article",
        package = "BiocWorkflowTools",
        edit = FALSE)
  
  if ( isTRUE(open) && isAvailable() )
    openProject(path)
  
  invisible(vignette)
}
