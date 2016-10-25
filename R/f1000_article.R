#' F1000 article format
#' 
#' Format for creating F1000 software articles.
#'
#' Creates a latex file that can be uploaded to F1000 Overleaf
#' 
#' @inheritParams rmarkdown::pdf_document
#' @param toc TRUE to include a table of contents in the output
#' @param number_sections TRUE to number section headings
#' @param keep_tex Keep the intermediate tex file used in the conversion to PDF
#' @param ... Arguments to \code{rmarkdown::pdf_document}
#' 
#' @return R Markdown output format to pass to
#'   \code{\link[rmarkdown:render]{render}}
#'
#' @examples
#'
#' \dontrun{
#' rmarkdown::draft("MyArticle.Rmd", template="f1000_article", package="BiocWorkflowTools")
#' }
#'
#' @importFrom bookdown pdf_book
#' 
#' @export
f1000_article <- function(toc = FALSE,
                          number_sections = FALSE,
                          keep_tex = TRUE,
                          ...) {
  
  base_format_postprocessed <- function(toc,
                                        number_sections,
                                        keep_tex,
                                        ...) {
    
    template <- system.file("rmarkdown", "templates", "f1000_article", "resources", "template.tex",
                            package = "BiocWorkflowTools")
    
    config <- rmarkdown::pdf_document(toc = toc,
                                      number_sections = number_sections,
                                      keep_tex = keep_tex,
                                      template = template,
                                      ...)
    
    config$post_processor <- function(metadata, input, output, clean, verbose) {
      lines <- readUTF8(output)
      
      ## insert author affiliations
      lines <- BiocStyle:::modifyLines(lines,
                                       from = '%% AUTH AFFIL %%',
                                       insert = BiocStyle:::auth_affil_latex(metadata))
      
      ## convert instances of \longtable to \table
      lines <- .processPandocTables(lines)
      
      writeUTF8(lines, output)
      
      output
    }
    
    config
  }
  
  # use bookdown::pdf_book because of the added capability of cross-referencing
  pdf_book(toc = toc,
           number_sections = number_sections,
           keep_tex = keep_tex,
           ...,
           base_format = base_format_postprocessed)
  
}
