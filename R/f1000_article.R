#' F1000 article format
#' 
#' Format for creating F1000 software articles.
#'
#' Creates a latex file that can be uploaded to F1000 Overleaf
#' 
#' @inheritParams rmarkdown::pdf_document
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
#' @importFrom rmarkdown includes_to_pandoc_args
#' 
#' @export
f1000_article <- function(toc = FALSE,
                          number_sections = FALSE,
                          keep_tex = TRUE,
                          md_extensions = "+link_attributes",
                          extra_dependencies = NULL,
                          ...) {
  
  base_format_postprocessed <- function(toc,
                                        number_sections,
                                        keep_tex,
                                        extra_dependencies,
                                        ...) {
    
    template <- system.file("rmarkdown", "templates", "f1000_article", "resources", "template.tex",
                            package = "BiocWorkflowTools")
    
    config <- rmarkdown::pdf_document(toc = toc,
                                      number_sections = number_sections,
                                      keep_tex = keep_tex,
                                      template = template,
                                      extra_dependencies = extra_dependencies,
                                      ...)
    
    ## preprocessor to set pandoc variables
    config$pre_processor <- function(metadata, input_file, runtime, knit_meta, files_dir, output_dir) {
      args <- c()
      
      defaults <- list(
        documentclass = "extarticle",
        fontsize = "9pt",
        papersize = "a4",
        citationoptions = "numbers",
        letcitecitep = if (isTRUE(grepl("round", metadata$citationoptions, fixed=TRUE))) "yes" else "no"
      )
      
      ## set to default if not specified in document header
      vars <- defaults[!names(defaults) %in% names(metadata)]
      
      ## remove disabled boolean options
      vars <- vars[!tolower(vars) %in% c("no", "false")]
      
      ## format as pandoc command-line args
      if (length(vars) > 0L) {
        vars <- paste(names(vars), vars, sep="=")
        vars <- c(rbind(rep("--variable", length(vars)), vars))
        args <- c(args, vars)
      }
      
      ## following code copied from 'pdf_pre_processor' defined in 'rmarkdown::pdf_document'
      format_deps <- list()
      format_deps <- append(format_deps, extra_dependencies)
      
      if (rmarkdown:::has_latex_dependencies(knit_meta)) {
        all_dependencies <- if (is.null(format_deps)) list() else format_deps
        all_dependencies <- append(all_dependencies, rmarkdown:::flatten_latex_dependencies(knit_meta))
        filename <- tempfile()
        rmarkdown:::latex_dependencies_as_text_file(all_dependencies, filename)
        args <- c(args, includes_to_pandoc_args(includes(in_header = filename)))
      }
      
      args
    }
    
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
           extra_dependencies = extra_dependencies,
           ...,
           base_format = base_format_postprocessed)
  
}
