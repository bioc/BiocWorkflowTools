#' F1000Research article format
#' 
#' Format for creating F1000Research software tool articles.
#'
#' Creates LaTeX sources which can be submitted to F1000Research through Overleaf.
#' 
#' @inherit rmarkdown::pdf_document params return
#' @param fig_align Default alignment of figures. Possible values are "center" (default) "left" and "right".
#' @param ... Arguments to \code{\link{pdf_document}}
#'
#' @section Citations: 
#' 
#' R Markdown supports automatic generation of citations. You can find more information on
#' the markdown citation syntax in the
#' \href{http://rmarkdown.rstudio.com/authoring_bibliographies_and_citations.html}{Bibliographies
#' and Citations} article in the R Markdown online documentation.
#' 
#' A bibliography file can be specified using the \code{bibliography} metadata field in the document's YAML header. 
#' Metadata variables for customizing citation style include:
#' 
#' \describe{
#'    \item{\code{biblio-style}}{Bibliography style (e.g. "unsrtnat", "plainnat")}
#'    \item{\code{natbiboptions}}{Options to \code{natbib} LaTeX package (e.g. "number", "super", "round")}
#'    \item{\code{biblatexoptions}}{Options to \code{biblatex} LaTeX package}
#' }
#'
#' @examples
#'
#' \dontrun{
#' 
#' rmarkdown::draft("MyArticle.Rmd", template="f1000_article", package="BiocWorkflowTools")
#' }
#'
#' @importFrom bookdown pdf_book
#' @importFrom rmarkdown output_format
#' @importFrom tools file_path_sans_ext
#' 
#' @export
f1000_article <- function(toc = FALSE,
                          number_sections = FALSE,
                          fig_width = 5.67,
                          fig_height = fig_width,
                          fig_align = "center",
                          keep_tex = TRUE,
                          citation_package = "natbib",
                          md_extensions = "+link_attributes",
                          pandoc_args = "--wrap=preserve",
                          ...) {
  
  template_dir <-  system.file(package = 'BiocWorkflowTools', 
                               'rmarkdown', 'templates', 'f1000_article')
  template <- file.path(template_dir, 'resources', 'template.tex')
  template_files <- file.path(template_dir, 'skeleton', c('f1000_styles.sty', 'F1000header.png'))
  
  ## preprocessor setting pandoc variables
  pre_processor <- function(metadata, input_file, runtime, knit_meta, files_dir, output_dir) {
    args <- c()
    
    defaults <- list(
      documentclass = "extarticle",
      fontsize = "9pt",
      papersize = "a4",
      letcitecitep = if (isTRUE(grepl("round", metadata$natbiboptions, fixed=TRUE))) "yes" else "no"
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
    
    args
  }
  
  post_processor <- function(metadata, input, output, clean, verbose) {
    lines <- readUTF8(output)
    
    ## insert author affiliations
    lines <- BiocStyle:::modifyLines(lines,
                                     from = '%% AUTH AFFIL %%',
                                     insert = BiocStyle:::auth_affil_latex(metadata))
    
    ## convert instances of \longtable to \table
    lines <- .processPandocTables(lines)
    
    ## remove calls to pandoc's \tightlist macro
    lines <- lines[!grepl("^[[:blank:]]*\\\\tightlist$", lines)]
    
    writeUTF8(lines, output)
    
    output
  }
  
  # use `pre_knit` to copy f1000 template files as this seems to be the only 
  # function which is aware of the original input file name passed to `render` 
  pre_knit <- function(input, ...) {
    # copy f1000 style file & header
    file.copy(template_files, dirname(normalizePath(input)))
  }
  
  ## add a dummy geometry field to document front matter in order to prevent the
  ## default pre processor 'pdf_pre_processor' defined in 
  ## 'rmarkdown::pdf_document' from setting the 'geometry' variable to pandoc
  post_knit <- function(metadata, input_file, runtime, ...) {
    if ( is.null(metadata$geometry) ) {
      output_file <- sprintf("%s.knit.md", file_path_sans_ext(input_file))
      lines <- readUTF8(output_file)
      lines <- rmarkdown:::partition_yaml_front_matter(lines)
      front_matter <- lines$front_matter
      
      ## append geometry entry to yaml front matter
      l <- length(front_matter)
      front_matter <- c(front_matter[-l], "geometry: default", front_matter[l])
      
      writeUTF8(c(front_matter, lines$body), output_file)
    }
  }
  
  # run when document is rendered to a different directory than the input file
  intermediates_generator <- function(original_input, encoding, intermediates_dir) {
    ## copy f1000 style file & header
    file.copy(template_files, normalizePath(intermediates_dir))
    
    ## append style template files
    basename(template_files)
  }
  
  knitr <- list(opts_chunk = list(fig.align = fig_align))
  
  # use bookdown::pdf_book because of the added capability of cross-referencing
  config <- output_format(knitr = knitr, pandoc = NULL,
                          clean_supporting = FALSE,
                          pre_knit = pre_knit,
                          post_knit = post_knit,
                          pre_processor = pre_processor,
                          intermediates_generator = intermediates_generator,
                          post_processor = post_processor,
                          base_format = pdf_book(toc = toc,
                                                 number_sections = number_sections,
                                                 fig_width = fig_width,
                                                 fig_height = fig_height,
                                                 template = "default",
                                                 keep_tex = keep_tex,
                                                 citation_package = citation_package,
                                                 md_extensions = md_extensions,
                                                 pandoc_args = pandoc_args,
                                                 base_format = rmarkdown::pdf_document,
                                                 ...)
  )
  
  ## override the default document template which is used in order to retain
  ## some original template-dependent rmarkdown functionality
  pos <- match("--template", config$pandoc$args)
  if (is.na(pos)) {
    config$pandoc$args <- c(config$pandoc$args, "--template")
    pos <- length(config$pandoc$args)
  }
  config$pandoc$args[ pos + 1L ] <- template
  
  config
}
