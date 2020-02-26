
readUTF8 = function(file, ...) readLines(con = file, encoding = 'UTF-8', warn = FALSE, ...)

writeUTF8 = function(text, ...) writeLines(enc2utf8(text), ..., useBytes = TRUE)


#' pandoc uses \longtable when converting markdown tables.  From what I 
#' understand this is hardcoded behaviour.
#' Since this isn't the table format specified by the f1000 template, 
#' we'll attempt to convert the pandoc output into one that matches.  
#' There are probably lots of caveats that aren't caught yet.
#' The general principle is to find each \longtable chunk and process it 
#' separately. The modified text is then re inserted into the document, 
#' and the original lines removed.
#' 
#' @importFrom stringr str_detect
#' @noRd
.processPandocTables <- function(lines) {
  
  ## find lines marking start and end of \longtable chunks and create a list of duples
  longtableLines <- which(str_detect(string = lines, pattern= "(\\\\begin|\\\\end)\\{longtable\\}.*$"))
  ## we do nothing if there are no tables
  if(length(longtableLines)) {
    longtableLines <- split(longtableLines, 
                            rep(1:(length(longtableLines)/2), each = 2))
    ## create a list where each entry is a table chunk
    tableList <- lapply(longtableLines, 
                        function(x, lines) { lines[(x[1]:x[2])] }, lines)
    
    ## fix each chunk and return a vector of lines, which we insert in the correct place
    fixed_table_lines <- sapply(tableList, .individualTable)
    lines[ do.call("rbind", longtableLines)[,1] ] <- fixed_table_lines
    
    ## flag all the original \longtable lines and remove
    rm_lines <- unlist(lapply(longtableLines, function(x) { (x[1]+1):(x[2]) }))
    lines <- lines[ -rm_lines ]
  }
  
  lines
}


#' This function expects to be passed a character vector containing the
#' lines from a single \longtable chunk in a latex document.  It then tries 
#' to reformat this to the standard \table environment and returns the results
#' as a single string seperated by new line characters
#' 
#' @importFrom stringr str_detect str_replace
#' @noRd
.individualTable <- function(lines) {
  
  ## find the column justifications
  ## we use these later once the tabledata environment is defined
  justLines <- which(str_detect(pattern = "^.*\\\\begin\\{longtable\\}.*$", string = lines))
  if(length(justLines)) {
    justEntries <- gsub(pattern = "^.*\\](\\{.*\\})", replacement = "\\1", x = lines[justLines])
  }
  
  ## header line comes after the first \toprule
  first_toprule <- min(which(str_detect(pattern = "^.*\\\\toprule$", string = lines)))
  last_endhead <- max(which(str_detect(pattern = "^.*\\\\endhead$", string = lines)))
  
  ## insert header line with latex tag, and remove all other parts of table header
  lines[first_toprule] <- paste0("\\centering\n\\begin{tabledata}", justEntries)
  lines[first_toprule+1] <- paste("\\header", lines[first_toprule+1])
  lines <- lines[-((first_toprule+2):(last_endhead))]
  
  ## lines between our new header and \bottomrule are data rows
  last_header <- max(which(str_detect(pattern = "^\\\\header", string = lines)))
  last_bottomrule <- max(which(str_detect(pattern = "^\\\\bottomrule$", string = lines)))
  lines[(last_header+1):(last_bottomrule-1)] <- paste("\\row", lines[(last_header+1):(last_bottomrule-1)])
  
  ## mark the end of the table data
  lines[last_bottomrule] <- "\\end{tabledata}"
  
  ## substitute longtable for regular table
  lines <- gsub(pattern = "^.*(\\\\begin\\{|\\\\end\\{)long(table\\}).*$", 
                replacement = "\\1\\2", 
                x = lines)
  
  ## add placement specifier 
  lines <- gsub(pattern = "^(\\\\begin\\{table\\})$", 
                replacement = "\\1[htbp]", 
                x = lines)
  
  ## fix the new lines
  lines <- str_replace(string = lines, 
                       pattern = "(^\\\\row.*|^\\\\header.*)\\\\tabularnewline$",
                       replacement = "\\1\\\\\\\\")
  lines <- str_replace(string = lines, 
                       pattern = "(^.*)\\\\tabularnewline$", 
                       replacement = "\\1")
  
  ## collapse into a single string seperated with new lines.
  paste(lines, collapse = "\n")
}

#' We use rmarkdown's function to find additional figures, bibtex files etc
#' and copy them to the same directory we're putting the final document.
#' 
#' @importFrom rmarkdown find_external_resources
#' @noRd
.copyExternalResources <- function( rmd, dest = NULL ) {
  files <- find_external_resources( rmd )[,'path']
  if(length(files)) {
    file.copy(from = file.path(dirname(rmd), files), 
              to = dest)
  }
}