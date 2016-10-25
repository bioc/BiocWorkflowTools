
readUTF8 = function(file, ...) readLines(con = file, encoding = 'UTF-8', warn = FALSE, ...)

writeUTF8 = function(text, ...) writeLines(enc2utf8(text), ..., useBytes = TRUE)
