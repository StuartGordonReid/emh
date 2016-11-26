.check_data <- function(data) {
  if (!is.numeric(data) && !zoo::is.zoo(data))
    stop("tests only work with a vector or zoo")
}


.read_test_data <- function(con) {
  lines <- readLines(con)
  lines <- lapply(lines, str_trim)
  binstr <- str_c(lines, collapse = '')
  bindat <- strsplit(binstr, '')
  return(do.call(strtoi, bindat))
}

