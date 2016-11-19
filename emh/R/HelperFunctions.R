

.check_data <- function(data) {
  if (!is.numeric(data) && !is.zoo(data))
    stop("tests only work with a vector or zoo")
}
