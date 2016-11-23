#' @title Download a set of codes from Quandl.com.
#' @description This function downloads a set of codes form Quandl.com. The best variable for each one of the datasets
#' is extracted by defaults. For equity time series this usually corresponds to the adjusted close.
#'
#' @param codes character :: a list of characters containing codes.
#' @return zoo :: A zoo object containing daily prices for each code.
#'
data_quandl_downloader <- function(codes) {
  # The generic Quandl API key for TuringFinance.
  Quandl::Quandl.api_key("t6Rn1d5N1W6Qt4jJq_zC")
  print("DOWNLOADING DATASETS ...")

  # Create a progress bar.
  pb <- txtProgressBar(style = 3)

  # Download the data.
  datasets <- list()
  for (i in 1:length(codes)) {
    tryCatch(expr = {
      # Download the raw price data from Quandl.com.
      data <- Quandl::Quandl(codes[i], type = "zoo", collapse = "daily")

      # Get the indices of the various possible values.
      adjusted <- which(colnames(data) == "Adjusted Close")
      close <- which(colnames(data) == "Close")
      rate <- which(colnames(data) == "Rate")

      # Extract the variable and add it to the datasets.
      closes <- data[ ,max(adjusted, close, rate)]
      datasets[[length(datasets) + 1]] <- closes
    }, error = function(e) {
      warning(paste("Error downloading", codes[i], e))
      codes <- codes[!codes %in% codes[i]]
    })
    setTxtProgressBar(pb, i / length(codes))
  }
  names(datasets) <- codes
  return(datasets)
}
