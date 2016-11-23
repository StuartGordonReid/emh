#' @title Rolling Time Series Permutation.
#' @description Produce a variation of the given time series by means of a rolling-window permutation transformation.
#'
#' @param logrets zoo :: A logarithmic return time series.
#' @param window numeric :: The window size over which to permute.
#' @param cluster SOCKCluster :: A cluster for running this in parallel.
#' @param overlapping logical :: Compute overlapping permutations? TRUE or FALSE.
#' @return The logarithmic returns shuffled locally in time in time chunks window-size big.
#'
#' @details This function is used to compute a rolling-window permutation of the given time series. This permutation
#' has the same moments of the original data over any length of returns sufficiently larger than the window size but
#' has a randomized order. This is a good benchmark for randomness tests because it does not assume any prior returns
#' distribution, also called a specification, against which the returns are being tested e.g. Gaussian. Two options are
#' available for the permutation: overlapping or non-overlapping. Overlapping permutations do not place a strict bound
#' on the distance any two originally subsequent returns may be from one another in the permutated returns.
#'
simulate_permutation <- function(logrets, window, cluster = NULL, overlapping = FALSE) {
  tau <- length(logrets)
  if (overlapping) {
    logrets.vec <- as.vector(logrets)
    for (t in 1:(length(logrets.vec) - window))
      logrets.vec[t:(t + window - 1)] <- sample(logrets.vec[t:(t + window - 1)], window)
    return(zoo::zoo(logrets.vec, zoo::index(logrets)))
  } else {
    # Subset the logrets by the remainder.
    remainder <- tau %% window
    if (remainder != 0) {
      logrets <- logrets[1:(tau - remainder)]
      warning(paste("logrets length is not a multiple of the window size",
                    "discared the last", remainder, "logrets in the series."))
    }

    # Split logrets into a list of windows.
    m <- matrix(logrets, nrow = window)
    l <- lapply(apply(m, 2, list), unlist)

    # Shuffle the windows and return the shuffled data.
    if (is.null(cluster)) l <- lapply(l, sample, size = window)
    else l <- parLapply(cluster, l, sample, size = window)

    # Return the data as a zoo object.
    return(zoo::zoo(unlist(l), zoo::index(logrets)))
  }
}
