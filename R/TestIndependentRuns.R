#' @title Runs Test for Randomness
#' @description Performs the runs test on binarized returns.
#'
#' @param rets numeric :: time series returns. May be a zoo or numeric vector.
#' @param a numeric :: alpha. This controls the significance level of the results.
#'
test_runs <- function(rets, a = 0.99, r = "lm", w = 126) {
  # Check and convert the data.
  .check_data(data = rets)
  binrets <- as_binary(rets)

  # Get the numbers of bits.
  k <- length(binrets)
  k.ones <- count_ones(binrets)
  k.zeros <- count_zeroes(binrets)

  # Calculate the expectations.
  mean <- ((2 * k.ones * k.zeros) / k) + 1
  variance <- ((mean - 1) * (mean - 2)) / (k - 1)

  # Calculate the number of runs.
  k.runs <- test_runs_number(binrets)

  # Compute the z score of k.runs.
  z.score <- (k.runs - mean) / sqrt(variance)

  # Compute the p-value of the z-score.
  p.value <- pnorm(z.score)

  # Compute the required threshold.
  thresh <- abs(qnorm((1 - a) / 2))

  # Return the results object.
  return(RandomnessResult("Independent Runs", "Runs",
                          k.runs, p.value, z.score,
                          abs(z.score) > thresh))
}


#' @title Count the number of ones in a binary sequence
#' @description Returns the count of the number of one symbols in a binary sequence.
#' @param binrets numeric :: A binary sequence e.g. c(0, 1, 0, 0, 1, 1)
#'
count_ones <- function(binrets) {
  return(cBitsOne(binrets))
}


#' @title Count the number of zeroes in a binary sequence
#' @description Returns the count of the number of zero symbols in a binary sequence.
#' @param binrets numeric :: A binary sequence e.g. c(0, 1, 0, 0, 1, 1)
#'
count_zeroes <- function(binrets) {
  return(cBitsZero(binrets))
}


#' @title Computes the number of independent runs in a binary sequence.
#' @description Returns the number of runs in a binary sequence.
#' @param binrets numeric :: A binary sequence e.g. c(0, 1, 0, 0, 1, 1)
#'
test_runs_number <- function(binrets) {
  return(cRunsNumber(binrets))
}


#' @title Computes the length of the longest independent run in a binary sequence.
#' @description Returns the length of the longest run in a binary sequence.
#' @param binrets numeric :: A binary sequence e.g. c(0, 1, 0, 0, 1, 1)
#'
test_runs_longest <- function(binrets) {
  return(cRunsLongest(binrets))
}

