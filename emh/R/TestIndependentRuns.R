

#' @title Runs Test for Randomness
#' @description Performs the runs test on binarized returns.
#'
#' @details TODO
#'
#' @param rets numeric :: time series returns. May be a zoo or numeric vector.
#' @param a numeric :: alpha. This controls the significance level of the results.
#'
test.runs <- function(rets, a = 0.99) {
  # Check and convert the data.
  .check.data(data = rets)
  binrets <- as.binary(rets)

  # Get the numbers of bits.
  k <- length(binrets)
  k.ones <- count.ones(binrets)
  k.zeros <- count.zeroes(binrets)

  # Calculate the expectations.
  mean <- ((2 * k.ones * k.zeros) / k) + 1
  variance <- ((mean - 1) * (mean - 2)) / (k - 1)

  # Calculate the number of runs.
  k.runs <- test.runs.number(binrets)

  # Compute the z score of k.runs.
  z.score <- (k.runs - mean) / sqrt(variance)

  # Compute the p-value of the z-score.
  p.value <- pnorm(z.score)

  # Compute the required threshold.
  thresh <- abs(qnorm((1 - a) / 2))

  # Return the results object.
  return(c(k.runs, p.value, z.score,
           abs(z.score) > thresh))
}


#' @title Count the number of ones in a binary sequence
#' @description Returns the count of the number of one symbols in a binary sequence.
#' @param binrets numeric :: A binary sequence e.g. c(0, 1, 0, 0, 1, 1)
#'
count.ones <- function(binrets) {
  return(cBitsOne(binrets))
}


#' @title Count the number of zeroes in a binary sequence
#' @description Returns the count of the number of zero symbols in a binary sequence.
#' @param binrets numeric :: A binary sequence e.g. c(0, 1, 0, 0, 1, 1)
#'
count.zeroes <- function(binrets) {
  return(cBitsZero(binrets))
}


#' @title Computes the number of independent runs in a binary sequence.
#' @description Returns the number of runs in a binary sequence.
#' @param binrets numeric :: A binary sequence e.g. c(0, 1, 0, 0, 1, 1)
#'
test.runs.number <- function(binrets) {
  return(cRunsNumber(binrets))
}


#' @title Computes the length of the longest independent run in a binary sequence.
#' @description Returns the length of the longest run in a binary sequence.
#' @param binrets numeric :: A binary sequence e.g. c(0, 1, 0, 0, 1, 1)
#'
test.runs.longest <- function(binrets) {
  return(cRunsLongest(binrets))
}

