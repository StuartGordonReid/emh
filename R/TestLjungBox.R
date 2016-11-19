#' @title Ljung-Box portmanteau test that autocorrelations are non-zero.
#' @description Performs a Ljung-Box to see if autocorrelations are non-zero.
#'
#' @param rets numeric :: time series returns. May be a zoo or numeric vector.
#' @param a numeric :: alpha. This controls the significance level of the results.
#' @param n.lags numeric :: the number of lags to test up to.
#'
test_ljungbox <- function(rets, a = 0.99, n.lags = 15) {
  # Number of lags.
  k <- length(rets)

  # The denominator.
  den <- k - seq(1, n.lags)

  # Compute the lagged autocorrelations.
  autocors <- c()
  for (l in 1:n.lags) {
    rets.head <- head(rets, k - l)
    rets.tail <- tail(rets, k - l)
    correl <- cor(rets.head, rets.tail)
    autocors <- c(autocors, correl)
  }

  # Compute the test statistic (Q) for the test.
  stat <- k * (k + 2) * sum((autocors ^ 2) / den)

  # Extract the p-value for the Q statistic.
  p.value <- 1 - pchisq(stat, df = n.lags)

  # Determins the Z-score.
  z.score <- qnorm(p.value)

  # Compute the required threshold.
  thresh <- abs(qnorm((1 - a) / 2))

  # Return the results object.
  return(c(stat, p.value, z.score,
           abs(z.score) > thresh))
}
