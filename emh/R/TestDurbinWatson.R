

#' @title Durbin-Watson test for serial correlation.
#' @description Performs the Durbin-Watson test for serial correlation.
#'
#' @details TODO
#'
#' @param rets numeric :: time series returns. May be a zoo or numeric vector.
#' @param a numeric :: alpha. This controls the significance level of the results.
#'
test.durbinwatson <- function(rets, a = 0.99) {
  # Check and convert the data.
  .check.data(data = rets)
  rets <- as.numeric(rets)

  # Now construct the data frame.
  k <- length(rets)
  lags <- head(rets, k - 1)
  rets <- tail(rets, k - 1)
  data <- data.frame(rets, lags)

  # Use lmtest to compute the p-values.
  colnames(data) <- c("x", "y")
  dw <- lmtest::dwtest(formula = x ~ y, data = data,
                       alternative = "two.sided")

  # Get the test statistic (D) for the test.
  stat <- dw$statistic

  # Get the p-value for the D statistic.
  p.value <- dw$p.value

  # Determins the Z-score.
  z.score <- qnorm(p.value)

  # Compute the required threshold.
  thresh <- abs(qnorm((1 - a) / 2))

  # Return the results object.
  return(c(stat, p.value, z.score,
           abs(z.score) > thresh))
}


.test.durbinwatson.nop <- function(residuals) {
  if (is.zoo(residuals)) {
    residuals <- as.numeric(residuals)
  } else if (!is.numeric(residuals)) {
    stop("test.durbinwatson only works with a vector or zoo")
  }

  k <- length(residuals)
  sumr2 <- sum(residuals ^ 2)
  diffs <- cAsDifferences(residuals, 1)
  sumd2 <- sum(diffs ^ 2)
  d <- sumd2 / sumr2
  return(c(d, d < 1))
}

