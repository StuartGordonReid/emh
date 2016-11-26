#' @title Bartell's rank test for serial correlation.
#' @description Performs the Breusch-Godfrey test for serial correlation.
#'
#' @param rets numeric :: time series returns. May be a zoo or numeric vector.
#' @param a numeric :: alpha. This controls the significance level of the results.
#'
test_bartellrank <- function(rets, a = 0.99, r = "lm", w = 126) {
  # Check and convert the data.
  .check_data(data = rets)
  rets <- as.numeric(rets)

  # Run the bartell's rank test in randtests.
  br <- randtests::bartels.rank.test(rets)

  # Get the test statistic (D) for the test.
  stat <- br$statistic

  # Get the p-value for the D statistic.
  p.value <- br$p.value

  # Determins the Z-score.
  z.score <- qnorm(p.value)

  # Compute the required threshold.
  thresh <- abs(qnorm((1 - a) / 2))

  # Return the results object.
  return(RandomnessResult("Bartell Rank", "Variance Ratio",
                          stat, p.value, z.score, abs(z.score) > thresh))
}
