#' @title Breusch-Godfrey test for serial correlation.
#' @description Performs the Breusch-Godfrey test for serial correlation.
#'
#' @param rets numeric :: time series returns. May be a zoo or numeric vector.
#' @param a numeric :: alpha. This controls the significance level of the results.
#'
test_breuschgodfrey <- function(rets, a = 0.99) {
  # Check and convert the data.
  .check_data(data = rets)
  rets <- as.numeric(rets)

  # Now construct the data frame.
  k <- length(rets)
  y.var <- tail(rets, k - 1)
  x.var <- head(rets, k - 1)
  data <- data.frame(y.var, x.var)

  # Use lmtest to compute the p-values.
  colnames(data) <- c("y", "x")

  # Fit the linear model.
  lmfit <- lm(formula = y ~ x,
              data = data)

  # Now compute the breusch-godfrey statistic.
  bg <- lmtest::bgtest(formula = lmfit)

  # Get the test statistic (D) for the test.
  stat <- bg$statistic

  # Get the p-value for the D statistic.
  p.value <- bg$p.value

  # Determins the Z-score.
  z.score <- qnorm(p.value)

  # Compute the required threshold.
  thresh <- abs(qnorm((1 - a) / 2))

  # Return the results object.
  return(c(stat, p.value, z.score,
           abs(z.score) > thresh))
}
