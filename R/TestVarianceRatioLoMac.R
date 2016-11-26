#' @title Given a log price process, X, compute the Z-score which can be used
#' to accept or reject the hypothesis that the process evolved according to a
#' Brownian Motion model with drift and stochastic volatility.
#'
#' @description Given a log price process, X, and a sampling interval, q, this
#' method returns a Z score indicating the confidence we have that X evolved
#' according to a Brownian Motion mode with drift and stochastic volatility. This
#' heteroskedasticity-consistent variance ratio test essentially checks to see
#' whether or not the observed Mr statistic for the number of observations, is
#' within or out of the limiting distribution defined by the Asymptotic Variance.
#'
#' @param rets vector :: A log returns process.
#' @param a numeric :: The confidence interval to use.
#' @param q int :: The sampling interval for the estimator.
#'
test_vratio_lo_mac <- function(rets, a = 0.99, q = 2, r = "lm", w = 126) {
  # The number if returns over q.
  n <- floor(length(rets)/q)
  X <- log(as_levels(rets))
  X <- as.numeric(X)

  # Compute the variance ratio statistic.
  statistic <- sqrt(n * q) * .m_ratio(X, q)
  asymtotic_variance <- .calibrate_asymptotic_variance(X, q)
  z.score <- statistic / sqrt(asymtotic_variance)

  # Compute the p-value of the z-score.
  p.value <- pnorm(z.score)

  # Compute the required threshold.
  thresh <- abs(qnorm((1 - a) / 2))

  # Return the results object.
  return(RandomnessResult("Lo MacKinlay", "Variance Ratio",
                          statistic, p.value, z.score, abs(z.score) > thresh))
}


#' @title Estimator for the value of mu. Mu is the drift component in the
#' Geometric Brownian Motion model.
#'
#' @description Given a log price process, this function estimates the value of
#' mu. Mu is the daily component of the returns which is attributable to upward,
#' or downward, drift. This estimate can be annualized.
#'
#' @param X vector :: A log price process.
#' @param annualize logical :: Annualize the parameter estimate.
#' @return mu.est double :: The estimated value of mu.
#'
.calibrate_mu <- function(X, annualize = TRUE) {
  # Ensure the format of X is appropriate.
  X <- as.numeric(as.vector(X))

  # Estimate the value of mu.
  n <- length(X)
  mu.est <- (X[n] - X[1])/n

  if (!annualize) return(mu.est)
  else return(mu.est * 252)
}


#' @title Estimator for the value of sigma. Sigma is the standard deviation of
#' the random component of returns in the Geometric Brownian Motion model. This
#' estimate can be calculated in a biased or unbiased manner.
#'
#' @description Given a log price process and a parameter, q, which specifies
#' the sampling intervel this function estimates the value of Sigma. Sigma
#' represents the standarddeviation of the random disturbance component of daily
#' returns. This estimate can be annualized and can be computed in a biased or
#' unbiased manner.
#'
#' @details The parameter, q, specifies the sampling interval to use when
#' estimating sigma. When q = 1 the function will use every day's prices, when
#' q = 2 the function will use every second day's prices, and so on and so
#' forth. For a sufficient number of days the estimation of sigma under
#' different sampling intervales e.g. 2 and 4, should converge.
#'
#' @param X vector :: A log price process.
#' @param q int :: The sampling interval for the estimator.
#' @param annualize logical :: Annualize the parameter estimate. True or False.
#' @param unbiased logical :: Use the unbiased estimate. True or False.
#' @return sd.est double :: The estimated value of Sigma.
#'
.calibrate_sigma <- function(X, q = 1, annualize = TRUE, unbiased = TRUE) {
  # Get the estimate value for the drift component.
  mu.est <- .calibrate_mu(X, annualize = FALSE)

  # Ensure that the format of X is appropriate.
  X <- as.numeric(as.vector(X))

  # Calculate the number of times q goes into the length of X.
  n <- floor(length(X)/q)
  sd.est <- 0.0
  for (t in 2:n)
    sd.est <- sd.est + (X[t * q] - X[(t * q) - q] - (q * mu.est))^2

  # Calculate the average sigma using the unbiased or biased method.
  if (!unbiased) sd.est <- sd.est / (q * n)
  else sd.est <- sd.est / (q * n - 1)

  if (!annualize) return(sd.est)
  else return(sqrt((sd.est * 252)))
}


#' @title A more efficient estimator for the value of sigma. Sigma is the
#' standard deviation of the random component of returns in the Geometric
#' Brownian Motion model. This estimate can be calculated in an unbiased manner.
#'
#' @description Given a log price process and a parameter, q, which specifies
#' the sampling intervel this function estimates the value of Sigma. Sigma
#' represents the standarddeviation of the random disturbance component of
#' daily  returns. This estimate can be annualized and can be computed in
#' a biased or unbiased manner.
#'
#' @details The difference between this estimator and the estimator defined in
#' the .calibrate_sigma function is that this method makes use of overlapping
#' windows of log price data. Whilst this does increase the number of
#' observations and improve the accuracy of the estimator it is no longer
#' unbiased. That said, Monte Carlo simulations indicate that this bias is
#' negligable and, in fact, this estimator is more accurate than the one
#' defined by .calibrate_sigma.
#'
#' @inheritParams .calibrate_sigma
#' @return sd.est double :: The estimated value of Sigma.
#'
.calibrate_sigma_overlapping <- function(X, q = 1, annualize = TRUE,
                                      unbiased = TRUE) {
  # Get the estimate value for the drift component.
  mu.est <- .calibrate_mu(X, annualize = FALSE)

  # Ensure that the format of X is appropriate.
  X <- as.numeric(as.vector(X))

  # Calculate the number of times q goes into the length of X.
  n <- floor(length(X)/q)
  sd.est <- 0.0
  for (t in (q + 1):(n * q))
    sd.est <- sd.est + (X[t] - X[t - q] - (q * mu.est))^2

  # Calculate the average sigma using the unbiased or biased method.
  if (!unbiased) sd.est <- sd.est / (n * (q^2))
  else sd.est <- sd.est / ((q * ((n * q) - q + 1)) * (1 - (q / (n * q))))

  if (!annualize) return(sd.est)
  else return(sqrt((sd.est * 252)))
}


#' @title Estimator for the value of the asymptotic variance of the Mr statistic.
#' This is equivalent to a weighted sum of the asymptotic variances for each of
#' the autocorrelation co-efficients under the null hypothesis.
#'
#' @details Given a log price process, X, and a sampling interval, q, this
#' method is used to estimate the asymptoticvariance of the Mr statistic in the
#' presence of stochastic volatility. In other words, it is a heteroskedasticity
#' consistent estimator of the variance of the Mr statistic. This parameter is
#' used to estimate the probability that the given log price process was
#' generated by a Brownian Motion model with drift and stochastic volatility.
#'
#' @param X vector :: A log price process.
#' @param q int :: The sampling interval for the estimator.
#'
.calibrate_asymptotic_variance <- function(X, q) {
  avar <- 0.0
  for (j in 1:(q - 1)) {
    theta <- .calibrate_delta(X, q, j)
    avar <- avar + (2 * (q - j) / q) ^ 2 * theta
  }
  return(avar)
}


#' @title Helper function for the .calibrate_asymptotic_variance function.
#'
.calibrate_delta <- function(X, q, j) {
  # Get the estimate value for the drift component.
  mu.est <- .calibrate_mu(X, FALSE)

  # Estimate the asymptotice variance given q and j.
  n <- floor(length(X)/q)
  numerator <- 0.0
  for (k in (j + 2):(n * q)) {
    t1 <- (X[k] - X[k - 1] - mu.est)^2
    t2 <- (X[k - j] - X[k - j - 1] - mu.est)^2
    numerator <- numerator + (t1 * t2)
  }
  denominator <- 0.0
  for (k in 2:(n * q))
    denominator <- denominator + (X[k] - X[k - 1] - mu.est)^2

  # Compute and return the statistic.
  thetaJ <- (n * q * numerator) / (denominator^2)
  return(thetaJ)
}


#' @title Compute the Jd statistic.
#'
#' @description Compute the Jd statistic. The Jd statistic is the difference
#' between two estimate values for Sigma computed using the .calibrate_sigma
#' function for sampling intervals 1 and q. This statistic should converge to zero.
#'
#' @inheritParams .calibrate_sigma
#' @return Jd double :: The Jd statistic defined by Lo and MacKinlay.
#'
.j_differences <- function(X, q, annualize = FALSE, unbiased = TRUE) {
  # Estimate the value of sigma at sampling intervals 1 and q.
  sigmaA <- .calibrate_sigma(X, q = 1, annualize, unbiased)
  sigmaB <- .calibrate_sigma(X, q = q, annualize, unbiased)
  Jd <- sigmaA - sigmaB
  return(Jd)
}


#' @title Compute the Jr statistic.
#'
#' @description Compute the Jr statistic. The Jr statistic is the ratio of two
#' estimate values for Sigma computed using the .calibrate_sigma function for
#' sampling intervals 1 and q minus 1. This statistic should converge to zero.
#'
#' @inheritParams .calibrate_sigma
#' @return Jr double :: The Jr statistic defined by Lo and MacKinlay.
#'
.j_ratio <- function(X, q, annualize = FALSE, unbiased = TRUE) {
  # Estimate the value of sigma at sampling intervals 1 and q.
  sigmaA <- .calibrate_sigma(X, q = 1, annualize, unbiased)
  sigmaB <- .calibrate_sigma(X, q = q, annualize, unbiased)
  Jr <- (sigmaA / sigmaB) - 1
  return(Jr)
}


#' @title Compute the Md statistic.
#'
#' @description Compute the Md statistic. The Md statistic is the difference
#' between two estimate values for Sigma computed using the .calibrate_sigma
#' function for sampling intervals 1 and the estimate value for Sima computed
#' using the .calibrate_sigma_overlapping function for sampling inveral q.
#' This statistic should converge to zero.
#'
#' @inheritParams .calibrate_sigma
#' @return Md double :: The Md statistic defined by Lo and MacKinlay.
#'
.m_differences <- function(X, q, annualize = FALSE, unbiased = TRUE) {
  # Estimate the value of sigma at sampling intervals 1 and q.
  sigmaA <- .calibrate_sigma(X, q = 1, annualize, unbiased)
  sigmaB <- .calibrate_sigma_overlapping(X, q = q, annualize, unbiased)
  Md <- sigmaA - sigmaB
  return(Md)
}


#' @title Compute the Mr statistic.
#'
#' @description Compute the Mr statistic. The Md statistic is the ratio between
#' two estimate values for Sigma computed using the .calibrate_sigma function for
#' sampling intervals 1 and the estimate value for Sima computed using the
#' .calibrate_sigma_overlapping function for sampling inveral q minus 1.
#' This statistic should converge to zero.
#'
#' @inheritParams .calibrate_sigma
#' @return Mr double :: The Mr statistic defined by Lo and MacKinlay.
#'
.m_ratio <- function(X, q, annualize = FALSE, unbiased = TRUE) {
  # Estimate the value of sigma at sampling intervals 1 and q.
  sigmaA <- .calibrate_sigma(X, q = 1, annualize, unbiased)
  sigmaB <- .calibrate_sigma_overlapping(X, q = q, annualize, unbiased)
  Mr <- (sigmaA / sigmaB) - 1
  return(Mr)
}


# .m_ratio_test <- function(paths = 500, q = 2, stochastic.volatility = TRUE) {
#   values <- c()
#   for (i in 1:paths) {
#     mu.rand <- runif(1, min = -0.25, max = 0.25)
#     sd.rand <- runif(1, min = 0.005, max = 0.75)
#
#     X <- logPriceProcess(t = (252 * 10), mu = mu.rand, rd.sigma = sd.rand,
#                          stochastic.volatility = stochastic.volatility)
#
#     values <- c(values, .m_ratio(X, q = q))
#   }
#
#   if (stochastic.volatility) main <- "Values of Mr with Stochastic Volatility"
#   else main <- "Values of Mr without Stochastic Volatility"
#
#   plot(values, main = main, xlab = "Log Price Process Index",
#        ylab = paste("Value of Mr(q=", q, ")", sep = ''),
#        ylim = c(-0.175, 0.175))
# }
#
#
# .m_differences_test <- function(paths = 500, q = 2, stochastic.volatility = TRUE) {
#   values <- c()
#   for (i in 1:paths) {
#     mu.rand <- runif(1, min = -0.25, max = 0.25)
#     sd.rand <- runif(1, min = 0.005, max = 0.75)
#
#     X <- logPriceProcess(t = (252 * 10), mu = mu.rand, rd.sigma = sd.rand,
#                          stochastic.volatility = stochastic.volatility)
#
#     values <- c(values, .m_differences(X, q = q))
#   }
#
#   if (stochastic.volatility) main <- "Values of Md with Stochastic Volatility"
#   else main <- "Values of Md without Stochastic Volatility"
#
#   plot(values, main = main, xlab = "Log Price Process Index",
#        ylab = paste("Value of Md(q=", q, ")", sep = ''),
#        ylim = c(-0.00025, 0.00025))
# }
#
#
#
# .calibrate_mu_test <- function(real.mu = 0.15, samples = 30,
#                             stochastic.volatility = TRUE) {
#   # Start off the plot with the actual values (scatter)
#   plot(rep(real.mu, 50), ylab = "Mu Estimate", xlab = "Length of X (years)",
#        main = "Illustration of the paramator estimator for Mu",
#        ylim = c(real.mu - (2 * real.mu), real.mu + (2 * real.mu)))
#
#   # The add samples many estimates of mu from X's of length t.
#   for (i in 1:samples) {
#     mu.estimates <- c()
#     for (t in seq(252, (252 * 50), 252)) {
#       X <- logPriceProcess(t = t, mu = real.mu, rd.sigma = 0.25,
#                            stochastic.volatility = stochastic.volatility)
#       mu.estimates <- c(mu.estimates, .calibrate_mu(X))
#     }
#     lines(mu.estimates, col = colors(1)[292 + i], lw = 2)
#   }
# }
#
#
# .calibrate_sigma_test <- function(real.sigma = 0.5, samples = 30,
#                                stochastic.volatility = TRUE) {
#   # Start off the plot with the actual values (scatter)
#   plot(rep(real.sigma, 50), ylab = "Sigma Estimate", xlab = "Length of X (years)",
#        main = "Illustration of the paramator estimator for Sigma",
#        ylim = c(real.sigma - (0.05 * real.sigma), real.sigma + (0.05 * real.sigma)))
#
#   # The add samples many estimates of mu from X's of length t.
#   for (i in 1:samples) {
#     sigma.estimates <- c()
#     for (t in seq(252, (252 * 50), 252)) {
#       X <- logPriceProcess(t = t, mu = 0.1, rd.sigma = real.sigma,
#                            stochastic.volatility = stochastic.volatility)
#       sigma.estimates <- c(sigma.estimates, .calibrate_sigma_overlapping(X))
#     }
#     print(sigma.estimates)
#     lines(sigma.estimates, col = colors(1)[292 + i], lw = 2)
#   }
# }
#
#
# .mean_test <- function(sigma, samples = 100000) {
#   sigmas <- c()
#   for (i in 1:samples)
#     sigmas <- c(sigmas, randomDisturbance(mu = 0.0, sigma = sigma,
#                                           stochastic.volatility = TRUE))
#   return(sd(sigmas))
# }
