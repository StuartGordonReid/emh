#' @title Test whether or not a price series is random.
#' @description This function performs a number of randomness tests on price data at different frequencies.
#'
#' @details This function accepts a price series, S, and transforms it into a set of price changes over different
#' frequencies. The first set of frequencies specifies the order over which to compute the price changes whereas
#' the second set of frequencies specifies a point-in-time interval over which to compute the price changes. For
#' example if freqs1 = c(1, 2) the every-one-day and every-two-days price changes will be computed and if freqs2 =
#' c("Wed", "Week") the wednesday-to-wednesday and week-to-week price changes will be computed. Each set of price
#' changes is then fed through the set of all randomness tests given a confidence interval, a. The results of each
#' test - including the test statistic, two-sided p-value, sample size, and result - are tabulated and returned.
#'
#' @param S zoo :: A price series. MUST be a zoo object (for downsampling to work).
#' @param a numeric :: Controls the significance level of the tests e.g. 0.95 for 95 percent.
#' @param freqs1 numeric :: A vector containing the lags to use for the tests.
#' @param freqs2 character :: A vector containing time-based downsampling frequencies.
#' @return data.frame :: A data.frame containing all of the tabulated test results.
#'
is_random <- function(S, a = 0.99,
                      freqs1 = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10),
                      freqs2 = c("Mon", "Tue", "Wed", "Thu",
                                 "Fri", "Week", "Month")) {

  # Check that we have the right type of data.
  if (!is.zoo(S))
    stop("is.random only works with zoo time series.")

  # Extract the returns and log returns.
  results <- list(); logrets <- as_logreturns(S)

  # Create a progress bar.
  pb <- txtProgressBar(style = 3)
  nfreq <- length(freqs1) + length(freqs2)

  for (fq in freqs1) {
    # Downsample the data to this frequency.
    if (fq == 1) logrets.freq <- logrets
    else logrets.freq <- suppressWarnings(as_frequency(logrets, fq))
    logrets.freq <- na.omit(logrets.freq)

    # Construct the label for this frequency.
    fq.label <- paste("(t-", fq, " to t)", sep = "")

    # Run the tests at this frequency and add them to list.
    result <- .run_tests(logrets.freq, fq.label, a)
    results[[length(results) + 1]] <- result

    # Update the progress bar.
    setTxtProgressBar(pb, length(results) / nfreq)
  }

  for (fq in freqs2) {
    # Downsample the data to this frequency.
    logrets.freq <- suppressWarnings(as_frequency(logrets, fq))
    logrets.freq <- na.omit(logrets.freq)

    # Construct the label for this frequency.
    fq.label <- paste("(", fq, " to ", fq, ")", sep = "")

    # Run the tests at this frequency and add them to list.
    result <- .run_tests(logrets.freq, fq.label, a)
    results[[length(results) + 1]] <- result

    # Update the progress bar.
    setTxtProgressBar(pb, length(results) / nfreq)
  }

  # Merge all of the results together.
  results <- do.call(rbind, results)
  rownames(results) <- seq(1, nrow(results))
  return(results)
}


.run_tests <- function(r, fq, a = 0.99) {
  # Run each one of the statistical tests on the returns.
  result.runs <- round(test_runs(r, a = a), 6)
  result.dw <- round(test_durbinwatson(r, a = a), 6)
  result.lb <- round(test_ljungbox(r, a = a), 6)
  result.bg <- round(test_breuschgodfrey(r, a = a), 6)
  results.br <- round(test_bartellrank(r, a = a), 6)
  result.vr1 <- round(test_vratio_lo_mac(r, a = a), 6)

  # Add all of the results from the statistical tests together.
  results <- as.data.frame(t(data.frame(result.runs,
                                        result.dw,
                                        result.lb,
                                        result.bg,
                                        results.br,
                                        result.vr1)))

  # Construct a data frame with all the results and relevant information.
  colnames(results) <- c("Statistic", "Two_sided_p", "Z_Score", "Non_Random")
  results$Sample_Size <- rep(length(r), nrow(results))
  results$Non_Random <- as.logical(results$Non_Random)
  results$Frequency <- rep(fq, nrow(results))

  # Add the test names to the data frame.
  results$Test_Name <- c("Independent Runs",
                         "Durbin-Watson",
                         "Ljung-Box",
                         "Breusch-Godfrey",
                         "Bartell Rank",
                         "Variance-Ratio LoMac")

  # Return the results of the statistical tests in a nice table.
  return(results[,c("Test_Name", "Frequency", "Sample_Size",
                    "Statistic", "Two_sided_p", "Z_Score", "Non_Random")])
}
