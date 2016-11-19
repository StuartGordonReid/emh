
#' @title This function plots the distribution of the returns.
#' @description This function basically just fits a density kernel to the returns and plots it. This will allow you to
#' see whether or not the returns you are modelling or testing are fat-tailed or not.
#'
#' @param returns zoo or numeric vector :: The returns you want to plot the distribution of.
#'
plot_fattails <- function(returns) {
  # Trim the returns to make the plot more visible.
  returns <- as.xts(returns)
  returns[returns > 0.075] <- NA
  returns[returns < -0.075] <- NA
  returns <- as.zoo(na.omit(returns))

  # Get the density of the returns.
  returns.d <- density(returns)

  # Get the density of a normal distribution.
  normal.d <- density(rnorm(length(returns),
                            mean(returns),
                            sd(returns)))

  # Plot the density of the returns.
  plot(returns.d, col = rgb(0,0,1,1/4),
       ylab = "Probability %", xlab = "Daily Log Return",
       main = "Kernel Density of S&P 500 Daily Log Returns")
  polygon(returns.d, col = rgb(0,0,1,1/4))

  # Plot the density of the normal distribution.
  lines(normal.d, col = rgb(1,0,0,1/4), xlim = c(-8, 15))
  polygon(normal.d, col = rgb(1,0,0,1/4))

  # Add the legend to the graph of returns.
  legend("topleft", inset = 0.05, title = "Distributions", cex = 0.6,
         lwd = 10, lty = c(2, 2), col = c(rgb(0,0,1,1/4), rgb(1,0,0,1/4)),
         legend = c("Kernel Density of S&P 500 Returns",
                    "Kernerl Density of Normal Distribution"))
}


#' @title Plots the rolling volatility of a daily time series.
#' @description Financial time series are characterized by fat-tails, and stochastic volatility. This plot will plot
#' the rolling annualized volatility of a daily return series.
#'
#' @param returns zoo or numeric vector :: the return series you want to visualize the stochastic volatility of.
#'
plot_rollvol <- function(returns) {
  returns.stdev <- rollapplyr(na.omit(returns), 30, sd) * sqrt(252)
  plot(returns.stdev, main = "30-day Rolling Annualized Volatility of the S&P 500",
       ylab = "Annualized Volatility", xlab = "Time")
}


#' @title A handy function for plotting the results of the randomness tests by frequency and test name.
#' @description This function generates two plots. The first plot is the breakdown of the percentage of randomness
#' tests which indicated that the return series was non-random by frequency. The second plot is the breakdown of the
#' percentage of randomness tests which indicated that the return series was non-random by the type of the test.
#'
#' @param results_df data.frame :: A data.fram produced by is_random.
#'
plot_results <- function(results_df) {
  .plot_results_frequency(results_df)
  .plot_results_test_name(results_df)
}


.plot_results_frequency <- function(results_df) {
  frequencies <- unique(results_df$Frequency)
  pct_non_random <- c()
  for (fr in frequencies) {
    results_df_fr = results_df[results_df$Frequency == fr, ]
    pct_non_random <- c(pct_non_random,
                        sum(results_df_fr$Non_Random) /
                          length(results_df_fr$Non_Random))
  }
  names(pct_non_random) = frequencies
  par(las=2, mar=c(7.5,4.1,4.1,2.1), mgp=c(4, 1, 0))
  barplot(pct_non_random, main = "Percentage of tests with non-random result by frequency", cex.names=0.7)
}


.plot_results_test_name <- function(results_df) {
  test_names <- unique(results_df$Test_Name)
  pct_non_random <- c()
  for (fr in test_names) {
    results_df_fr = results_df[results_df$Test_Name == fr, ]
    pct_non_random <- c(pct_non_random,
                        sum(results_df_fr$Non_Random) /
                          length(results_df_fr$Non_Random))
  }
  names(pct_non_random) = test_names
  par(las=2, mar=c(7.5,4.1,4.1,2.1), mgp=c(4, 1, 0))
  barplot(pct_non_random, main = "Percentage of tests with non-random result by test name", cex.names=0.7)
}


.plot_results_sample_size <- function(results_df) {
  sample_sizes <- unique(results_df$Sample_Size)
  pct_non_random <- c()
  for (fr in sample_sizes) {
    results_df_fr = results_df[results_df$Sample_Size == fr, ]
    pct_non_random <- c(pct_non_random,
                        sum(results_df_fr$Non_Random) /
                          length(results_df_fr$Non_Random))
  }
  names(pct_non_random) = sample_sizes
  par(las=2, mar=c(7.5,4.1,4.1,2.1), mgp=c(4, 1, 0))
  barplot(pct_non_random, main = "Percentage of tests with non-random result by sample size", cex.names=0.7)
}


# plot_coloured_points <- function(points = 100, q = 2) {
#   alls <- c()
#   odds <- c()
#   evens <- c()
#
#   for (i in 1:points) {
#     r1 <- runif(1, min = 0.25, max = 0.75)
#     alls <- c(alls, r1)
#
#     if (i %% q) {
#       evens <- c(evens, r1)
#       odds <- c(odds, NA)
#     } else {
#       odds <- c(odds, r1)
#       evens <- c(evens, NA)
#     }
#   }
#
#   plot(alls, pch = 17, col = 'black', ylim = c(0, 1), ylab = "Random Disturbance",
#        xlab = "Time, t", main = "All Random Disturbances")
#   plot(odds, pch = 19, col = 'darkgreen', ylim = c(0, 1), ylab = "Random Disturbance",
#        xlab = "Time, t", main = "Two Subsets of Random Disturbances")
#   points(evens, pch = 15, col = 'red')
# }

