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


plot_rollvol <- function(returns) {
  returns.stdev <- rollapplyr(na.omit(returns), 30, sd) * sqrt(252)
  plot(returns.stdev, main = "30-day Rolling Annualized Volatility of the S&P 500",
       ylab = "Annualized Volatility", xlab = "Time")
}


plot_coloured_points <- function(points = 100, q = 2) {
  alls <- c()
  odds <- c()
  evens <- c()

  for (i in 1:points) {
    r1 <- runif(1, min = 0.25, max = 0.75)
    alls <- c(alls, r1)

    if (i %% q) {
      evens <- c(evens, r1)
      odds <- c(odds, NA)
    } else {
      odds <- c(odds, r1)
      evens <- c(evens, NA)
    }
  }

  plot(alls, pch = 17, col = 'black', ylim = c(0, 1), ylab = "Random Disturbance",
       xlab = "Time, t", main = "All Random Disturbances")
  plot(odds, pch = 19, col = 'darkgreen', ylim = c(0, 1), ylab = "Random Disturbance",
       xlab = "Time, t", main = "Two Subsets of Random Disturbances")
  points(evens, pch = 15, col = 'red')
}

