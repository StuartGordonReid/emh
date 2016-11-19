

# # A simple 'emh' how to:
# library(emh)
#
# # Get a list of codes for Quandl.
# index.codes <- data.quandl.indices()
#
# # Download the corresponding time series (prices).
# index.prices <- data.quandl.downloader(index.codes)
#
# # Run randomness tests on GSPC returns at 99% confidence level.
# randomness.results <- is.random(index.prices$`YAHOO/INDEX_GSPC`, a = 0.99,
#                                 # The "lags" to compute returns over.
#                                 freqs1 = c(1, 2, 3, 3, 4, 5, 6, 7, 8, 9, 10),
#                                 # The time frames to compute returns over e.g. weekly.
#                                 freqs2 = c("Mon", "Tue", "Wed", "Thu", "Fri", "Week", "Month"))
#
# # View the results (a data.frame).
# View(randomness.results)


library(Quandl)
library(PerformanceAnalytics)

# djia.returns = Quandl("BCB/UDJIAD1", type = "zoo", transform = "rdiff")
djia.residuals.rolling = as.residuals.rolling(djia.returns, w = 126)
djia.market.premium.rolling = as.trend.rolling(djia.returns, w = 126)

djia.levels.returns = log(as.levels(log(djia.returns + 1)))
djia.levels.residuals.rolling = log(as.levels(log(djia.residuals.rolling + 1)))
djia.levels.market.premium.rolling = log(as.levels(log(djia.market.premium.rolling + 1)))

plot(djia.levels.returns, ylim = c(-1, 7),
     main = "Dow Jones Industrial Average 1896 - 2016",
     ylab = "log of price level", xlab = "time")
lines(djia.levels.market.premium.rolling, col = "grey")
lines(djia.levels.residuals.rolling, col = "red")
