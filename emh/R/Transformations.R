

as_levels <- function(logrets) {
  if (is.zoo(logrets)) {
    dates <- index(logrets)
    levels <- cAsLevels(as.numeric(logrets))
    return(zoo(levels, dates))
  } else if (!is.numeric) {
    stop("Please supply log returns as a vector or zoo.")
  }
  return(cAsLevels(logrets))
}


as_logreturns <- function(levels, order = 1) {
  if (is.zoo(levels)) {
    dates <- index(levels)
    levels <- as.numeric(levels)
    logrets <- cAsLogReturns(levels, order)
    return(zoo(logrets, tail(dates, length(logrets) - order)))
  } else if (!is.numeric(levels)) {
    stop("Please supply levels as a vector or zoo")
  }
  return(cAsLogReturns(levels, order))
}


as_returns <- function(levels, order = 1) {
  return(exp(as_logreturns(levels, order)) - 1)
}


as_binary <- function(rets) {
  if (is.zoo(rets)) {
    dates <- index(rets)
    rets <- as.numeric(rets)
    return(zoo(cAsBinary(rets), dates))
  } else if (!is.numeric(rets)) {
    stop("Please supply levels as a vector or zoo")
  }
  return(cAsBinary(rets))
}


as_frequency <- function(logrets.zoo, frequency) {
  if (!is.zoo(logrets.zoo))
    stop("as_frequency only works with zoo objects")

  if (frequency == 1)
    return(logrets.zoo)

  if (is.numeric(frequency)) {
    # Calculate the remainder.
    tau <- length(logrets.zoo)
    remainder <- tau %% frequency

    # Subset the returns by the remainder.
    if (remainder != 0) {
      logrets.zoo <- logrets.zoo[1:(tau - remainder)]
      warning(paste("logrets.zoo length is not a multiple of the frequency",
                    "discared the last", remainder, "logrets.zoo in the series."))
    }

    # Update tau and convert to vector.
    tau <- length(logrets.zoo)
    rvec <- as.vector(logrets.zoo)

    # Calculate the rolling frequency-period cumulative returns.
    rfreq <- rvec[seq(1, tau, frequency)]
    for (i in 2:frequency)
      rfreq <- rfreq + rvec[seq(i, tau, frequency)]

    # Return the cumulative returns at the new frequency as a zoo object.
    return(zoo(rfreq, index(logrets.zoo)[seq(frequency, tau, frequency)]))
  } else  {
    # First interpolate so all dates are present.
    logrets.zoo <- as_interpolated(logrets.zoo)

    # Get the dates.
    tau <- length(logrets.zoo)
    dates <- index(logrets.zoo)
    logrets.zoo <- as.vector(logrets.zoo)

    if (frequency %in% c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun")) {
      # Calculate the cumulative day-to-day returns.
      rfreq <- c(logrets.zoo[1]); ix <- 1
      for (t in 2:length(logrets.zoo)) {
        if (weekdays(dates[t], abbreviate = T) != frequency) {
          rfreq[ix] <- rfreq[ix] + logrets.zoo[t]
        } else {
          rfreq <- c(rfreq, logrets.zoo[t]); ix <- ix + 1
        }
      }

      # Return the cumulative returns as a zoo object.
      return(zoo(rfreq, dates[which(weekdays(dates, abbreviate = T) == frequency)]))
    } else if (frequency %in% "Week") {
      # Calculate the cumulative week to week returns.
      rfreq <- c(logrets.zoo[1]); ix <- 1
      previous <- week(dates[1])

      for (t in 2:length(logrets.zoo)) {
        if (week(dates[t]) == previous) {
          rfreq[ix] <- rfreq[ix] + logrets.zoo[t]
        } else {
          rfreq <- c(rfreq, logrets.zoo[t]); ix <- ix + 1
          previous <- week(dates[t])
        }
      }

      # Return the cumulative returns as a zoo object.
      weekdiffs <- head(week(dates), tau - 1) - tail(week(dates), tau - 1)
      weekdates <- dates[which(abs(weekdiffs) > 0)]
      return(zoo(rfreq, weekdates))
    } else if (frequency %in% "Month") {
      # Calculate the cumulative month to month returns.
      rfreq <- c(logrets.zoo[1]); ix <- 1
      previous <- month(dates[1])

      for (t in 2:length(logrets.zoo)) {
        if (month(dates[t]) == previous) {
          rfreq[ix] <- rfreq[ix] + logrets.zoo[t]
        } else {
          rfreq <- c(rfreq, logrets.zoo[t]); ix <- ix + 1
          previous <- month(dates[t])
        }
      }

      # Return the cumulative returns as a zoo object.
      monthdiffs <- head(month(dates), tau - 1) - tail(month(dates), tau - 1)
      monthdates <- dates[which(abs(monthdiffs) > 0)]
      return(zoo(rfreq, monthdates))
    }
  }
}


as_interpolated <- function(logrets.zoo) {
  if (!is.zoo(logrets.zoo))
    stop("as_frequency only works with zoo objects")

  # First interpolate the time series so all dates are present.
  dates.all <- seq(start(logrets.zoo), end(logrets.zoo), by = "day")
  zeros.zoo <- zoo(rep(0, length(dates.all)), dates.all)
  logrets.zoo <- rowSums(merge(logrets.zoo, zeros.zoo))

  # Fill all previously missing dates with 0.
  return(na.fill(zoo(logrets.zoo, dates.all), 0))
}


as_residuals <- function(rets) {
  dates <- NULL
  if (is.zoo(rets)) {
    dates <- index(rets)
    rets <- as.numeric(rets)
    logrets <- log(rets + 1)
    residuals <- logrets - mean(logrets)
    return(zoo(exp(residuals) - 1, dates))
  } else if (is.numeric(rets)) {
    logrets <- log(rets + 1)
    residuals <- logrets - mean(logrets)
    return(exp(residuals) - 1)
  } else {
    stop("Please supply levels as a vector or zoo")
  }
}


as_residuals_rolling <- function(rets, w = 126) {
  dates <- NULL
  if (is.zoo(rets)) {
    dates <- index(rets)
    rets <- as.numeric(rets)
    residuals <- cAsRollingResiduals(rets, w - 1)
    return(zoo(residuals, tail(dates, length(residuals))))
  } else if (is.numeric(rets)) {
    return(cAsRollingResiduals(rets, w - 1))
  } else {
    stop("Please supply levels as a vector or zoo")
  }
}


as_trend_rolling <- function(rets, w = 126) {
  dates <- NULL
  if (is.zoo(rets)) {
    dates <- index(rets)
    rets <- as.numeric(rets)
    trend <- cAsRollingTrend(rets, w - 1)
    return(zoo(trend, tail(dates, length(trend))))
  } else if (is.numeric(rets)) {
    return(cAsRollingTrend(rets, w - 1))
  } else {
    stop("Please supply levels as a vector or zoo")
  }
}


as_residuals_lm <- function(rets) {
  if (is.zoo(rets)) {
    rets.lagged <- lag(rets, -1)
    data <- merge(rets, rets.lagged)
    data <- as.data.frame(na.omit(data))
  } else if (is.numeric(rets)) {
    rets.lagged <- head(rets, length(rets) - 1)
    rets <- tail(rets, length(rets) - 1)
    data <- data.frame(rets, rets.lagged)
  } else {
    stop("Please supply levels as a vector or zoo")
  }

  fit <- lm(rets ~ rets.lagged, data)
  return(zoo(fit$residuals, names(fit$residuals)))
}
