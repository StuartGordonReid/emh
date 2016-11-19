#' @title Converts log returns to prices
#' @description This function converts log returns to a price series.
#'
#' @param logrets zoo or numeric :: A sequence of log returns for some time series.
#' @return A zoo object or numeric vector containing price levels.
#'
as_levels <- function(logrets) {
  if (is.zoo(logrets)) {
    dates <- index(logrets)
    levels <- cAsLevels(as.numeric(logrets))
    return(zoo(levels, dates))
  } else if (!is.numeric(logrets)) {
    stop("Please supply log returns as a vector or zoo.")
  }
  return(cAsLevels(logrets))
}


#' @title Converts prices to log returns.
#' @description This function converts prices to log returns of order, order.
#'
#' @param levels zoo or numeric :: A sequence of prices for some time series
#' @param order numeric :: The order of the returns to compute. When 1 this is the same as daily returns.
#' @return A zoo object or numeric vector containing log returns of order, order.
#'
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


#' @title Converts prices to nominal returns.
#' @description This function converts price levels to nominal returns of order, order.
#'
#' @param levels zoo or numeric :: A sequence of prices for some time series.
#' @param order numeric :: The order of the returns to compute.
#' @return A zoo object or numeric vector containing returns of order, order.
#'
as_returns <- function(levels, order = 1) {
  return(exp(as_logreturns(levels, order)) - 1)
}


#' @title Converts returns into binary numbers.
#' @description Positive returns are represented as a 1, negative returns are represented as a 0.
#'
#' @param rets zoo or numeric :: A sequence of returns (logarithmic or nominal)
#' @return A zoo object or numeric vector containing the binarized returns.
#'
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


#' @title Converts a zoo object from a daily frequency to a lower frequency.
#' @description This function converts a zoo object containing daily logarithmic returns into a specified frequency
#' which is lower than daily e.g. weekly returns or five-daily returns.
#'
#' @param logrets.zoo zoo :: A zoo object of returns at daily frequency.
#' @param frequency numeric or string :: A value indicating the desired new frequency. Options are an integer value
#' greater than or equal to 1 or a string set to "Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun", "Week", or "Month".
#' @return A zoo object containing the returns converted to the specified frequency.
#'
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


#' @title Interpolates missing days in the sequence with zero returns.
#' @description This is required to accurately measure the returns on frequencies defined by day-of-the-week, week, or
#' month frequencies. This ensures that, for example, Monday to Monday returns are only ever one week long.
#'
#' @param logrets.zoo zoo :: A zoo object of returns at daily frequency.
#' @return A zoo object containing the interpolated time series.
#'
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


#' @tile Converts returns to residual returns above the mean.
#' @description This function extracts the mean of the logarithmic returns from the log returns and then exponentiates
#' back into the nominal return space. In other words, it extracts the continuously compounded mean return.
#'
#' @param rets zoo or numeric :: nominal returns of the asset.
#' @return A zoo object or numeric vector containing nominal residual returns.
#'
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


#' @title Converts returns to residual returns above a rolling simple mean.
#' @description This function extracts a rolling simple moving average from the returns.
#'
#' @param rets zoo or numeric :: nominal returns of the asset
#' @param w numeric :: the length of the simple-moving-average to compute.
#' @return A zoo object or numeric vector containing nominal residual returns.
#'
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


#' @title Returns the w-day simple moving average of the returns.
#' @description This returns the w-day simple moving average return of the returns.
#'
#' @param rets zoo or numeric vector :: nominal returns of the asset
#' @param w numeric :: the length of the simple-moving-average to compute
#' @return A zoo object or numeric vector containing nominal rolling residual returns.
#'
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


#' @title Returns the residual of the returns and a linear model fitted to the returns lagged by one time period.
#' @description This function returns the residual of the returns and a linear model fitted to the returns lagged by
#' one time period. It is a common procedure used in many statistical tests of randomness.
#'
#' @param rets zoo or numeric vector :: nominal returns of the asset
#' @returns A zoo object or numeric vector containing the residuals from a regression.
#'
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
