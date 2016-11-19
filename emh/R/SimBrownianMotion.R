#' @title Brownian Motion with or without Drift.
#' @description Simulates a logarithmic return series samples from a Brownian Motion model without without drift.
#'
#' @param n numeric :: The amount of time to simulate.
#' @param dt numeric :: Delta t, change in time.
#' @param vol numeric :: Annualized volatility.
#' @param drift numeric :: Average daily log return.
#' @return A numeric vector containing simulated log returns.
#'
#' @details This function simulates a logarithmic return series from a Brownian Motion model with or without drift.
#' Brownian Motion was first proposed as a model of security prices by Louis Bachelier in his 1900 PhD thesis entitled
#' "The Theory of Speculation". Geometric Brownian Motion, Brownian Motion with drift, is commonly used to price
#' financial securities. It is the model of security returns assumed by the Black Scholes options pricing model.
#'
#' \itemize{
#'  \item{"Theorie de la speculation."}{Bachelier, Louis. Gauthier-Villars, 1900.}
#' }
#'
simulate_brownianmotion <- function(n = 252, dt = 0.003968254, vol = 0.25, drift = 0.0) {
  # Simulate a Wiener process.
  wiener <- rnorm(n, 0, sqrt(dt) * vol)

  # Add the drift component.
  if (drift != 0)
    wiener <- wiener + (drift - 0.5 * (vol ^ 2)) * dt

  # Return the stochastic process.
  return(wiener)
}
