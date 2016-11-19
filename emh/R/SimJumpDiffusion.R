#' @title A Merton Jump Diffusion Model
#' @description Simulates a logarithmic returns series from a Merton jump diffusion model with drift.
#'
#' @param n numeric :: The amount of time to simulate.
#' @param dt numeric :: Delta t, change in time.
#' @param vol numeric :: Annualized volatility.
#' @param drift numeric :: Average daily log return.
#' @param jlambda numeric :: The probability of a jump happening.
#' @param jsigma numeric :: The volatility of jumps which occur.
#' @param jmu numeric :: The average jump size which occurs.
#' @return A numeric vector containing simulated log returns.
#'
#' @details This function simulates a logarithmic return series from a Merton Jump Diffusion model. The Merton Jump
#' Diffusion model is a popular stochastic process used for simulating time series with fat tails. Which side of the
#' tail is fat is controlled by the parameters jlambda, jmu, and jsigma.
#'
simulate_merton_model <- function(n = 252, dt = 0.003968254, vol = 0.25, drift = 0.0,
                                  jlambda = 0.0156, jsigma = 0.01, jmu = -0.0075) {
  jump_diffusion = .simulate_jump_diffusion(n, dt, vol, drift, jlambda, jsigma, jmu)
  geometric_brownian_motion = simulate_brownian_motion(n, dt, vol, drift)
  return(jump_diffusion + geometric_brownian_motion)
}


.simulate_jump_diffusion <- function(n = 252, dt = 0.003968254, vol = 0.25, drift = 0.0,
                                    jlambda = 0.0156, jsigma = 0.01, jmu = -0.0075) {
  s_n = 0; time = 0
  small_lamda = -(1.0 / jlambda)

  jump_sizes = c()
  for (t in 1:n)
    jump_sizes = c(jump_sizes, 0.0)

  while (s_n < n) {
    s_n = s_n + small_lamda * log(runif(1))
    for (j in 0:n) {
      if (time * dt <= s_n * drift) {
        if (s_n * drift <= (j + 1) * dt) {
          jump_sizes[j] = jump_sizes[j] + rnorm(1, jmu, jsigma)
        }
      }
      time = time + 1
    }
  }

  return(jump_sizes)
}
