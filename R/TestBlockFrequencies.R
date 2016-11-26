

test_block_frequency <- function(rets, a = 0.99, bs = 128, r = "lm", w = 126) {
  # Check and convert the data.
  .check_data(data = rets)

  # Compute the binarized residuals.
  if (r == "lm") {
    residuals <- as_residuals_lm(rets)
    binrets <- as_binary(residuals)
    binrets <- as.numeric(binrets)
  } else if (r == "sma") {
    residuals <- as_residuals_rolling(rets, w = w)
    binrets <- as_binary(residuals)
    binrets <- as.numeric(binrets)
  } else {
    stop("Unsupported residual; options are lm and sma.")
  }

  # Compute the result of the block frequency test.
  res <- .compute_block_frequency(binrets, bs)
  chi.squared <- res[1]
  p.value <- res[2]
  z.score <- res[3]
  thresh <- abs(qnorm((1 - a) / 2))

  # Return the results object.
  return(RandomnessResult("Block Frequency", "Runs",
                          chi.squared, p.value, z.score,
                          abs(z.score) > thresh))
}


.compute_block_frequency <- function(binrets, bs) {
  # Work out the number of blocks.
  k.blocks <- floor(length(binrets) / bs)
  start <- 1; end <- bs; psum <- 0.0
  for (i in 1:k.blocks) {
    block <- binrets[start:end]
    pi <- sum(block) / bs
    psum <- psum + (pi - 0.5) ^ 2
    start = start + bs; end = end + bs
  }

  # Compute the test statistic.
  chi.squared <- 4.0 * bs * psum

  # Now compute the p value.
  t1 <- k.blocks / 2.0
  t2 <- chi.squared / 2.0
  p.value <- 1.0 - pgamma(t1, t2, lower=FALSE)

  # And return the results.
  z.score <- qnorm(p.value)
  return(c(chi.squared, p.value, z.score))
}
