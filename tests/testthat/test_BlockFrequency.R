context("TESTING THE BLOCK FREQUENCY TEST")


test_that("Block Frequency Test is correst on pi", {
  binrets <- emh:::.read_test_data('pi')
  binrets <- head(binrets, 1000000)
  res <- emh:::.compute_block_frequency(binrets, 128)
  chi.squared <- res[1]; p.value <- res[2]
  testthat::expect_lt(abs(p.value - 0.380615), 0.005)
})


test_that("Block Frequency Test is correst on e", {
  binrets <- emh:::.read_test_data('e')
  binrets <- head(binrets, 1000000)
  res <- emh:::.compute_block_frequency(binrets, 128)
  chi.squared <- res[1]; p.value <- res[2]
  testthat::expect_lt(abs(p.value - 0.211072), 0.005)
})


test_that("Block Frequency Test is correst on sqrt2", {
  binrets <- emh:::.read_test_data('sqrt2')
  binrets <- head(binrets, 1000000)
  res <- emh:::.compute_block_frequency(binrets, 128)
  chi.squared <- res[1]; p.value <- res[2]
  testthat::expect_lt(abs(p.value - 0.833222), 0.005)
})


test_that("Block Frequency Test is correst on sqrt3", {
  binrets <- emh:::.read_test_data('sqrt3')
  binrets <- head(binrets, 1000000)
  res <- emh:::.compute_block_frequency(binrets, 128)
  chi.squared <- res[1]; p.value <- res[2]
  testthat::expect_lt(abs(p.value - 0.473961), 0.005)
})
