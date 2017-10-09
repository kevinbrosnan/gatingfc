context("Calculating Global Energy")

test_that("If trend in not equal to +1 or -1 then report error", {
  x <- matrix(c(1, 1, 1, 1, -1, -1, -1, -1, 1), nrow = 3)
  expect_error(energy_global(x, trend = 0),
               "'trend' must be either 1 or -1")
  expect_error(energy_global(x, trend = 0.8),
               "'trend' must be either 1 or -1")
  expect_error(energy_global(x, trend = -6.5),
               "'trend' must be either 1 or -1")
})

test_that("If x is not a matrix or of class gatingfc_grid", {
  x <- matrix(sample(c(-1, 1), size = 16, replace = TRUE), nrow = 4)
  y <- x
  x <- data.frame(x)
  class(y) <- "gatingfc"
  expect_error(energy_global(x = x, trend = -1),
               "'x' must be a matrix or of class 'gatingfc_grid'")
  expect_error(energy_global(x = y, trend = 1),
               "'x' must be a matrix or of class 'gatingfc_grid'")
})

test_that("Check that global energy works correctly", {
  x <- matrix(c(-1, 1, -1, -1), nrow = 2)
  y <- x
  class(y) <- "gatingfc_grid"
  expect_equal(energy_global(x, trend = -1), -2)
  expect_equal(energy_global(x, trend = +1), +2)
  expect_equal(energy_global(y, trend = -1), -2)
  expect_equal(energy_global(y, trend = +1), +2)
})
