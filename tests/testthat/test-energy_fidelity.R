context("Calculating Fidelity to Data Energy")

test_that("If y and f not matrix or of class gatingfc_grid", {
  y <- matrix(sample(c(-1, 1), size = 16, replace = TRUE), nrow = 4)
  f <- y
  class(f) <- "gatingfc"
  expect_error(energy_fidelity(y = y, f = f),
               "'y' and 'f' must be a matrix, data frame or of class 'gatingfc_grid'")
  class(y) <- "gatingfc_grid"
  expect_error(energy_fidelity(y = y, f = f),
               "'y' and 'f' must be a matrix, data frame or of class 'gatingfc_grid'")
  f <- matrix(sample(c(-1, 1), size = 16, replace = TRUE), nrow = 4)
  y <- f
  class(y) <- "silly_class"
  expect_error(energy_fidelity(y = y, f = f),
               "'y' and 'f' must be a matrix, data frame or of class 'gatingfc_grid'")
  class(f) <- "gatingfc_grid"
  expect_error(energy_fidelity(y = y, f = f),
               "'y' and 'f' must be a matrix, data frame or of class 'gatingfc_grid'")

})

test_that("If f and y not of the same dimension", {
  f <- matrix(sample(c(-1, 1), size = 16, replace = TRUE), nrow = 4)
  y <- f[-1, ]

  expect_error(energy_fidelity(y = y, f = f),
               "'y' and 'f' are not of the same size")
})

test_that("Check that fidelity energy works correctly", {
  y <- matrix(c(-1, 1, -1, -1), nrow = 2)
  f1 <- y * -1
  f2 <- f1
  f2[1] <- f2[1] * -1

  expect_equal(energy_fidelity(y = y, f = f1), 4)
  expect_equal(energy_fidelity(y = y, f = f2), 2)
  class(y) <- "gatingfc_grid"
  expect_equal(energy_fidelity(y = y, f = f1), 4)
  expect_equal(energy_fidelity(y = y, f = f2), 2)
})
