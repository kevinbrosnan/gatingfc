context("Testing make_grid functionality")

test_that("If only one column of data input then report error", {
  x <- c(1:20)
  expect_error(make_grid(x = x), "\"x\" needs to be a matrix of at least two columns")
})

test_that("If more than two columns of data input then report warning", {
  x <- matrix(1, nrow = 30, ncol = 3)
  expect_warning(make_grid(x = x, min = 0, max = 255),
                 "\"x\" has more than 2 dimensions. Using the first and second columns")
})

test_that("Working cases with matrix and two vector inputs", {
  x <- matrix(c(0, 0,
                1, 2,
                3, 7,
                6, 3,
                4, 4),
              ncol = 2, byrow = TRUE)
  z <- matrix(c(1, 0, 0, 0, 0, 0, 0, 0,
                0, 0, 1, 0, 0, 0, 0, 0,
                0, 0, 0, 0, 0, 0, 0, 0,
                0, 0, 0, 0, 0, 0, 0, 1,
                0, 0, 0, 0, 1, 0, 0, 0,
                0, 0, 0, 0, 0, 0, 0, 0,
                0, 0, 0, 1, 0, 0, 0, 0,
                0, 0, 0, 0, 0, 0, 0, 0),
              nrow = 8, ncol = 8, byrow = TRUE)
  expect_equal(make_grid(x = x, min = 0, max = 7), z)
})
