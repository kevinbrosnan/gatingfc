context("Testing Neighbourhood Energy")

test_that("Check input validity", {

  x <- matrix(0, nrow = 5, ncol = 5)
  n <- matrix(0, nrow = 25, ncol = 4)

  expect_error(energy_neighbourhood(x = x, position = 1, neighbourhoods = 2),
               "neighbourhoods needs to be a matrix of neighbourhood connections")
  expect_error(energy_neighbourhood(x = x, position = 1, neighbourhoods = "abc"),
               "neighbourhoods needs to be a matrix of neighbourhood connections")

  expect_error(energy_neighbourhood(x = 2, position = 1, neighbourhoods = n),
               "x must be a matrix")
  expect_error(energy_neighbourhood(x = "abc", position = 1, neighbourhoods = n),
               "x must be a matrix")

  expect_error(energy_neighbourhood(x = x, position = 26, neighbourhoods = n),
               "position must be within the length of x")
  expect_error(energy_neighbourhood(x = x, position = 2.1, neighbourhoods = n),
               "position must be an integer")
})

test_that("Neighbourhood Energy Complete", {
  x <- matrix(
    c(-1, -1, -1, -1,
      1, -1, -1, -1,
      -1,  1,  1, -1,
      1,  1,  1, -1),
    nrow = 4, ncol = 4, byrow = TRUE
  )

  neighbours <- neighbourhoods(x = x, order = 1, method = "von Neumann")
  expect_equal(energy_neighbourhood(x = x, position = -1,
                                    neighbourhoods = neighbours),
               -12)

  neighbours <- neighbourhoods(x = x, order = 1, method = "Moore")
  expect_equal(energy_neighbourhood(x = x, position = -1,
                                    neighbourhoods = neighbours),
               -20)
})

test_that("Neighbourhood Energy Position", {
  x <- matrix(
    c(-1, -1, -1, -1,
       1, -1, -1, -1,
      -1,  1,  1, -1,
       1,  1,  1, -1),
    nrow = 4, ncol = 4, byrow = TRUE
  )
  neighbours <- neighbourhoods(x = x, order = 1, method = "von Neumann")

  expect_equal(energy_neighbourhood(x = x, position = 4,
                                    neighbourhoods = neighbours),
               0)
  expect_equal(energy_neighbourhood(x = x, position = 5,
                                    neighbourhoods = neighbours),
               -3)
  expect_equal(energy_neighbourhood(x = x, position = 6,
                                    neighbourhoods = neighbours),
               0)
  expect_equal(energy_neighbourhood(x = x, position = 11,
                                    neighbourhoods = neighbours),
               0)
  expect_equal(energy_neighbourhood(x = x, position = 15,
                                    neighbourhoods = neighbours),
               -1)
})
