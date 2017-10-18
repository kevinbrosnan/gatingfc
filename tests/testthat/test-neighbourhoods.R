context("Test Neighbourhoods functionality")

test_that("Validate Inputs", {

  x <- matrix(0, nrow = 5, ncol = 5)

  expect_error(neighbourhoods(x = x, order = 0, method = "Moore"),
               "The order must be a positive integer.")
  expect_error(neighbourhoods(x = x, order = 2.1, method = "Moore"),
               "The order must be a positive integer.")

  expect_error(neighbourhoods(x = x, order = 1, method = "test"),
               "The available methods are 'von Neumann' or 'Moore'")
  expect_error(neighbourhoods(x = x, order = 1, method = 6),
               "The available methods are 'von Neumann' or 'Moore'")

  expect_error(neighbourhoods(x = x, order = 3, method = "Moore"),
               "The dimension of x in both rows and columns must exceed 2 x order")
  expect_error(neighbourhoods(x = x, order = 3, method = "von Neumann"),
               "The dimension of x in both rows and columns must exceed 2 x order")

})

test_that("Moore Neighbourhoods", {

  x <- matrix(0, nrow = 5, ncol = 5)

  moore_first <- matrix(
    c(NA,	NA,	NA,	NA,	 2, NA,  6,	7,
      NA,	NA,	NA,	 1,	 3,	 6,  7,  8,
      NA,	NA,	NA,	 2,	 4,	 7,  8,	9,
      NA,	NA,	NA,	 3,	 5,	 8,  9, 10,
      NA,	NA,	NA,	 4,	NA,	 9,	10,	NA,
      NA,	 1,	 2,	NA,	 7,	NA,	11,	12,
      1,  2,	 3,	 6,	 8,	11,	12,	13,
      2,	 3,	 4,	 7,	 9,	12,	13,	14,
      3,	 4,	 5,	 8,	10,	13,	14,	15,
      4,	 5,	NA,	 9,	NA,	14,	15,	NA,
      NA,	 6,	 7,	NA,	12,	NA,	16,	17,
      6,	 7,	 8,	11,	13,	16,	17,	18,
      7,	 8,	 9,	12,	14,	17,	18,	19,
      8,	 9,	10,	13,	15,	18,	19,	20,
      9,	10,	NA,	14,	NA,	19,	20,	NA,
      NA,	11,	12,	NA,	17,	NA,	21,	22,
      11,	12,	13,	16,	18,	21,	22,	23,
      12,	13,	14,	17,	19,	22,	23,	24,
      13,	14,	15,	18,	20,	23,	24,	25,
      14,	15,	NA,	19,	NA,	24,	25,	NA,
      NA,	16,	17,	NA,	22,	NA,	NA,	NA,
      16,	17,	18,	21,	23,	NA,	NA,	NA,
      17,	18,	19,	22,	24,	NA,	NA,	NA,
      18,	19,	20,	23,	25,	NA,	NA,	NA,
      19,	20,	NA,	24,	NA,	NA,	NA,	NA),
    nrow = 25, ncol = 8, byrow = TRUE
  )

  moore_second <- matrix(
    c(NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA, 2,	3,NA,NA, 6,	7, 8,NA,NA,11,12,13,
      NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,	1, 3,	4,NA,	6, 7,	8, 9,NA,11,12,13,14,
      NA,NA,NA,NA,NA,NA,NA,NA,NA,NA, 1,	2, 4,	5, 6,	7, 8,	9,10,11,12,13,14,15,
      NA,NA,NA,NA,NA,NA,NA,NA,NA,NA, 2,	3, 5,NA, 7,	8, 9,10,NA,12,13,14,15,NA,
      NA,NA,NA,NA,NA,NA,NA,NA,NA,NA, 3,	4,NA,NA, 8,	9,10,NA,NA,13,14,15,NA,NA,
      NA,NA,NA,NA,NA,NA,NA,	1, 2,	3,NA,NA, 7, 8,NA,NA,11,12,13,NA,NA,16,17,18,
      NA,NA,NA,NA,NA,NA, 1,	2, 3,	4,NA,	6, 8, 9,NA,11,12,13,14,NA,16,17,18,19,
      NA,NA,NA,NA,NA,	1, 2,	3, 4,	5, 6,	7, 9,10,11,12,13,14,15,16,17,18,19,20,
      NA,NA,NA,NA,NA,	2, 3,	4, 5,NA, 7,	8,10,NA,12,13,14,15,NA,17,18,19,20,NA,
      NA,NA,NA,NA,NA,	3, 4,	5,NA,NA, 8, 9,NA,NA,13,14,15,NA,NA,18,19,20,NA,NA,
      NA,NA, 1, 2, 3,NA,NA,	6, 7,	8,NA,NA,12,13,NA,NA,16,17,18,NA,NA,21,22,23,
      NA,	1, 2, 3, 4,NA, 6,	7, 8,	9,NA,11,13,14,NA,16,17,18,19,NA,21,22,23,24,
       1,	2, 3, 4, 5,	6, 7,	8, 9,10,11,12,14,15,16,17,18,19,20,21,22,23,24,25,
       2,	3, 4, 5,NA,	7, 8,	9,10,NA,12,13,15,NA,17,18,19,20,NA,22,23,24,25,NA,
       3,	4, 5,NA,NA,	8, 9,10,NA,NA,13,14,NA,NA,18,19,20,NA,NA,23,24,25,NA,NA,
      NA,NA, 6, 7, 8,NA,NA,11,12,13,NA,NA,17,18,NA,NA,21,22,23,NA,NA,NA,NA,NA,
      NA,	6, 7, 8, 9,NA,11,12,13,14,NA,16,18,19,NA,21,22,23,24,NA,NA,NA,NA,NA,
       6,	7, 8, 9,10,11,12,13,14,15,16,17,19,20,21,22,23,24,25,NA,NA,NA,NA,NA,
       7,	8, 9,10,NA,12,13,14,15,NA,17,18,20,NA,22,23,24,25,NA,NA,NA,NA,NA,NA,
       8, 9,10,NA,NA,13,14,15,NA,NA,18,19,NA,NA,23,24,25,NA,NA,NA,NA,NA,NA,NA,
      NA,NA,11,12,13,NA,NA,16,17,18,NA,NA,22,23,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
      NA,11,12,13,14,NA,16,17,18,19,NA,21,23,24,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
      11,12,13,14,15,16,17,18,19,20,21,22,24,25,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
      12,13,14,15,NA,17,18,19,20,NA,22,23,25,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
      13,14,15,NA,NA,18,19,20,NA,NA,23,24,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA),
    nrow = 25, ncol = 24, byrow = TRUE
  )

  expect_equal(neighbourhoods(x, order = 1, method = "Moore"),
               moore_first)

  expect_equal(neighbourhoods(x, order = 2, method = "Moore"),
               moore_second)
})

test_that("von Neumann Neighbourhoods", {

  x <- matrix(0, nrow = 5, ncol = 5)

  neumann_first <- matrix(
    c(NA,NA, 2,	6,
      NA, 1, 3,	7,
      NA, 2, 4,	8,
      NA, 3, 5,	9,
      NA, 4,NA,10,
       1,NA, 7,11,
       2, 6, 8,12,
       3, 7, 9,13,
       4, 8,10,14,
       5, 9,NA,15,
       6,NA,12,16,
       7,11,13,17,
       8,12,14,18,
       9,13,15,19,
      10,14,NA,20,
      11,NA,17,21,
      12,16,18,22,
      13,17,19,23,
      14,18,20,24,
      15,19,NA,25,
      16,NA,22,NA,
      17,21,23,NA,
      18,22,24,NA,
      19,23,25,NA,
      20,24,NA,NA),
    nrow = 25, ncol = 4, byrow = TRUE
  )

  neumann_second <- matrix(
    c(NA,NA,NA,NA,NA,NA, 2,	3,NA,	6, 7,11,
      NA,NA,NA,NA,NA,	1, 3,	4, 6,	7, 8,12,
      NA,NA,NA,NA, 1,	2, 4,	5, 7,	8, 9,13,
      NA,NA,NA,NA, 2,	3, 5,NA, 8,	9,10,14,
      NA,NA,NA,NA, 3,	4,NA,NA, 9,10,NA,15,
      NA,NA, 1, 2,NA,NA, 7,	8,NA,11,12,16,
      NA, 1, 2, 3,NA, 6, 8, 9,11,12,13,17,
      NA, 2, 3, 4, 6,	7, 9,10,12,13,14,18,
      NA, 3, 4, 5, 7, 8,10,NA,13,14,15,19,
      NA, 4, 5,NA, 8,	9,NA,NA,14,15,NA,20,
       1,NA, 6, 7,NA,NA,12,13,NA,16,17,21,
       2, 6, 7, 8,NA,11,13,14,16,17,18,22,
       3, 7, 8, 9,11,12,14,15,17,18,19,23,
       4, 8, 9,10,12,13,15,NA,18,19,20,24,
       5, 9,10,NA,13,14,NA,NA,19,20,NA,25,
       6,NA,11,12,NA,NA,17,18,NA,21,22,NA,
       7,11,12,13,NA,16,18,19,21,22,23,NA,
       8,12,13,14,16,17,19,20,22,23,24,NA,
       9,13,14,15,17,18,20,NA,23,24,25,NA,
      10,14,15,NA,18,19,NA,NA,24,25,NA,NA,
      11,NA,16,17,NA,NA,22,23,NA,NA,NA,NA,
      12,16,17,18,NA,21,23,24,NA,NA,NA,NA,
      13,17,18,19,21,22,24,25,NA,NA,NA,NA,
      14,18,19,20,22,23,25,NA,NA,NA,NA,NA,
      15,19,20,NA,23,24,NA,NA,NA,NA,NA,NA),
    nrow = 25, ncol = 12, byrow = TRUE
  )

  expect_equal(neighbourhoods(x, order = 1, method = "von Neumann"),
               neumann_first)

  expect_equal(neighbourhoods(x, order = 2, method = "von Neumann"),
               neumann_second)

})
