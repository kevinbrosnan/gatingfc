context("Gating Tests")

test_that("Verify inputs", {
  x <- rep(1, times = 16)
  y <- rep(0, times = 16)

  mat.long <- cbind(x, y)
  mat.wide <- rbind(x, y)
  vec <- c(x, y)
  df.long <- as.data.frame(mat.long)
  df.wide <- as.data.frame(mat.wide)
  df.vec <- as.data.frame(vec)

  expect_error(gating(x = mat.long),
               'Input object must be a data frame of dimension N times p')
  expect_error(gating(x = mat.wide),
               'Input object must be a data frame of dimension N times p')
  expect_error(gating(x = vec),
               'Input object must be a data frame of dimension N times p')

  expect_warning(gating(x = df.wide),
                 paste('Input object has greater than 2 columns of data and',
                       'var.names is not specified, using the first 2 columns',
                       'of input object. Please specify var.names for other',
                       'pairs of variables.', sep = "\n  "))

  expect_error(gating(x = df.vec),
               'Input object must be a data frame with at least 2 columns.')

  expect_error(gating(x = df.long, var.names = c(1, 2, 3)),
               'var.names must have a maximum length of 2')

})
