#' Generates a grid of count data
#'
#' \code{make_grid} generates a lattice grid for the bivariate data inputted
#' by \code{x}.
#'
#' @details
#'  This function produces a grid of bivariate count data observed in flow
#'  cytometry analysis. This data lies on a lattice support grid where cells
#'  show the bivariate counts.
#'
#' @param x a two column matrix of flow cytometry varaibles or a vector
#' representing on cytometry variable.
#'
#' @examples
#'  x <- matrix(sample(c(0:7), size = 10, replace = TRUE), ncol = 2)
#'  y <- make_grid(x)
#'
#' @return A square count matrix representing the observed data. Values greater
#' the 0 represent activity of cells, while 0 records an inactive cell location.
#'
#' @author Kevin Brosnan \email{kevin.c.brosnan@@gmail.com}
#'
#' @export

make_grid <- function(x) {

  if (is.null(dim(x)) || dim(x)[2] < 2) {
    stop("'x' needs to be a matrix of at least two columns")
  }
  if (dim(x)[2] > 2) {
    warning("'x' has more than 2 dimensions. Using the first and second columns")
  }

  y <- x[,2]
  x <- x[,1]

  # Assume recordings are in channels 0 -> 2^a - 1
  min.value <- 0
  max.value <- 2 ^ ceiling(log2(max(max(x), max(y)) + 1)) - 1

  unique.recorded <- max.value - min.value + 1
  mat.grid <- matrix(0, nrow = unique.recorded, ncol = unique.recorded)

  index <- (x + 1) + y * unique.recorded
  index <- table(index)

  mat.grid[as.numeric(names(index))] <- index
  class(mat.grid) <- "gatingfc_grid"
  return(mat.grid)
}
