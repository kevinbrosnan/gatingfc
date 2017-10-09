#' Global energy of lattice grid
#'
#' \code{energy_global} calculates the complete global energy for the lattice
#' grid. The tendency of the overall grid position is provided through
#' \code{trend}.
#'
#' @param x the lattice grid for which the global energy is required
#' @param trend the global tendancy of the lattice grid
#'
#' @return the value of the global energy function
#'
#' @examples
#'   x <- matrix(sample(c(-1, 1), times = 16, prob = c(0.6, 0.4)), nrow = 4)
#'   energy_global(x)
#'
#' @author Kevin Brosnan (\email(kevin.c.brosnan@@ul.ie))
#'
#' @export

energy_global <- function(x, trend = -1) {

  if (trend != 1 && trend != -1) {
    stop("\"trend\" must be either +1 or -1")
  }

  if (!is.matrix(x) && !is.data.frame(x) && class(x) != "gatingfc_grid") {
    stop("\"x\" must be a matrix, data frame or of class \"gatingfc_grid\"")
  }

  y <- as.matrix(x)

  global <- -sum(y)
  global <- sign(trend) * global
  return(global)
}
