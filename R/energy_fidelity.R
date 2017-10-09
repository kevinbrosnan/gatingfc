#' Calculate the energy function for fidelity to underlying data
#'
#' The \code{energy_fidelity} function calculates the energy associated with
#' the difference between the underlying observed data and the current
#' configuration of the model.
#'
#' @param y the underlying observed lattice grid
#' @param f the current configuration of the model for which to compare
#'
#' @return A measure of the discordance between the observed data and the model
#'
#' @author Kevin Brosnan (\email{kevin.c.brosnan@@gmail.com})
#'
#' @examples
#'   y <- matrix(sample(c(-1, 1), size = 16, prob = c(0.6, 0.4), replace = TRUE), nrow = 4)
#'   f <- matrix(sample(c(-1, 1), size = 16, prob = c(0.65, 0.35), replace = TRUE), nrow = 4)
#'   energy_fidelity(y = y, f = f)
#'
#' @export

energy_fidelity <- function(y, f) {

  if (class(y) != "gatingfc_grid" && class(y) != "matrix") {
    stop("'y' and 'f' must be a matrix, data frame or of class 'gatingfc_grid', y")
  }

  if (class(f) != "matrix" && class(f) != "gatingfc_grid") {
    stop("'y' and 'f' must be a matrix, data frame or of class 'gatingfc_grid'")
  }

  if (nrow(y) != nrow(f) || ncol(y) != ncol(f)) {
    stop("'y' and 'f' are not of the same size")
  }

  energy <- -sum(y * f)
  return(energy)
}
