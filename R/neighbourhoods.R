#' Neighbourhood of complete lattice grid
#'
#' This function provides a single wrapper to \code{neighbourhood_moore} and
#' \code{neighbourhood_neumann}. It is advised that this function be called to
#' find the neighbourhoods in this package, as it provides the most error checks
#'
#' @param x the lattice grid for which a neighbourhood structure is required
#' @param order the order of the neighbourhood system
#' @param method the neighbourhood method, 'Moore' or 'von Neumann'
#'
#' @return a matrix, where each row corresponds to an observations neighbours
#'
#' @author Kevin Brosnan (\email{kevin.c.brosnan@@gmail.com})
#'
#' @examples
#'  y <- matrix(sample(c(-1, 1), size = 16, prob = c(0.6, 0.4), replace = TRUE), nrow = 4)
#'  neigh <- neighbourhoods(x = y, order = 1, method = "Moore")
#'
#' @export

neighbourhoods <- function(x, order = 1, method = "Moore") {

  if (floor(order) != order || order < 1) {
    stop("The order must be a positive integer.")
  }

  method <- tolower(method)

  if (method != "von neumann" && method != "moore") {
    stop("The available methods are 'von Neumann' or 'Moore'")
  }

  no.row <- nrow(x)
  no.col <- ncol(x)

  if (no.row < (2 * order + 1) || no.col < (2 * order + 1)) {
    stop("The dimension of x in both rows and columns must exceed 2 x order")
  }

  if (method == "von neumann") {
    neighbours <- neighbourhood_neumann(x = x, order = order)
  } else {
    neighbours <- neighbourhood_moore(x = x, order = order)
  }

  neighbours <- neighbours[, -ceiling(ncol(neighbours) / 2)]
  neighbours[which(neighbours <= 0)] <- NA
  neighbours[which(neighbours > no.row * no.col)] <- NA
  return(neighbours)
}
