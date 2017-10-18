#' Calculate the neighbourhood energy
#'
#' This functions provides a method to calculate the neighbourhood energy
#' on the complete lattice grid, or at a single point within the provided
#' lattice grid.
#'
#' @param x the lattice grid for which the energy is required
#' @param position the linear index of the node for which energy is required
#' @param neighbourhoods the neighbourhoods associated with the lattice grid
#'
#' @return the neighbourhood energy for the lattice or the specific node
#'
#' @author Kevin Brosnan (\email{kevin.c.brosnan@@gmail.com})
#'
#' @examples
#'  y <- matrix(sample(c(-1, 1), size = 16, prob = c(0.6, 0.4), replace = TRUE), nrow = 4)
#'  neigh <- neighbourhoods(x = y, order = 1, method = "Moore")
#'  energy <- energy_neighbourhood(x = y, position = -1, neighbourhoods = neigh)
#'
#' @export

energy_neighbourhood <- function(x, position, neighbourhoods) {

  if (!is.matrix(neighbourhoods)) {
    stop("neighbourhoods needs to be a matrix of neighbourhood connections")
  }

  if (!is.matrix(x)) {
    stop("x must be a matrix")
  }

  if (position > length(x)) {
    stop("position must be within the length of x")
  }

  if (floor(position) != position) {
    stop("position must be an integer")
  }

  # Calculate the full energy of the lattice
  if (position <= 0) {
    energy <- 0
    for (i in 1:length(x)) {
      energy <- energy - x[i] * (sum(x[neighbourhoods[i, ]], na.rm = TRUE))
    }

  # Calculate the energy for the point defined by position
  } else {
    energy <- -x[position] * (sum(x[neighbourhoods[position, ]], na.rm = TRUE))
  }

  return(energy)
}
