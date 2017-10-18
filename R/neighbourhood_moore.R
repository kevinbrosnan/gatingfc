#' Moore Neighbourhood Algorithm
#'
#' This functions computes the complete neighbourhood system of the qth order
#' for a lattice grid under the Moore algorithm
#'
#' @param x the lattice grid for which a neighbourhood structure is required
#' @param order the order of the neighbourhood system
#'
#' @return a matrix, where each row corresponds to an observations neighbours
#'
#' @author Kevin Brosnan (\email{kevin.c.brosnan@@gmail.com})
#'
#' @examples
#'  y <- matrix(sample(c(-1, 1), size = 16, prob = c(0.6, 0.4), replace = TRUE), nrow = 4)
#'  neigh <- neighbourhood_moore(x = y, order = 1)
#'
#' @export

neighbourhood_moore <- function(x, order) {

  no.row <- nrow(x)
  no.col <- ncol(x)

  max.neighbours <- (2 * order + 1) ^ 2 - 1
  neighbours <- matrix(1:(no.row * no.col), nrow = no.col * no.row,
                       ncol = max.neighbours + 1)

  neigh.distance <- vector(length = 0)
  for (i in -order:order) {
    neigh.distance <- c(neigh.distance,
                        seq(from = i * no.row - order,
                            by = 1, length.out = 2 * order + 1))
  }

  neighbours <- neighbours + matrix(c(neigh.distance), nrow = no.col * no.row,
                                    ncol = max.neighbours + 1, byrow = TRUE)

  # Top Left Corner Issues
  top.left.corner <- vector(length = 0)
  for (j in 1:order) {
    top.left.corner <- c(top.left.corner,
                         seq(from = (order + j) * (2 * order + 1) + 1, by = 1,
                             length.out = order))
  }
  neighbours[1, top.left.corner] <- NA

  # Bottom Left Corner Issues
  bottom.left.corner <- vector(length = 0)
  for (j in 1:order) {
    bottom.left.corner <- c(bottom.left.corner,
                            seq(from = 2 * (order ^ 2) + 1 + (j - 1),
                                by = (2 * order + 1), length.out = (order + 2)))
  }
  neighbours[no.row, bottom.left.corner] <- NA

  # Top Right Corner Issues
  top.right.corner <- vector(length = 0)
  for (j in 1:order) {
    top.right.corner <- c(top.right.corner, seq(from = j, by = 2 * order + 1,
                                                length.out = 2 * order + 1))
  }
  neighbours[(no.row ^ 2 - no.row + 1), top.right.corner] <- NA

  # Bottom Right Corner Issues
  bottom.right.corner <- vector(length = 0)
  for (j in 1:order) {
    bottom.right.corner <- c(bottom.right.corner,
                             seq(from = order + j + 1,
                                 by = 2 * order + 1, length.out = order))
  }
  neighbours[no.row ^ 2, bottom.right.corner] <- NA

  # Left Edge Issues
  left.edge <- seq(from = 1, by = 1, length.out = order * (2 * order + 1))
  neighbours[2:(no.row - 1), left.edge] <- NA

  neighbours[order, seq(from = 1, by = (2 * order + 1),
                        length.out = (2 * order + 1))] <- NA
  neighbours[(no.row - order + 1), seq(from = (2 * order + 1),
                                       by = (2 * order + 1),
                                       length.out = (2 * order + 1))] <- NA

  # Right Edge Issues
  neighbours[(no.row ^ 2 - no.row + order),
             seq(from = 1, by = (2 * order + 1),
                 length.out = (2 * order + 1))] <- NA
  neighbours[(no.row ^ 2 - order + 1),
             seq(from = (2 * order + 1), by = (2 * order + 1),
                 length.out = (2 * order + 1))] <- NA

  # Top Edge Issues
  top.edge <- vector(length = 0)
  for (j in 1:order) {
    top.edge <- c(top.edge, seq(from = j, by = (2 * order + 1),
                                length.out = (2 * order + 1)))
  }
  neighbours[seq(from = no.row + 1, by = no.row,
                 length.out = no.col - 2), top.edge] <- NA

  # Bottom Edge Issues
  bottom.edge <- vector(length = 0)
  for (j in 1:order) {
    bottom.edge <- c(bottom.edge,
                     seq(from = order + j + 1, by = (2 * order + 1),
                         length.out = (2 * order + 1)))
  }
  neighbours[seq(from = 2 * no.row, by = no.row,
                 length.out = no.col - 2), bottom.edge] <- NA

  # Order Row Issues
  for (i in seq(from = order + no.row, by = no.row, length.out = no.col - 2)) {
    neighbours[i, which(neighbours[i, ] %% no.row == 0)] <- NA
  }

  for (i in seq(from = 2 * no.row - (order - 1), by = no.row,
                length.out = no.col - 2)) {
    neighbours[i, which(neighbours[i, ] %% no.row == 1)] <- NA
  }

  return(neighbours)
}
