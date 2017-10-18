#' von Neumann Neighbourhood Algorithm
#'
#' This functions computes the complete neighbourhood system of the qth order
#' for a lattice grid under the von Neumann algorithm
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
#'  neigh <- neighbourhood_neumann(x = y, order = 1)
#'
#' @export

neighbourhood_neumann <- function(x, order) {

  no.row <- nrow(x)
  no.col <- ncol(x)

  max.neighbours <- 2 * order * (order + 1)
  neighbours <- matrix(1:(no.row * no.col), nrow = no.col * no.row,
                       ncol = max.neighbours + 1)
  p <- -order:order
  p.hat <- c(0:order, (order - 1):0)
  neigh.distance <- vector(length = 0)
  for (i in 1:length(p)) {
    neigh.distance <- c(neigh.distance,
                        seq(from = (p[i] * no.row - p.hat[i]),
                            to = (p[i] * no.row + p.hat[i]),
                            by = 1))
  }

  neighbours <- neighbours + matrix(c(neigh.distance), nrow = no.col * no.row,
                                    ncol = max.neighbours + 1, byrow = TRUE)
  # Top Left Corner Issues
  top.left.corner <- seq(from = 1, to = order * (order + 1), by = 1)
  p <- c(0, seq(from = 2 * order - 1, to = 1, by = -2))
  p <- cumsum(p)

  if (order >= 2) {
    for (j in 2:order) {
      top.left.corner <- c(top.left.corner,
                           seq(from = (order * (order + 2) + 2 + p[j - 1]),
                               by = 1, length.out = order - j + 1))
    }
  }
  neighbours[1, top.left.corner] <- NA

  # Bottom Left Corner Issues
  bottom.left.corner <- seq(from = 1, to = order ^ 2, by = 1)
  p <- c(0, seq(from = 2 * order, to = 1, by = -2))
  p <- cumsum(p)
  for (j in 1:order) {
    bottom.left.corner <- c(bottom.left.corner,
                            seq(from = (order * (order + 1) + 2 + p[j]),
                                by = 1, length.out = order - j + 1))
  }
  neighbours[no.row, bottom.left.corner] <- NA

  # Top Right Corner Issues
  top.right.corner <- seq(from = (order * (order + 2) + 2), by = 1,
                          to = (2 * order * (order + 1) + 1))
  p <- c(0, seq(from = 3, by = 2, length.out = order - 1))
  p <- cumsum(p)

  for (j in 1:order) {
    top.right.corner <- c(top.right.corner,
                          seq(from = 2 + p[j], by = 1, length.out = j))
  }
  neighbours[(no.row ^ 2 - no.row + 1), top.right.corner] <- NA

  # Bottom Right Corner Issues
  bottom.right.corner <- seq(from = (order * (order + 1) + 2), by = 1,
                             to = (2 * order * (order + 1) + 1))
  if (order >= 2) {
    p <- c(0, seq(from = 4, by = 2, length.out = order - 2))
    p <- cumsum(p)

    for (j in 2:order) {
      bottom.right.corner <- c(bottom.right.corner,
                               seq(from = 4 + p[j - 1], by = 1,
                                   length.out = j - 1))
    }
  }
  neighbours[(no.row ^ 2), bottom.right.corner] <- NA

  # Left Edge Issues
  left.edge <- seq(from = 1, to = order ^ 2, by = 1)
  neighbours[2:(no.row - 1), left.edge] <- NA
  if (order > 2) {
    p <- c(0, seq(from = 2 * order - 1, by = -2, length.out = order - 2))
    for (i in 2:(order - 1)) {
      for (j in 1:(order - i)) {
        neighbours[i, seq(from = (order * (order + 2) + 2 + p[j]), by = 1,
                          length.out = (order - i - j + 1))] <- NA
      }
    }
  }

  # Top Edge Issues
  top.edge <- vector(length = 0)
  p <- seq(from = 3, by = 2, to = (2 * order + 1))
  p <- c(0, p, rev(p)[-1])
  p <- cumsum(p)

  for (j in -(order - 1):(order - 1)) {
    top.edge <- c(top.edge,
                  seq(from = 2 + p[order + j],
                      by = 1, length.out = order - abs(j)))
  }

  neighbours[seq(from = (no.row + 1), by = no.row,
                 length.out = no.col - 2), top.edge] <- NA

  # Bottom Edge Issues
  bottom.edge <- vector(length = 0)
  p <- seq(from = 4, by = 2, length.out = order - 1)
  p <- c(0, p, rev(p))
  p <- cumsum(p)

  for (j in -(order - 1):(order - 1)) {
    bottom.edge <- c(bottom.edge,
                     seq(from = 4 + p[order + j],
                         by = 1, length.out = order - abs(j)))
  }

  neighbours[seq(from = 2 * no.row, to = (no.row * (no.row - 1)),
                 by = no.row), bottom.edge] <- NA

  # Right Edge Issues
  right.edge <- seq(from = (order * (order + 2) + 2),
                    to = (2 * order * (order + 1) + 1), by = 1)
  neighbours[seq(from = (no.row * (no.row - 1) + 1),
                 to = (no.row ^ 2 - 1), by = 1), right.edge] <- NA

  # Order Row Issues
  for (i in seq(from = order, by = no.row, length.out = no.col)) {
    neighbours[i, which(neighbours[i, ] %% no.row == 0)] <- NA
  }

  for (i in seq(from = no.row - (order - 1), by = no.row,
                length.out = no.col)) {
    neighbours[i, which(neighbours[i, ] %% no.row == 1)] <- NA
  }

  return(neighbours)
}
