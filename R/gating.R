#' gating
#'
#' The \code{gating} function is the main component of the \code{gatingfc}
#' package. It combines many of the other functions within this package
#' and provides a wrapper function to automated the procedure of gating.
#'
#' @param x data frame with variables to be gated
#' @param var.names the variables to be used for gating
#'
#' @return a data frame with the model output specified as:
#'
#' @author Kevin Brosnan (\email{kevin.c.brosnan@@gmail.com})
#'
#'
#' @export gating
gating <- function(x, var.names = NULL) {

####---- 1. Verify Inputs --------------------------------------------------####

  # If x is not a data.frame then stop
  if (!is.data.frame(x)) {
    stop('Input object must be a data frame of dimension N times p')
  }

  # Make sure var.names has a maximum of length 2
  if (length(var.names) > 2) {
    stop('var.names must have a maximum length of 2')
  }

  # Make sure x is of the correct dimension and set up var.names accordingly
  if (ncol(x) > 2 && is.null(var.names)) {
    warning(paste('Input object has greater than 2 columns of data and',
                  'var.names is not specified, using the first 2 columns',
                  'of input object. Please specify var.names for other',
                  'pairs of variables.', sep = "\n  "))
    var.names <- c(1, 2)
  } else if (ncol(x) < 2) {
    stop('Input object must be a data frame with at least 2 columns.')
  } else if (ncol(x) == 2) {
    var.names <- c(1, 2)
  }

####---- 2. Construct Lattice Grid -----------------------------------------####

  # Retain the rows of interest in x
  x <- x[, var.names]
  # Create the lattice grid
  grid <- make_grid(x)
  # Get the dimension of the lattice grid
  grid.dim <- nrow(grid)

####---- 3. Outlier Detection & Removal ------------------------------------####

  # Bottom Edge and Right Edge Observations set to zero
  grid[, grid.dim] <- grid[grid.dim, ] <- 0

  # Record the positions of edge values in original x
  extreme.values <- unique(c(which(x[,1] == grid.dim - 1),
                             which(x[,2] == grid.dim - 1)))

  # Create removals vector which returns 1 for outlier, 0 otherwise
  removals <- rep(0, times = nrow(x))
  removals[extreme.values] <- 1

####---- 4. Gating ---------------------------------------------------------####



####---- 5. Cluster Labelling ----------------------------------------------####


####---- 6. Reconstruct Data Frame -----------------------------------------####


####---- 7. Construct Output -----------------------------------------------####

  output <- structure(list())
  return(output)
}
