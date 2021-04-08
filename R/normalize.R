# functions for normalizing matrices
#


#' normalize vectors in a matrix, row-by-row
#'
#' @param m numeric matrix
#'
#' @return modified matrix where each row has unit norm
normalize_by_row <- function(x) {
  for (i in seq_len(nrow(x))) {
    r <- x[i,]
    r2sum <- sum(r*r)
    if (r2sum > 0) {
      x[i,] <- r / sqrt(r2sum)
    }
  }
  x
}


#' function with same signature as normalize_by_row, does nothing
#'
#' @param m numeric matrix
#'
#' @return same matrix as input
normalize_none <- function(x) {
  x
}

