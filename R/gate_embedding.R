# working with gates in an embedding
#


#' extract a smaller embedding dataset by focusing on a gate
#'
#' @param d data table with an embedding
#' @param xy character vector of length 2, two column in d with coordinates
#' @param xlim numeric of length 2, limits for a rectangular gate
#' @param ylim numeric of length 2, limits for a rectangular gate
#'
#' @return data table containing a subset of rows from d
gate_embedding <- function(d, xy=c("UMAP_1", "UMAP_2"), xlim=NULL, ylim=NULL) {
  # shortcut to get coordinates
  x <- d[[xy[1]]]
  y <- d[[xy[2]]]
  # apply the gate to get a vector of included/excluded items
  hits <- rep(TRUE, nrow(d))
  if (!is.null(xlim) & !is.null(ylim)) {
    hits <- (x >= xlim[1] & x <= xlim[2]) & (y >= ylim[1] & y <= ylim[2])
  }
  d[hits]
}


#' get a rectangle around a target item in an embedding
#'
#' @param d data table with an embedding
#' @param target character, central item
#' @param xy
#' @param r numeric, radius around the target
#'
#' @return numeric vector of length 4, coordinates like for graphics::rect
rect_around_target <- function(d, target,
                               xy=c("UMAP_1", "UMAP_2"), r=1) {
  result <- d[id==target]
  x <- result[[xy[1]]]
  y <- result[[xy[2]]]
  c(x-r, y-r, x+r, y+r)
}

