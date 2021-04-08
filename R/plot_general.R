# general plot functions
# function multipanelLabel copied/modified from previous projects


#' add a text object into an existing plot (a label for a multi-panel figure)
#'
#' @param label character, text to place in top-left corner
#' @param x numeric, horizontal position where to place the label
#' (in user coordinates)
#' @param y numeric, vertical position where to place the label
#' (in user coordinates)
#'
multipanelLabel <- function(label, x=NULL, y=NULL) {
  nowpar <- graphics::par()
  pw <- nowpar$plt[2]-nowpar$plt[1]
  ph <- nowpar$plt[4]-nowpar$plt[3]
  uw <- nowpar$usr[2]-nowpar$usr[1]
  uh <- nowpar$usr[4]-nowpar$usr[3]
  if (is.null(x)) {
    x <- nowpar$usr[1] - (uw/pw)*nowpar$plt[1]
  }
  if (is.null(y)) {
    y <- nowpar$usr[3] - (uh/ph)*nowpar$plt[3] + (uh/ph)
  }
  text(x, y, label, xpd=1, Rcssclass="multipanel")
}



#' add a set of axes and labels to an existing plot
add_axes <- function(xlim, ylim=NULL,
                     at=list(x=seq(0, 1, 4), y=seq(0,1, 4))) {
  # ensure that a vector is named
  force_names <- function(x) {
    if (is.null(names(x)))
      x <- setNames(x, x)
    x
  }
  if (!is.null(xlim)) {
    axis(1, at=xlim, label=c("", ""), line=0, tck=0, Rcssclass="x")
    axis(1, at=at$x, label=NA, line=0, Rcssclass="x")
    for (i in seq_along(at$x)) {
      axis(1, at=at$x[i], label=names(force_names(at$x))[i], lwd=0, Rcssclass="x")
    }
  }
  if (!is.null(ylim)) {
    axis(2, at=ylim, label=c("", ""), line=0, tck=0, Rcssclass="y")
    axis(2, at=at$y, label=NA, line=0, Rcssclass="y")
    axis(2, at=at$y, label=names(force_names(at$y)), lwd=0, Rcssclass="y")
  }
}

#' add a set of labels to an existing plot
add_mtext <- function(xlab, ylab, main) {
  mtext(side=1, xlab, Rcssclass="xlab")
  mtext(side=2, ylab, Rcssclass="ylab")
  mtext(side=3, main, Rcssclass="main")
}