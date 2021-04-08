# plot function - histogram using only a line (not bars)


#' plot a histogram as a line
#'
#' @param v vector of values
#' @param breaks numeric vector, breakpoints for x-axis
#' @param xlim numeric of length 2, use to explicitly set limits for x axis
#' @param ylim numeric of length 2, use to explicitly set limits for y axis
#' @param xlab character, label for x axis
#' @param ylab character, label for y axis
#' @param main character, chart title
#' @param line_label character, text printed next to line
#' @param line_label_x numeric, x coordinate of line label
#' @param at list with components 'x' and 'y' with tick positions
#' @param Rcssclass character, style class
#' @param ... other arguments passed to points_umap
plot_linehist <- function(v, breaks,
                          xlim=NULL, ylim=NULL,
                          xlab="UMAP 1", ylab="output", main="",
                          line_label="", line_label_x=0,
                          at=list(x=seq(0, 10, by=2),
                                  y=seq(0, 1, by=0.5)),
                          Rcssclass=NULL, ...) {
  RcssCompulsoryClass <- RcssGetCompulsoryClass(c("linehist", Rcssclass))
  result <- hist(v, breaks=breaks, plot=FALSE)

  if (is.null(xlim)) {
    xlim <- range(v)
  }
  if (is.null(ylim)) {
    ylim <- range(result$density)
  }
  parplot(xlim, ylim, type="n", xlim=xlim, ylim=ylim)
  add_axes(xlim, ylim, at)
  add_mtext(xlab, ylab, main)

  lines(rep(breaks, each=2)[seq(2, 2*length(breaks)-1)],
        rep(result$density, each=2))
  line_label_y <- result$density[min(which(breaks>line_label_x))]
  text(line_label_x, line_label_y, line_label,
       Rcssclass="label")
}

