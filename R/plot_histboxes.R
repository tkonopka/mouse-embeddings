# plot a chart that is like a histgram, but shows a boxplot in each bin


#' plot a cross between a histogram and boxplot
#'
#' @param d data table with at least two columns
#' @param xy character, two column names for x- and y-axis
#' @param breaks numeric vector, breakpoints for x-axis
#' @param numeric of length 5, quantiles for boxes (whiskers, boxes, middle)
#' @param xlim numeric of length 2, use to explicitly set limits for x axis
#' @param ylim numeric of length 2, use to explicitly set limits for y axis
#' @param xlab character, label for x axis
#' @param ylab character, label for y axis
#' @param main character, chart title
#' @param at list with components 'x' and 'y' with tick positions
#' @param Rcssclass character, style class
#' @param ... other arguments passed to points_umap
plot_histboxes <- function(d, xy=c(1, 2), breaks,
                           q=c(0.05, 0.25, 0.5, 0.75, 0.95),
                           xlim=NULL, ylim=NULL,
                           xlab="UMAP 1", ylab="output", main="",
                           at=list(x=seq(0, 10, by=2),
                                   y=seq(0, 1, by=0.5)),
                           Rcssclass=NULL,
                           ...) {
  RcssCompulsoryClass <- RcssGetCompulsoryClass(c("histboxes", Rcssclass))

  if (is.null(xlim)) {
    xlim <- range(d[[xy[1]]])
  }
  if (is.null(ylim)) {
    ylim <- range(d[[xy[2]]])
  }
  parplot(xlim, ylim, type="n")
  add_axes(xlim, ylim, at)
  add_mtext(xlab, ylab, main)

  # compute boxes using quantiles
  boxes <- matrix(0, ncol=5, nrow=length(breaks)-1)
  d2 <- as.matrix(as.data.frame(d)[, xy])
  for (i in seq_along(breaks)[-1]) {
    bin <- c(breaks[i-1], breaks[i])
    values <- d2[d2[,1]>=bin[1] & d2[,1]<bin[2], 2]
    boxes[i-1, ] <- quantile(values, p=q)
  }

  # draw the boxes, mid-lines, whiskers
  bins <- cbind(head(breaks, length(breaks)-1),
                tail(breaks, length(breaks)-1),
                NA)
  bin_centers <- (bins[,1] + bins[,2])/2
  rect(bins[, 1], boxes[, 2], bins[, 2], boxes[, 4])
  lines(as.numeric(t(bins)), rep(boxes[,3], each=3), Rcssclass="mid")
  whiskers <- cbind(boxes[,1:2], NA, boxes[,4:5], NA)
  lines(rep(bin_centers, each=6), as.numeric(t(whiskers)), Rcssclass="whisker")

}

