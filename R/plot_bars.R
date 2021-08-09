# plot function - bar charts
#


#' plot a horizontal barplot
#'
#' @param v vector of values
#' @param n integer, number of bars to plot, defaults to plot all values in v
#' @param ylim numeric of length 2, use to explicitly set limits for y axis
#' @param xlab character, label for x axis
#' @param at list with components 'x' and 'y' with tick positions
#' @param Rcssclass character, style class
#' @param ... other arguments passed to points_umap
plot_bars <- function(v, n=length(v),
                      xlim=NULL,
                      xlab="",
                      at=list(x=seq(0, 10, by=2),
                              y=seq(0, 1, by=0.5)),
                      Rcssclass=NULL, ...) {
  RcssCompulsoryClass <- RcssGetCompulsoryClass(c("bars", Rcssclass))

  barwidth <- RcssValue("bars", "barwidth", default=0.8)
  bw2 <- barwidth/2

  if (is.null(xlim)) {
    xlim <- range(v)
  }
  ylim <- c(-n, -1) + bw2*c(-2, 2)

  parplot(xlim, ylim, type="n", xlim=xlim, ylim=ylim)
  axis(3, at=xlim, label=c("", ""), line=0, tck=0, Rcssclass="x")
  axis(3, at=at$x, label=NA, line=0, Rcssclass="x")
  axis(3, at=at$x, label=at$x, lwd=0, Rcssclass="x")
  mtext(side=3, xlab, Rcssclass="xlab")

  bar_centers <- seq(-1, -n, by=-1)
  rect(0, bar_centers+bw2, head(v, n), bar_centers-bw2)
  axis(2, at=bar_centers, label=head(names(v), n), Rcssclass="y")
}


#' plot a vertical/horizontal barplot with labels on the axis grouped
#'
#' @param x data table
#' @param value.col character, column in x with numeric values
#' @param group.col character, column in x for label groups
#' @param group.labels named list mapping groups (in x) to group labels
#' (shown in plot)
#' @param xlim numeric of length 2, use to explicitly set limits for x axis
#' @param ylim numeric of length 2, use to explicitly set limits for y axis
#' @param xlab character, label for x axis
#' @param ylab character, label for y axis
#' @param style.by character, column in x for styling of individual bars
#' @param horiz logical, set TRUE for horizontal bar plot
#' @param Rcssclass character, style class for plot
#'
plot_bargroups <- function(x, value.col="value", group.col="group",
                           group.labels=NULL, xlim=NULL, ylim=NULL,
                           xlab="", ylab="", style.by=NULL,
                           horiz=FALSE, Rcssclass=NULL) {

  RcssCompulsoryClass <- RcssGetCompulsoryClass(c("bars", Rcssclass))

  barwidth <- RcssValue("bars", "barwidth", default=0.8)
  groupgap <- RcssValue("bars", "groupgap", default=1)
  bw2 <- barwidth/2
  gg2 <- groupgap/2

  if (is.null(group.labels)) {
    group.labels <- unique(x[[group.col]])
    group.labels <- setNames(group.labels, group.labels)
  } else {
    group.labels <- rev(group.labels)
  }
  xparts <- split(x, x[[group.col]])[names(group.labels)]

  if (is.null(xlim)) {
    xlim <- c(-gg2, nrow(x) + length(xparts)*groupgap)
  }
  if (is.null(ylim)) {
    ylim <- c(0, max(x[[value.col]]))
  }

  if (horiz) {
    templim <- ylim
    ylim <- xlim
    xlim <- templim
    parplot(xlim, ylim, type="n", xlim=xlim, ylim=ylim)
    axis(3, at=xlim, label=c("", ""), line=0, tck=0, Rcssclass="x")
    axis(3, label=NA, line=0, Rcssclass="x")
    axis(3, lwd=0, Rcssclass="x")
    group.left <- 0
    y_line <- RcssValue("axis", "line", default=0, Rcssclass="y")
    for (i in seq_along(xparts)) {
      idata <- xparts[[i]]
      bar_centers <- group.left - 0.5 + seq_len(nrow(idata))
      if (is.null(style.by)) {
        rect(0, bar_centers-bw2, idata[[value.col]], bar_center+bw2)
      } else {
        for (j in seq_len(nrow(idata))) {
          rect(0, bar_centers[j]-bw2, idata[[value.col]][j], bar_centers[j]+bw2,
               Rcssclass=idata[[style.by]][j])
        }
      }
      group.right <- group.left + nrow(xparts[[i]]) + groupgap
      text(xlim[1]-strwidth("a")*y_line, (group.left + group.right)/2,
           group.labels[[i]], Rcssclass=c("axis", "y"))
      group.left <- group.right
    }
    axis(2, at=ylim, label=c("", ""), line=0, tck=0, Rcssclass="y")
    mtext(side=3, xlab, Rcssclass="xlab")
    mtext(side=2, ylab, Rcssclass="ylab")
  } else {
    parplot(xlim, ylim, type="n", xlim=xlim, ylim=ylim)
    axis(2, at=ylim, label=c("", ""), line=0, tck=0, Rcssclass="y")
    axis(2, label=NA, line=0, Rcssclass="y")
    axis(2, lwd=0, Rcssclass="y")
    group.left <- 0
    x_line <- RcssValue("axis", "line", default=0, Rcssclass="x")
    for (i in seq_along(xparts)) {
      idata <- xparts[[i]]
      bar_centers <- group.left - 0.5 + seq_len(nrow(idata))
      if (is.null(style.by)) {
        rect(bar_centers-bw2, 0, bar_center+bw2, idata[[value.col]])
      } else {
        for (j in seq_len(nrow(idata))) {
          rect(bar_centers[j]-bw2, 0, bar_centers[j]+bw2, idata[[value.col]][j],
               Rcssclass=idata[[style.by]][j])
        }
      }
      group.right <- group.left + nrow(xparts[[i]]) + groupgap
      text((group.left + group.right)/2, ylim[1]-strheight("a")*x_line,
           group.labels[[i]], Rcssclass=c("axis", "x"))
      group.left <- group.right
    }
    axis(1, at=xlim, label=c("", ""), line=0, tck=0, Rcssclass="x")
    mtext(side=1, xlab, Rcssclass="xlab")
    mtext(side=2, ylab, Rcssclass="ylab")
  }
}