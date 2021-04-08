# plot function - simple heat map
#


#' plot a heatmap with some continuous-value columns and some categorical
#' columns
#'
#' labels appear on the right
#'
#' @param d data table with at least two columns
#' @param continuous.cols character, columns with continuous data
#' @param categorical.cols character vector, columns with categorical data
#' @param label.col character, column in d with item labels
#' @param label.width numeric, used to trim item labels
#' @param axis.labels named vector, labels for columns that appear on the axis
#' @param main character, chart title
#' @param col.limits numeric of length 2, upper and lower color saturation values
#' @param col.fun function with signature like magma, inferno, viridis
#' @param Rcssclass character, style class
#'
plot_heatmap <- function(d, continuous.cols=NULL, categorical.cols=NULL,
                         label.col=NULL, label.width=6, axis.labels=NULL,
                         main="",
                         col.limits=c(0, 1), col.fun=magma,
                         Rcssclass=NULL) {
  RcssCompulsoryClass <- RcssGetCompulsoryClass(c("heatmap", Rcssclass))

  xlim <- c(0, length(continuous.cols)+length(categorical.cols)+0.5)
  ylim <- c(-nrow(d), 0)

  parplot(xlim, ylim, type="n")
  mtext(side=3, main, Rcssclass="main")

  colors <- col.fun(1024, begin=0, end=1)
  line_x <- RcssValue("axis", "line", default=1, Rcssclass="x")
  if (is.null(axis.labels)) {
    axis.labels <- c(continuous.cols, categorical.cols)
    axis.labels <- setNames(gsub("_", " ", axis.labels), axis.labels)
  }

  # draw rectangles for the continuous data
  x <- 0
  for (i in seq_along(continuous.cols)) {
    ivalues <- d[[continuous.cols[i]]]
    ivalues.norm <- (ivalues-col.limits[1])/(col.limits[2]-col.limits[1])
    ivalues.norm[ivalues.norm<0] <- 0
    ivalues.norm[ivalues.norm>1] <- 1
    ivalues.norm <- 1+floor(ivalues.norm*1023)
    for (j in seq_len(length(ivalues))) {
      rect(x, -j, x+1, -j+1, col=colors[ivalues.norm[j]], Rcssclass="cell")
    }
    text(x+0.5, -length(ivalues)-strheight("a")*line_x,
         label=axis.labels[[continuous.cols[i]]], Rcssclass=c("axis", "x"))
    x <- x+1
  }
  rect(0, ylim[1], x, ylim[2], Rcssclass="outer")

  # draw rectangles for the categorical data
  x <- length(continuous.cols)+0.5
  for (i in seq_along(categorical.cols)) {
    ivalues <- d[[categorical.cols[i]]]
    for (j in seq_len(length(ivalues))) {
      rect(x, -j, x+1, -j+1, Rcssclass=c("cell", ivalues[j]))
    }
    text(x+0.5, -length(ivalues)-strheight("a")*line_x,
         label=axis.labels[[categorical.cols[i]]], Rcssclass=c("axis", "x"))
    x <- x + 1
  }
  rect(length(continuous.cols)+0.5, ylim[1], x, ylim[2], Rcssclass="outer")

  # draw the labels on the right
  item.labels <- shorten(d[[label.col]], label.width)
  for (j in seq_len(length(item.labels))) {
    axis(4, at=-j+0.5, label=item.labels[j], Rcssclass="y")
  }

}


#' add a color scale to a heatmap
#'
#' @param scale_rect numeric of length 4, boundaries of rectangle with scale
#' @param col.limits numeric length 2, threshold limits for color function
#' @param col.fun function
#' @param main character, label printed above the scale
#' @param Rcssclass character, style class
#'
add_heatmap_scale <- function(scale_rect, col.limits=c(0, 1), col.fun=magma,
                               main="", col.labels=c(0, 0.5, 1),
                               Rcssclass=NULL) {
  RcssCompulsoryClass <- RcssGetCompulsoryClass(c("heatmap", "legend", "scale", Rcssclass))
  colors <- col.fun(128, begin=0, end=1)
  scale_x <- seq(scale_rect[1], scale_rect[3], length=129)
  scale_left <- head(scale_x, -1)
  scale_right <- tail(scale_x, -1)
  rect(scale_left, scale_rect[2], scale_right, scale_rect[4], col=colors,
       Rcssclass="cell")
  rect(scale_rect[1], scale_rect[2], scale_rect[3], scale_rect[4],
       Rcssclass="outer")
  scale_y <- scale_rect[c(2,4)]
  text(scale_rect[1], max(scale_y), main, Rcssclass="main")
  scale_width <- max(scale_x)-min(scale_x)
  for (i in seq_along(col.labels)) {
    ix <- min(scale_x) + (col.labels[i]*scale_width)
    text(ix, min(scale_y), col.labels[i], Rcssclass="label")
  }
}


#' add a color scale to a heatmap
#'
#' @param labels named vector mapping factors to text
#' @param main character, label printed above the scale
#' @param Rcssclass character, style class
#'
add_heatmap_legend <- function(x=0, y=0, labels=c(a="a", b="b"),
                               width=1, height=1,
                               main="",
                               Rcssclass=NULL) {
  RcssCompulsoryClass <- RcssGetCompulsoryClass(c("heatmap", "legend", Rcssclass))
  x.intersp <- RcssValue("legend", "x.intersp", default=0.5)
  y.intersp <- RcssValue("legend", "y.intersp", default=0.5)

  .y <- y
  for (i in seq_along(labels)) {
    rect(x, .y-height, x+width, .y,
         Rcssclass=c("cell", names(labels)[i]))
    text(x+(width*(1+x.intersp)), .y-(height/2), labels[i], Rcssclass="label")
    .y <- .y - (height*(1+y.intersp))
  }
  text(x, y+(0.5+y.intersp)*height, main, Rcssclass="main")
}

