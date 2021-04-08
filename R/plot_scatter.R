# plot function - scatter plot


#' plot a scatter plot
#'
#' @param d data table with at least two columns
#' @param xy character, two column names for x- and y-axis
#' @param xlim numeric of length 2, use to explicitly set limits for x axis
#' @param ylim numeric of length 2, use to explicitly set limits for y axis
#' @param qlim numeric of length 2, quantiles to determine xlim and ylim
#' @param force_square logical, sets xlim and ylim to same scale
#' @param xlab character, label for x axis
#' @param ylab character, label for y axis
#' @param main character, chart title
#' @param lab_suffix character, added after xlab and ylab
#' @param show_rect numeric of length 4, adds a rectangle in the background
#' @param show_points logical, default is to add points
#' @param at list with components 'x' and 'y' with tick positions
#' @param Rcssclass character, style class
#' @param ... other arguments passed to points_umap
plot_scatter <- function(d, xy=c(1, 2),
                         xlim=NULL, ylim=NULL,
                         xlab="UMAP 1", ylab="output", main="",
                         lab_suffix="",
                         show_rect=NULL,
                         show_points=TRUE,
                         at=list(x=seq(0, 10, by=2),
                                 y=seq(0, 1, by=0.5)),
                         Rcssclass=NULL, ...) {
  RcssCompulsoryClass <- RcssGetCompulsoryClass(c("scatter", Rcssclass))

  if (is.null(xlim)) {
    xlim <- range(d[[xy[1]]])
  }
  if (is.null(ylim)) {
    ylim <- range(d[[xy[2]]])
  }
  parplot(xlim, ylim, type="n")
  if (!is.null(show_rect)) {
    rect(show_rect[1], show_rect[2], show_rect[3], show_rect[4],
         Rcssclass="model")
  }

  add_axes(xlim, ylim, at=at)
  add_mtext(paste(xlab, lab_suffix), paste(ylab, lab_suffix), main)

  if (show_points) {
    points_scatter(d, xy, ...)
  }
}


#' add points into a scatter plot
#'
#' @param d data table
#' @param xy character vector, two columns in d
#' @param style_by character, column in x to extract style class
#' @param show_line logical, draw a line in the background
#' @param Rcssclass character, style class
#'
points_scatter <- function(d, xy, style_by=NULL, show_line=FALSE, Rcssclass=NULL, ...) {
  Rcssclass <- gsub("-", "_", Rcssclass)
  RcssCompulsoryClass <- RcssGetCompulsoryClass(c("scatter", Rcssclass))
  line_and_points <- function(xval, yval, Rcssclass=NULL) {
    if (show_line) {
      lines(xval, yval, Rcssclass=Rcssclass, ...)
    }
    points(xval, yval, Rcssclass=Rcssclass, ...)
  }
  if (is.null(style_by)) {
    line_and_points(d[[xy[1]]], d[[xy[2]]], ...)
  } else {
    d2 <- split(d, d[[style_by]])
    for (style in names(d2)) {
      line_and_points(d2[[style]][[xy[1]]], d2[[style]][[xy[2]]],
                      Rcssclass=style, ...)
    }
  }
}


#' add a line of best fit to a scatter plot
#'
#' @param d data table
#' @param xy character vector, two columns in d
#' @param vals numeric, values to predict
#' @param style_by character, column in x to extract style class
#' @param Rcssclass character, style class
#'
fitline_scatter <- function(d, xy, vals=NULL, style_by=NULL, Rcssclass=NULL, ...) {
  Rcssclass <- gsub("-", "_", Rcssclass)
  RcssCompulsoryClass <- RcssGetCompulsoryClass(c("scatter", Rcssclass))
  if (is.null(vals)) {
    vals <- range(d[[xy[1]]])
  }
  .df <- data.table(vals=vals)
  colnames(.df) <- xy[1]
  .formula <- as.formula(paste(xy[2], "~", xy[1]))
  if (is.null(style_by)) {
    p <- predict(lm(.formula, data=d), newdata=.df)
    lines(vals, p, ...)
  } else {
    d2 <- split(d, d[[style_by]])
    for (style in names(d2)) {
      p <- predict(lm(.formula, data=d2[[style]]), newdata=.df)
      lines(vals, p, Rcssclass=style, ...)
    }
  }
}

