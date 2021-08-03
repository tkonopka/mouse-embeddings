# plot function - legend with bar graph
#


#' plot a legend with a bar graph
#'
#' @param counts data table with a $count column and an $id column
#' @param labels list mapping ids to text labels (printed on graph)
#' @param show_count logical, set TRUE to display number next to bars
#' @param xlim numeric of length 2, use to explicitly set limits for x axis
#' @param ylim numeric of length 2, use to explicitly set limits for y axis
#' @param qlim numeric of length 2, quantiles to determine xlim and ylim
#' @param xlab character, label for x axis
#' @param main character, chart title
#' @param Rcssclass character, style class
#' @param ... other arguments passed to points_embedding
plot_legend_bars <- function(counts, labels, show_count=TRUE,
                             xlim=NULL, ylim=NULL,
                             xlab="", main="",
                             Rcssclass=NULL,
                             points_Rcssclass="embedding",
                             ...) {

  RcssCompulsoryClass <- RcssGetCompulsoryClass(c("legend_bars", Rcssclass))
  points_x <- RcssValue("legend_bars", "points_x", default=-0.5)
  labels_x <- RcssValue("legend_bars", "labels_x", default=-0.5)
  labels_y <- RcssValue("legend_bars", "labels_y", default=0.5)
  main_y <- RcssValue("legend_bars", "main_y", default=0.5)
  barwidth <- RcssValue("legend_bars", "barwidth", default=0.8)

  if (is.null(ylim)) {
    ylim <- c(-length(labels), 0)
  }
  height <- abs(ylim[2]-ylim[1])
  if (is.null(xlim)) {
    xlim <- c(0, max(counts$count))
  }
  width <- abs(xlim[2]-xlim[1])

  id_col <- setdiff(colnames(counts), "count")[1]
  counts$id <- counts[[id_col]]

  parplot(xlim, ylim, type="n")
  axis(3, at=xlim, label=c("", ""), line=0, tck=0, Rcssclass="x")
  axis(3, label=NA, line=0, Rcssclass="x")
  axis(3, lwd=0, Rcssclass="x")
  text(mean(xlim), labels_y, xlab, Rcssclass="xlab")
  text(xlim[1]+points_x*width+(points_x-labels_x)*width, main_y,
       main, Rcssclass="main")

  n <- length(labels)
  points_x <- xlim[1]+points_x*width
  labels_x <- xlim[1]+labels_x*width
  for (i in seq_len(n)) {
    iclass <- names(labels)[i]
    icount <- counts$count[counts$id==iclass]
    icol <- RcssValue("points", "col", default=0,
                      Rcssclass=c(points_Rcssclass, iclass))
    ilwd <- RcssValue("points", "lwd", default=0.5,
                      Rcssclass=c(points_Rcssclass, iclass))
    icex <- RcssValue("points", "cex", default=1,
                      Rcssclass=c(points_Rcssclass, iclass))
    ipch <- RcssValue("points", "pch", default=1,
                      Rcssclass=c(points_Rcssclass, iclass))
    points(points_x, -i+0.5, xpd=1,
           col=substr(icol, 0, 7), lwd=ilwd, cex=icex, pch=ipch)
    text(labels_x, -i+0.5, labels[[i]], Rcssclass="label")
    rect(0, -i+0.5+barwidth/2, icount, -i+0.5-barwidth/2)
    if (show_count) {
      text(icount, -i+0.5, paste(" ", icount), Rcssclass="count")
    }
  }

}

