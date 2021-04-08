# plot function - small 2x2 contingency table


#' plot a 2x2 table
#'
#' @param x matrix
#' @param xlab character, label for x axis
#' @param ylab character, label for y axis
#' @param main character, label for top of plot
#' @param Rcssclass character, style class
#'
plot_tab2x2 <- function(x, xlab="", ylab="", main="", Rcssclass=NULL) {
  RcssCompulsoryClass <- RcssGetCompulsoryClass(c("tab2x2", Rcssclass))

  xlim <- ylim <- c(0, 1)
  parplot(xlim, ylim, type="n", xlim=xlim, ylim=ylim)
  axis(1, at=c(0.25, 0.75), labels=colnames(x), lwd=0, Rcssclass="x")
  axis(2, at=c(0.25, 0.75), labels=rownames(x), lwd=0, Rcssclass="y")

  rect(c(0, 0.5), c(0, 0), c(0.5, 1), c(0.5, 0.5))
  rect(c(0, 0.5), c(0.5, 0.5), c(0.5, 1), c(1, 1))
  text(rep(0.25, 2), c(0.25, 0.75), x[,1], Rcssclass="value")
  text(rep(0.75, 2), c(0.25, 0.75), x[,2], Rcssclass="value")

  mtext(side=1, xlab, Rcssclass="xlab")
  mtext(side=2, ylab, Rcssclass="ylab")
  mtext(side=3, main, Rcssclass="main")

}

