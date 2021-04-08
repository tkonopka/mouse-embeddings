# plot function - enrichment analyses (p.values, etc.)
#


#' plot results of an enrichment analysis
#'
#' @param d data table with columns phenotype, p.value, phred
#' @param p_threshold numeric, p-value threshold, skips non-signficant
#' @param xlim numeric of length 2, use to explicitly set limits for x axis
#' @param ylim numeric of length 2, use to explicitly set limits for y axis
#' @param xlab character, label for x axis
#' @param ylab character, label for y axis
#' @param main character, chart title
#' @param threshold_label character, text printed by the threshold line
#' @param Rcssclass character, style class
#' @param ... other arguments passed to points_embedding
plot_enrichment <- function(d, p_threshold=0.05,
                            xlim=NULL, ylim=NULL,
                            xlab="fraction with phenotype", ylab="-log10 p-value",
                            main="", threshold_label="significance threshold",
                            Rcssclass=NULL,
                            ...) {
  RcssCompulsoryClass <- RcssGetCompulsoryClass(c("enrichment", Rcssclass))

  p_threshold <- p_threshold / nrow(d)
  d <- d[p.value < p_threshold, ]
  min_p <- min(d[p.value>0]$p.value)
  d$phred <- pmin(-log10(min_p), -log10(d$p.value))
  d$fraction <-  d$count_11 / (d$count_11 + d$count_10)

  if (is.null(xlim)) {
    xlim <- c(0, max(d$fraction))
  }
  if (is.null(ylim)) {
    ylim <- c(0, max(d$phred))
  }

  parplot(xlim, ylim, type="n")
  axis(1, at=xlim, label=c("", ""), line=0, tck=0, Rcssclass="x")
  axis(1, label=NA, line=0, Rcssclass="x")
  axis(1, lwd=0, Rcssclass="x")
  axis(2, at=ylim, label=c("", ""), line=0, tck=0, Rcssclass="y")
  axis(2, label=NA, line=0, Rcssclass="y")
  axis(2, lwd=0, Rcssclass="y")
  mtext(side=1, xlab, Rcssclass="xlab")
  mtext(side=2, ylab, Rcssclass="ylab")
  mtext(side=3, main, Rcssclass="main")

  lines(xlim, rep(-log10(p_threshold), 2), Rcssclass="threshold")
  text(xlim[2], -log10(p_threshold), threshold_label,
       Rcssclass="threshold")
  points(d$fraction, d$phred)
}

