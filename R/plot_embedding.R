# plot function - scatter plot representing an embedding
#


#' plot a scatter plot for a 2D embedding
#'
#' @param d data table with at least two numeric columns
#' @param xy character, two columns in d, defaults to UMAP_1/UMAP_2
#' @param xlim numeric of length 2, use to explicitly set limits for x axis
#' @param ylim numeric of length 2, use to explicitly set limits for y axis
#' @param qlim numeric of length 2, quantiles to determine xlim and ylim
#' @param force_square logical, sets xlim and ylim to same scale
#' @param xlab character, label for x axis
#' @param ylab character, label for y axis
#' @param main character, chart title
#' @param lab_suffix character, added after xlab and ylab
#' @param show_points logical, default is to add points
#' @param show_labels logical, default is to hide labels
#' @param Rcssclass character, style class
#' @param ... other arguments passed to points_embedding
plot_embedding <- function(d, xy=c("UMAP_1", "UMAP_2"),
                           xlim=NULL, ylim=NULL, qlim=c(0.005, 0.995),
                           force_square=TRUE,
                           xlab="UMAP 1", ylab="UMAP 2", main="",
                           lab_suffix="",
                           show_points=TRUE, show_labels=FALSE,
                           Rcssclass=NULL,
                           ...) {
  RcssCompulsoryClass <- RcssGetCompulsoryClass(c("embedding", Rcssclass))

  if (is.null(xlim)) {
    xlim <- quantile(d[[xy[1]]], p=qlim)
  }
  if (is.null(ylim)) {
    ylim <- quantile(d[[xy[2]]], p=qlim)
  }
  if (force_square) {
    xrange <- xlim[2]-xlim[1]
    yrange <- ylim[2]-ylim[1]
    extend_lim <- function(lim, factor) {
      if (factor<1) return (lim)
      width <- lim[2]-lim[1]
      lim + 0.5*c(-1, 1)*(factor-1)*width
    }
    xlim <- extend_lim(xlim, yrange/xrange)
    ylim <- extend_lim(ylim, xrange/yrange)
  }

  parplot(xlim, ylim, type="n")
  axis(1, at=xlim, label=c("", ""), line=0, tck=0, Rcssclass="x")
  .labels <- axis(1, label=NA, line=0, Rcssclass="x")
  .labels_even <- .labels[seq(2, length(.labels), by=2)]
  .labels_odd <- .labels[seq(1, length(.labels), by=2)]
  axis(1, at=.labels_even, label=.labels_even, lwd=0, Rcssclass="x")
  axis(1, at=.labels_odd, label=.labels_odd, lwd=0, Rcssclass="x")
  axis(2, at=ylim, label=c("", ""), line=0, tck=0, Rcssclass="y")
  .labels <- axis(2, label=NA, line=0, Rcssclass="y")
  axis(2, at=.labels, label=.labels, lwd=0, Rcssclass="y")
  add_mtext(paste(xlab, lab_suffix), paste(ylab, lab_suffix), main)

  if (show_points) {
    points_embedding(d, xy, ...)
  }
  if (show_labels) {
    text_embedding(d, xy, ...)
  }
}


#' add points into an embedding plot
#'
#' @param d data table with at least two numeric columns
#' @param xy character, two columns in d
#' @param detail numeric, proportion of points to show (downsampling)
#' @param style_by character, column in x to extract style class
#' @param style_groups character vector, style classes in order for plotting;
#' leave NULL to get a default ordering determined via split()
#' @param Rcssclass character, style class
#'
points_embedding <- function(d, xy=c("UMAP_1", "UMAP_2"),
                             detail=0.5, style_by=NULL, style_groups=NULL,
                             Rcssclass=NULL, ...) {
  Rcssclass <- gsub("-", "_", Rcssclass)
  RcssCompulsoryClass <- RcssGetCompulsoryClass(c("embedding", Rcssclass))
  if (detail<1) {
    subset_size <- ceiling(nrow(d)*detail)
    subset <- sample(seq_len(nrow(d)), subset_size, replace=FALSE)
    d <- d[sort(subset), ]
  }
  if (is.null(style_by)) {
    points(d[[xy[1]]], d[[xy[2]]], ...)
  } else {
    d2 <- split(d, d[[style_by]])
    if (is.null(style_groups)) {
      style_groups <- names(d2)
    }
    for (style in style_groups) {
      points(d2[[style]][[xy[1]]], d2[[style]][[xy[2]]], Rcssclass=style, ...)
    }
  }
}


#' add text labels into an embedding plot
#'
#' @param d data table with at least two numeric columns
#' @param xy character, two columns in d
#' @param detail numeric, proportion of points to show (downsampling)
#' @param col_by character, column in x to extract style class
#' @param Rcssclass character, style class
#'
text_embedding <- function(d, xy=c("UMAP_1", "UMAP_2"),
                           detail=0.5, style_by=NULL, Rcssclass=NULL, ...) {
  Rcssclass <- gsub("-", "_", Rcssclass)
  RcssCompulsoryClass <- RcssGetCompulsoryClass(c("embedding", Rcssclass))
  if (detail<1) {
    subset_size <- ceiling(nrow(d)*detail)
    subset <- sample(seq_len(nrow(d)), subset_size, replace=FALSE)
    d <- d[sort(subset), ]
  }
  if (is.null(style_by)) {
    text(d[[xy[1]]], d[[xy[2]]], d$id, ...)
  } else {
    d2 <- split(d, d[[style_by]])
    for (style in names(d2)) {
      text(d2[[style]][[xy[1]]], d2[[style]][[xy[2]]], d2[[style]]$id,
           Rcssclass=style, ...)
    }
  }
}


#' plot an embedding embedding, centered around a point
#'
#' @param d data table with at least two numeric columns
#' @param center numeric of length 2, center of diagram
#' @param r numeric, radius, determines xlim and ylim of chart
#' @param ... other arguments passed to plot_embedding
plot_centered_embedding <- function(d, center=c(0,0), r=0.3, ...) {
  plot_embedding(d, xlim=center[1]+c(-r, r), ylim=center[2]+c(-r, r), ...)
}



#' add a legend to an existing embedding plot
add_embedding_legend <- function(legend_pos=c(0,0), legend_shift=c(0, 0),
                                 labels=NULL,
                                 Rcssclass=NULL) {
  if (identical(labels, NULL)) {
    return(NULL)
  }

  RcssCompulsoryClass <- RcssGetCompulsoryClass(c("embedding", Rcssclass))

  if (identical(legend_pos, "topleft")) {
    usr <- graphics::par()$usr
    legend_pos <- usr[c(1,4)] + legend_shift
  } else if (identical(legend_pos, "bottomleft")) {
    usr <- graphics::par()$usr
    temp <- legend(0, 0, as.character(labels), plot=FALSE)
    legend_pos <- c(usr[1], usr[3]+temp$rect$h)
  }
  n <- length(labels)
  col <- rep(1, n)
  pch <- rep(1, n)
  lwd <- rep(1, n)
  cex <- rep(1, n)
  for (i in seq_len(n)) {
    iclass <- c("legend", names(labels)[i])
    col[i] <- RcssValue("points", "col", default=0, Rcssclass=iclass)
    lwd[i] <- RcssValue("points", "lwd", default=0.5, Rcssclass=iclass)
    cex[i] <- RcssValue("points", "cex", default=1, Rcssclass=iclass)
    pch[i] <- RcssValue("points", "pch", default=1, Rcssclass=iclass)
  }
  legend(legend_pos[1], legend_pos[2], as.character(labels),
         pch=pch, pt.cex=cex, pt.lwd=lwd, col=substr(col, 0, 7))
}



#' add one point and one label to an embedding plot
#'
#' @param d data table with at least two numeric columns, and one row
#' @param xy character, two columns in d, defaults to UMAP_1/UMAP_2
#' @param label_text character, if provided, this is used as the label
#' @param label_column character, used to extract
#' @param label_coord numeric of length 2, coordinates for the text label
#' @param Rcssclass character, style class

add_embedding_label <- function(d, xy=c("UMAP_1", "UMAP_2"),
                                label_text=NULL, label_column=NULL,
                                label_coord=NULL, Rcssclass=NULL) {
  if (nrow(d)>1) {
    stop("add_embedding_label only processes one label at at time")
  }
  RcssCompulsoryClass <- RcssGetCompulsoryClass(c("embedding", Rcssclass))
  x <- xy[1]
  y <- xy[2]
  points(d[[x]], d[[y]], Rcssclass="label")
  lines(c(d[[x]], label_coord[1]), c(d[[y]], label_coord[2]), Rcssclass="label")
  label_str <- ""
  if (!is.null(label_text)) {
    label_str <- label_text
  } else if (!is.null(label_column)) {
    label_str <- d[[label_column]]
  }
  text(label_coord[1], label_coord[2], label_str, Rcssclass="label")
}



#' draw a panel with a horizontal line and a centered heading
plot_line_header <- function(main) {
  RcssCompulsoryClass <- RcssGetCompulsoryClass(c("line_header"))
  parplot(c(0, 1), c(0, 1))
  lines(c(0, 1), c(0, 0))
  text(0.5, 0.5, main)
}