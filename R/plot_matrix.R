# plot function - simple heat map
#


#' plot a square matrix as a heatmap
#'
#'
#' @param m matrix,
#' @param groups_x list, determines order of labels on the x-axis
#' @param groups_y list, determines order of labels on the y-axis
#' @param labels named vector, mapping column names in m to labels for axes
#' @param main character, chart title
#' @param col.limits numeric of length 2, upper and lower color saturation values
#' @param col.fun function with signature like magma, inferno, viridis
#' @param Rcssclass character, style class
#'
plot_matrix <- function(m, groups_x=colnames(x), groups_y=rownames(y),
                        labels=setNames(rownames(m), rownames(m)),
                        main="", xlim=NULL, ylim=NULL,
                        col.limits=c(0, 1), col.fun=magma,
                        Rcssclass=NULL) {
  RcssCompulsoryClass <- RcssGetCompulsoryClass(c("matrix", Rcssclass))
  axis_line_x <- RcssValue("axis", "line", default=0.5, Rcssclass="x")
  axis_line_y <- RcssValue("axis", "line", default=0.5, Rcssclass="y")
  group_padding <- RcssValue("matrix", "group_padding", default=0.1)

  items_x <- unlist(groups_x)
  items_y <- unlist(groups_y)
  m <- m[items_y, items_x]

  if (is.null(xlim)) xlim <- c(0, ncol(m))
  if (is.null(ylim)) ylim <- c(-nrow(m), 0)

  parplot(xlim, ylim, type="n")
  mtext(side=3, main, Rcssclass="main")

  if (is(col.fun, "function")) {
    colors <- col.fun(1024, begin=0, end=1)
  } else {
    colors <- rgb(colorRamp(col.fun)(seq(0, 1, length=1024))/255)
  }

  # draw cells in the matrix
  for (i in seq_len(ncol(m))) {
    ivalues <- m[, i]
    ivalues.norm <- (ivalues-col.limits[1])/(col.limits[2]-col.limits[1])
    ivalues.norm[ivalues.norm<0] <- 0
    ivalues.norm[ivalues.norm>1] <- 1
    ivalues.norm <- 1+floor(ivalues.norm*1023)
    for (j in seq_len(length(ivalues))) {
      rect(i-1, -j, i, -j+1, col=colors[ivalues.norm[j]], Rcssclass="cell")
    }
  }
  rect(0, 0, ncol(m), -nrow(m), Rcssclass="outer")

  # draw labels on the axes
  x <- 0
  for (i in seq_along(groups_x)) {
    i_size <- length(groups_x[[i]])
    if (i_size>1) {
      lines(x+c(group_padding, i_size-group_padding), rep(axis_line_x/3, 2),
            Rcssclass="group")
    }
    text(x + (i_size/2), axis_line_x, label=labels[names(groups_x)[i]],
         Rcssclass=c("axis", "x"))
    x <- x + i_size
  }
  y <- 0
  for (j in seq_along(groups_y)) {
    j_size <- length(groups_y[[j]])
    if (j_size>1) {
      lines(rep(-axis_line_y/3, 2), y-c(group_padding, j_size-group_padding),
            Rcssclass="group")
    }
    text(-axis_line_y, y-(j_size)/2, label=labels[names(groups_y)[j]],
         Rcssclass=c("axis", "y"))
    y <- y - j_size
  }

}

