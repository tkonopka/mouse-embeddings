# plot functions for vignettes - draw cartoons / schematic diagrams


#' draw an empty diagram
#'
plot_schematic_blank <- function(height=1) {
  RcssCompulsoryClass <- RcssGetCompulsoryClass(c("schematic", "blank"))
  parplot(c(0, 1), c(1-height, 1), type="n",
          xlim=c(0, 1), ylim=c(1-height, 1))
}


#' draw a horizontal line with a title
add_schematic_line_header <- function(main, xlim, y) {
  RcssCompulsoryClass <- RcssGetCompulsoryClass("schematic")
  lines(xlim, rep(y, 2), Rcssclass="line_header")
  text(mean(xlim), y, main, Rcssclass="line_header")
}


#' draw a cartoon ontology graph
#'
#' @param root numeric of length 2, coordinates for the root node of graph
add_schematic_ontology <- function(root, width=0.3, height=0.1) {
  RcssCompulsoryClass <- RcssGetCompulsoryClass("schematic")
  leaves <- seq(root[1]-width/2, root[1]+width/2, length=6)
  mids <- c(mean(leaves[1:2]), mean(leaves[3:4]), mean(leaves[5:6]))
  # draw the edges
  edge_y <- root[2] - c(0, 0.5, 1, 0.5, 1)*height
  lines(c(root[1], mids[1], leaves[1], mids[1], leaves[2]), edge_y,
        Rcssclass="ontology")
  lines(c(root[1], mids[2], leaves[3], mids[2], leaves[4]), edge_y,
        Rcssclass="ontology")
  lines(c(root[1], mids[3], leaves[5], mids[3], leaves[6]), edge_y,
        Rcssclass="ontology")
  # draw the nodes on top
  points(root[1], root[2], Rcssclass="ontology")
  points(mids, rep(root[2]-height/2, 3), Rcssclass="ontology")
  points(leaves, rep(root[2]-height, 6), Rcssclass="ontology")
}


#' draw content for one term
#'
#' @param pos numeric of length 2, coordinate of top-left corner
#' @param x list, item for crossmap
#' @param max.width numeric, width used to cut long text
#' @param max.lines integer, maximal number of lines to print
#' @param line.height numeric, height of each line
#' @param indent character, prependent to lines to create an appearance
#' of an indented block
#'
add_schematic_term_description <- function(pos, x, max.width=0.3,
                                           max.lines=5,
                                           line.height=0.05,
                                           indent=0.04) {
  RcssCompulsoryClass <- RcssGetCompulsoryClass(c("schematic", "term"))
  result <- lapply(x, function(term) {
    term_result <- c(paste0(term$metadata$id, ":"),
                     term$data$name, term$data$def, unlist(term$data$parents))
    head(shorten(term_result, max.width), max.lines)
  })
  y <- pos[2]
  for (i in seq_along(result)) {
    i.data <- result[[i]]
    text(pos[1], y, i.data[1])
    i.data <- i.data[-1]
    for (j in seq_along(i.data)) {
      text(pos[1]+indent, y, i.data[j])
      y <- y - line.height
    }
  }
}



#' draw a table, row-by-row
#'
#' @param pos numeric, top-left coordinate of table
#' @param x matrix or data frame
#' @param max.width numeric, width of table
#' @param line.height numeric, height of one table row
add_schematic_table <- function(pos, x, max.width, line.height=0.05) {
  RcssCompulsoryClass <- RcssGetCompulsoryClass(c("schematic", "table"))
  col.mids <- seq(pos[1]-max.width/2, pos[1]+max.width/2, length=2*ncol(x)+1)
  col.mids <- col.mids[seq(2, length(col.mids), by=2)]
  text(col.mids, rep(pos[2], length(col.mids)), colnames(x), Rcssclass="header")
  lines(pos[1]+c(-max.width/2, max.width/2), rep(pos[2]-line.height/2, 2))
  y <- pos[2]-line.height
  for (i in seq_len(nrow(x))) {
    text(col.mids, rep(y, length(col.mids)), as.character(x[i,]))
    y <- y - line.height
  }
  for (j in seq_along(col.mids)[-1]) {
    lines(rep((col.mids[j]+col.mids[j-1])/2, 2), c(y, pos[2])+line.height/2)
  }
}


#' draw a small heatmap
add_schematic_heatmap <- function(pos, x, width=0.2, height=0.10,
                                  color="#222222") {
  RcssCompulsoryClass <- RcssGetCompulsoryClass(c("schematic", "heatmap"))
  boxes_x <- seq(pos[1]-width/2, pos[1]+width/2, length=ncol(x)+1)
  boxes_left <- rev(rev(boxes_x)[-1])
  boxes_right <- boxes_x[-1]
  row.height <- height/nrow(x)
  x_trans <- matrix(sprintf("%x", as.integer(x*255)), ncol=ncol(x))
  x_trans[x_trans=="0"] <- "00"
  for (i in seq_len(nrow(x))) {
    rect(boxes_left, rep(pos[2]-(i-1)*row.height, length(boxes_left)),
         boxes_right, rep(pos[2]-i*row.height, length(boxes_left)),
         col=paste0(color, x_trans[i,]),
         Rcssclass="cell")
    text(pos[1]-width/2, pos[2]-(i-0.5)*row.height, rownames(x)[i],
         Rcssclass="axis", adj=c(1, 0.5))
  }
  rect(min(boxes_left), pos[2], max(boxes_right), pos[2]-height,
       Rcssclass="border")
  text(pos[1], pos[2]+0.5*row.height, "phenotypes",
       Rcssclass="axis", adj=c(0.5, 0))
}


#' draw closed polygon centered around (x, y), radius r, with n_segments
#'
#' @param center numeric of length 2, coordinates for marker center
#' @param r numeric, size of marker
#' @param label character, text for center of marker
#' @param n_segments integer, number of segments for marker
#' (rectangle, pentagon, hexagon)
#' @param Rcssclass character, style class
draw_knn_marker <- function(center, r, label=NULL, n_segments=5,
                            Rcssclass=NULL) {
  a <- head(seq(0, 2*pi, length=n_segments+1), n_segments)
  polygon(center[1]+r*sin(a), center[2]+r*cos(a), Rcssclass=Rcssclass)
  text(center[1], center[2], label, Rcssclass=Rcssclass)
}


#' plot a schematic of one node and its neighbors
#'
#' This extracts values from knn {} selector in css
#'
#' @param label character, label for central gene
#' @param neighbors character vector, labels for neighbor nodes
#' @param neighbor_style character vector, css styles for neighbors
#' @param n_segments integer, number of corners on polygons
#' @param Rcssclass character, style class
plot_schematic_knn <- function(label, neighbors, neighbor_style=neighbors,
                               xlim=c(-1, 1), ylim=c(-1, 1),
                               Rcssclass=NULL) {

  # extract geometry information from css
  n_segments <- RcssValue("knn", "n_segments", default=5, Rcssclass=Rcssclass)
  r_primary <- RcssValue("knn", "r_primary", default=0.2, Rcssclass=Rcssclass)
  r_neighbor <- RcssValue("knn", "r_neighbor", default=0.2, Rcssclass=Rcssclass)
  r_knn <- RcssValue("knn", "r_knn", default=0.8, Rcssclass=Rcssclass)

  RcssCompulsoryClass <- RcssGetCompulsoryClass(c("schematic", "knn", Rcssclass))
  parplot(xlim, ylim, type="n")
  n <- length(neighbors)
  angles <- head(seq(0, 2*pi, length=n+1), n)
  radial_x <- rbind(0, r_knn*sin(angles), NA)
  radial_y <- rbind(0, r_knn*cos(angles), NA)
  lines(as.numeric(radial_x), as.numeric(radial_y), Rcssclass="radial")
  for (i in seq_along(neighbors)) {
    draw_knn_marker(c(radial_x[2,i], radial_y[2,i]), r_neighbor,
                    n_segments=n_segments,
                    label=neighbors[i], Rcssclass=neighbor_style[i])
  }
  draw_knn_marker(c(0, 0), r_primary, n_segments=n_segments,
                  label=label, Rcssclass="primary")
}


#' plot a new chart with a legend for the knn schematic
#'
#' This extracts values from knn {} selector in css
#'
#' @param primary_label character, label for central gene
#' @param neighbor_label character vector, labels for neighbor node
#' @param property named character vector, for drawing color boxes
#' @param markers_x numeric, x-position for markers
#' @param labels_x numeric, x-position for legend labels
#' @param Rcssclass character, style class
#'
plot_schematic_knn_legend <- function(primary_label="",
                                      neighbor_label="",
                                      property=c(abc="abc", xyz="xyz"),
                                      markers_x=-0.75,
                                      labels_x=-0.5,
                                      xlim=c(-1, 1), ylim=c(-1, 1),
                                      Rcssclass="legend") {

  # extract geometry information from css
  n_segments <- RcssValue("knn", "n_segments", default=5, Rcssclass=Rcssclass)
  r_primary <- RcssValue("knn", "r_primary", default=0.2, Rcssclass=Rcssclass)
  r_neighbor <- RcssValue("knn", "r_neighbor", default=0.2, Rcssclass=Rcssclass)
  line_height <- RcssValue("knn", "line_height", default=0.3,
                               Rcssclass=Rcssclass)

  RcssCompulsoryClass <- RcssGetCompulsoryClass(c("schematic", "knn", Rcssclass))
  y <- ylim[2] - line_height
  parplot(xlim, ylim, type="n")

  # draw two types of markers with labels
  draw_knn_marker(c(markers_x, y), r_primary, n_segments=n_segments,
                  label="", Rcssclass="primary")
  text(labels_x, y, primary_label, Rcssclass="legend")
  y <- y - line_height
  draw_knn_marker(c(markers_x, y), r_neighbor, n_segments=n_segments,
                  label="", Rcssclass="neighbor")
  text(labels_x, y, neighbor_label, Rcssclass="legend")

  # draw rectangles with property colors
  marker_width <- (labels_x - markers_x)/2
  for (i in seq_along(property)) {
    y <- y - line_height
    rect(markers_x-marker_width, y-line_height/3,
         markers_x+marker_width, y+line_height/3,
         Rcssclass=names(property[i]))
    text(labels_x, y, property[i], Rcssclass="legend")
  }

}


#' plot a new chart with an equation explaining neighbor averaging
#'
#' This extracts values from knn {} selector in css
#'
#' @param primary_label character, label for central gene
#' @param neighbor_label character vector, labels for neighbor node
#' @param markers_x numeric, x-position for markers
#' @param labels_x numeric, x-position for legend labels
#' @param eq_x numeric, x-position for components in the equation
#' @param Rcssclass character, style class
#'
plot_schematic_knn_errors <- function(primary_label="",
                                      neighbor_label="",
                                      markers_x=-0.75,
                                      labels_x=-0.5,
                                      eq_x=c(-0.5, 0.0, 0.4, 0.6, 0.9),
                                      xlim=c(-1, 1), ylim=c(-1, 1),
                                      Rcssclass="legend") {

  # extract geometry information from css
  n_segments <- RcssValue("knn", "n_segments", default=5, Rcssclass=Rcssclass)
  r_primary <- RcssValue("knn", "r_primary", default=0.2, Rcssclass=Rcssclass)
  r_neighbor <- RcssValue("knn", "r_neighbor", default=0.2, Rcssclass=Rcssclass)
  line_height <- RcssValue("knn", "line_height", default=0.3,
                           Rcssclass=Rcssclass)

  RcssCompulsoryClass <- RcssGetCompulsoryClass(c("schematic", "knn", Rcssclass))
  y <- ylim[2] - line_height
  parplot(xlim, ylim, type="n")

  # draw two types of markers with labels
  draw_knn_marker(c(markers_x, y), r_primary, n_segments=n_segments,
                  label="", Rcssclass="primary")
  text(labels_x, y, primary_label, Rcssclass="legend")
  y <- y - line_height
  draw_knn_marker(c(markers_x, y), r_neighbor, n_segments=n_segments,
                  label="", Rcssclass="neighbor")
  text(labels_x, y, neighbor_label, Rcssclass="legend")

  # draw formula for average
  y <- y - line_height - line_height
  text(eq_x[1], y, "error = ", Rcssclass="legend")
  lines(rep(eq_x[2]-r_primary*1.25, 2), y+line_height*c(-0.6, 0.6), Rcssclass="norm")
  draw_knn_marker(c(eq_x[2], y), r_primary, n_segments=n_segments,
                  label="", Rcssclass="primary")
  text(eq_x[3], y, " - avg (", Rcssclass="legend")
  draw_knn_marker(c(eq_x[4], y), r_neighbor, n_segments=n_segments,
                  label="", Rcssclass="neighbor")
  text(eq_x[5], y, ")", Rcssclass="legend")
  lines(rep(eq_x[5]+(r_neighbor*0.75), 2), y+line_height*c(-0.6, 0.6), Rcssclass="norm")
}

