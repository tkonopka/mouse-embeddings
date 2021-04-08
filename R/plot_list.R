# plot function - text-based tables in plot format


# create a table of shortenings/abbreviations
list_abbreviations <- list(
  c("decreased", "decr."),
  c("increased", "incr."),
  c("abnormal", "abn."),
  c("morphology", "morph."),
  c("physiology", "phys."),
  c("function", "func."),
  c("phenotype", "phen."),
  c("incidence", "incid."),
  c("physiological", "phys."),
  c("classified", "class."),
  c("gastrointestinal", "gastrointest."),
  c("underdeveloped", "underdevel.")
)
plot_list_abbreviations <- data.table(do.call(rbind, list_abbreviations))
colnames(plot_list_abbreviations) <- c("long", "short")


#' shorten a long string (uses strwidth)
#'
#' @param x character string
#' @param max.width numeric
#'
#' @return character string based on x, with elipsis if cut short
shorten <- function(x, max.width) {
  edited <- rep(FALSE, length(x))
  result <- x
  too.long <- strwidth(result) > max.width
  counter <- 0
  while (sum(too.long)>0 & counter<max(nchar(x))) {
    result[too.long] <- substr(result[too.long], 1, nchar(result[too.long])-1)
    edited[which(too.long)] <- TRUE
    too.long <- strwidth(result) > max.width
    counter <- counter + 1
  }
  result[edited] <- paste0(result[edited], "...")
  result
}


#' abbreviate long strings using a dictionary (uses gsub)
#'
#' @param v character vector
#' @param abbreviations data frame with long/short abbreviations
#'
#' @return character vector based on v
abbreviate <- function(v, abbreviations=plot_list_abbreviations) {
  result <- v
  for (i in seq_along(abbreviations$long)) {
    result <- gsub(abbreviations$long[i], abbreviations$short[i], result)
  }
  result
}


#' draw a plot that looks like a table
#'
#' @param v character vector
#' @param main character, chart title
#' @param xlim numeric of length 2, use to explicitly set limits for x axis
#' @param ylim numeric of length 2, use to explicitly set limits for y axis
#' @param abbreviations data frame with $long and $short, used to abbreviate
#' strings
#' @param max.chars integer, number of characters to display on each line
#' @param Rcssclass character, style class
#'
plot_list <- function(v, main="",
                      xlim=NULL, ylim=NULL,
                      abbreviations=plot_list_abbreviations,
                      max.width=0.96,
                      Rcssclass=NULL) {
  RcssCompulsoryClass <- RcssGetCompulsoryClass(c("list", Rcssclass))

  if (is.null(xlim)) {
    xlim <- c(0, 1)
  }
  if (is.null(ylim)) {
    ylim <- c(0, 1)
  }
  line.height <- RcssValue("list", "line.height", default=0.1)
  title.height <- RcssValue("list", "title.height", default=0.15)
  padding <- RcssValue("list", "padding", default=0.05)

  # apply shortenings
  v <- abbreviate(v, abbreviations)
  y_main <- ylim[2]+(line.height/2 + title.height/2)
  y <- ylim[2] - (seq_along(v)-1)*line.height
  if (min(y)<ylim[1]) {
    y <- y[y>ylim[1]]
    num.cut = length(v) - length(y)+1
    v <- c(head(v, length(y)-1), paste0("(and ", num.cut, " others)"))
  }

  parplot(xlim, ylim, type="n")
  # draw boxes for the background
  rect(xlim[1]-padding, min(y)-line.height/2,
       xlim[2]+padding, y_main+title.height/2,
       Rcssclass="body")
  rect(xlim[1]-padding, y_main-title.height/2,
       xlim[2]+padding, y_main+title.height/2,
       Rcssclass="title")
  # draw the contents of the list
  text(0, y_main, main, Rcssclass="title")
  text(0, y, shorten(v, max.width), Rcssclass="body")
}

