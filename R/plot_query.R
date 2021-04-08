# plot function - display text (crossmap query) in a plot format


#' modify a long character string so that it has newlines
#' so that it can wrap nicely when printed in a plot
#'
#' @param x character, words separated with spaces
#' @param max_width numeric, maximal line width
#' @param max_lines integer, maximal number of lines in output.
#' Longer text is truncated with elipsis (...)
#'
#' @return character, the same content as x, with newline separating line
wrapped_text <- function(x, max_width, max_lines=5) {
  words <- unlist(strsplit(x, " "))
  words_x <- cumsum(strwidth(words))
  line_no <- floor(words_x / max_width)
  result <- sapply(split(words, line_no), paste, collapse=" ")
  if (length(result)>max_lines) {
    result <- head(result, max_lines)
    result[[max_lines]] <- paste0(result[[max_lines]], "...")
  }
  paste(unlist(result), collapse="\n")
}


#' plot an example of a search query
#'
#' @param query character, content of query
#' @param query_modifiers character, modifiers for main query
#' @param main character, two columns in d, defaults to UMAP_1/UMAP_2
#' @param y_query numeric, y position of query line
#' @param y_modifiers numeric, y position of modifiers line
#' @param x_content numeric, x position of content
#' @param max_lines integer, number of lines
#' @param Rcssclass character, style class
#'
#' @return
plot_query <- function(query, query_modifiers, main="",
                       y_query=1, y_modifiers=0.5, x_content=0.3,
                       max_lines=5,
                       Rcssclass=NULL) {

  RcssCompulsoryClass <- RcssGetCompulsoryClass(c("query", Rcssclass))

  xylim <- c(0, 1)
  parplot(xylim, xylim, type="n")
  mtext(side=3, wrapped_text(main, 0.95), Rcssclass="main")

  query_wrapped <- wrapped_text(query, 1-x_content-0.05, max_lines)
  modifiers_wrapped <- wrapped_text(query_modifiers, 1-x_content-0.05, max_lines)

  text(0, y_query, "Query:", Rcssclass="label")
  text(x_content, y_query, query_wrapped, Rcssclass="content")
  text(0, y_modifiers, "Modifiers:", Rcssclass="label")
  text(x_content, y_modifiers, modifiers_wrapped, Rcssclass="content")

}

