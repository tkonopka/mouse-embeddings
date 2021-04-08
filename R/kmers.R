

#' compute kmers for a string
#'
#' @param x string
#' @param k integer, length of k-mers
#'
#' @return character vector, all kmers in x
kmers <- function(x, k=6) {
  if (is(x, "list")) {
    return(lapply(x, kmers, k=k))
  }
  xlen <- nchar(x)
  result <- substring(x, seq_len(xlen), seq_len(xlen)+k-1)
  result[nchar(result)==k]
}

