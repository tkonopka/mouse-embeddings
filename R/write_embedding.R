# write coordinates for a umap embedding into a file
#


#' write an embedding to a table, adding some extra columns of metadata
#'
#' @param x object of class umap
#' @param path character, path to output file
#' @param label character, label that can appear in output file
#' @param col_prefix character, prefix for column names with embedding
#' coordinates
#'
#' @return the table written to file
write_embedding <- function(x, file="umap.tsv.gz", label="label",
                            col_prefix="UMAP_") {
  result <- x
  if (is(x, "umap")) {
    result <- x$layout
  }
  colnames(result) <- paste0(col_prefix, seq_len(ncol(result)))
  .rownames <- rownames(result)
  result <- data.table(result)
  if (is.null(.rownames)) {
    result$id <- paste0("item", seq_len(nrow(result)))
  } else {
    result$id <- .rownames
  }
  result$label <- label
  setcolorder(result, c("label", "id"))
  fwrite(result, file=file, sep="\t")
  NULL
}

