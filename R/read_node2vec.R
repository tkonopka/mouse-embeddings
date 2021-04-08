# read a node2vec embedding


#' read a node2vec embedding from a set of files
#'
#' @param node_file character, path to a file with node names
#' @param embedding_file character, path to a file with embedding coordinates
#'
#' @return table with node ids and embedding coordinates
read_node2vec <- function(node_file, embedding_file) {
  nodes <- fread(node_file)
  setnames(nodes, c("V1", "V2"), c("index", "id"))
  emb <- readLines(embedding_file)[-1]
  emb <- strsplit(emb, split=" ")
  emb <- rbindlist(lapply(emb, function(x) {
    data.table(matrix(as.numeric(x), ncol=length(x)))
  }))
  emb_colnames <- paste0("node2vec_", seq(1, ncol(emb)-1))
  colnames(emb) <- c("index", emb_colnames)
  result <- merge(nodes, emb, by="index")
  result$index <- NULL
  setcolorder(result, c("id", emb_colnames))
  result
}

