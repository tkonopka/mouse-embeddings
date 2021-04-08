# functions dealing with pca


#' normalize an embedding by making sure coordinate 1 has a specified width
#'
#' @param data matrix with embedding coordinates
#' @param width numeric, target width of first component. Use 20 to roughly match
#' UMAP results
#'
#' @return matrix of same dimensions as input data
normalize_embedding <- function(data, width=40) {
  n <- nrow(data)
  result <- data - matrix(rep(colSums(data)/n, each=n), nrow=n)
  width*result/ ( max(result[,1])-min(result[,1]))
}


#' compute an PCA (svd) embedding for a data matrix
#'
#' @param data numeric matrix, items in rows, features in columns
#' @param n_components integer, number of principal components
#' @param max_features integer, used to truncate data to smaller number of
#' features before running svd
#'
#' @return matrix with as many rows as data, wth n_components columns
svd_embedding <- function(data, n_components=4, max_features=4000) {
  stopifnot(is(data, "matrix"))
  if (ncol(data)>max_features) {
    data_sd <- apply(data, 2, sd)
    variable_features <- head(order(-data_sd), max_features)
    data <- data[, variable_features]
  }
  result <- svd(scale(t(data), center=TRUE, scale=FALSE), 0, n_components)$v
  rownames(result) <- rownames(data)
  colnames(result) <- paste0("PCA_", seq_len(n_components))
  normalize_embedding(result)
}

