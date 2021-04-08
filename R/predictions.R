# functions for generating predictions from neighbors and computing errors


#' compute euclidean distance between two vectors
#'
#' @param a numeric vector
#' @param b numeric vector
#'
#' @return numeric distance
d_euclidean <- function(a, b) {
  sqrt(sum((a-b)*(a-b)))
}


#' compute prediction error by averageing nearest neighbors
#'
#' @param knn object of class umap.knn
#' @param data matrix of original data
#' @param err.fun function, computes an error (distance) between true data
#' point and its prediction
#' @param normalize function, processes neighbor averges before computing error
#' @param mc.cores integer, number of cores for mclapply
#'
#' @return data table with id, k (umap-k minus 1), prediction error
prediction_errors_from_knn <- function(knn, data, err.fun=d_euclidean,
                                       mc.cores=2, normalize=normalize_by_row) {
  # ensure structure of umap object (allows for shortcuts in code)
  if (!is(knn, "umap.knn"))
    stop("input must be a umap.knn object")
  if (!identical(rownames(knn$indexes), rownames(knn$distances)))
    stop("knn object must have consistent names")
  if (!identical(rownames(knn$indexes), rownames(data)))
    stop("knn and data object must have consistent names")

  # set constants
  k <- ncol(knn$indexes)
  kvec <- seq(1, k-1)
  n <- nrow(data)
  knn_indexes_t <- t(knn$indexes[, seq(2, k)])

  # for each item in the dataset, make a series of averages over k=2, 3, ...
  # and report distances between the true vector and that new average
  result <- mclapply(as.list(seq_len(n)), function(index) {
    vec <- data[index,]
    neighbor_mat <- data[knn_indexes_t[, index], ]
    neighbor_avg <- apply(neighbor_mat, 2, cumsum) / kvec
    neighbor_avg <- normalize(neighbor_avg)
    errors <- apply(neighbor_avg, 1, err.fun, vec)
    data.table(index=index, k=kvec, error=errors)
  }, mc.cores=mc.cores)
  result <- rbindlist(result)
  if (identical(rownames(knn$indexes), NULL)) {
    result$id <- result$index
  } else {
    result$id <- rownames(knn$indexes)[result$index]
  }
  result$index <- NULL
  setcolorder(result, c("id", "k", "error"))
  result
}


#' summarize a numeric column in a table by quantiles and with a mean
#'
#' @param x data table
#' @param value_col character, column name in x
#' @param by character vector, for grouping
#'
#' @return data table with by columns, columns with quantile, and mean
summarize_quantiles_mean <- function(x, value_col="error", by="k") {
  p_levels <- c(0.05, 0.25, 0.5, 0.75, 0.95)
  q_mean_names <- c(paste0("q", c("05", 25, 50, 75, 95)), "mean")
  q_mean <- function(z) {
    result_q <- quantile(z, p=p_levels, na.rm=TRUE)
    result_mean <- mean(z, na.rm=TRUE)
    setNames(as.list(c(result_q, result_mean)), q_mean_names)
  }
  result <- x[, q_mean(.SD[[value_col]]), by=by]
  setnames(result, q_mean_names, paste0(value_col, "_", q_mean_names))
  result
}

