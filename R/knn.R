# functions dealing with umap.knn
#


#' get a vector of indexes that are connected to an index with distance 0
#'
#' Some umap.knn objects link objects always to the same neighbors, e.g. link
#' X to (a,b,c), Y to (a,b,c), Z to (a,b,c), and so on, with all distances zero.
#' The fact that X and Y and Z also have distance zero to each other is lost.
#' Linking always to the same neighbors (here a, b, c) can create distortions
#' in the umap layout algorithm. In those situations, it isgood to randomize
#' the neighbors assigned to A, B, a, b, c. This function identifies all
#' degenerate neighbors, i.e. links A to (B,a,b,c)
#'
#' @param obj object of class umap.knn
#' @param index integer of an item of interest
#'
#' @return vector of indexes that are all connected to 'index'
get_degenerate_cluster <- function(obj, index) {
  previous <- c()
  result <- sort(index)
  n <- ncol(obj$indexes)
  n_rows <- nrow(obj$indexes)
  zero_d <- data.table(from=rep(seq_len(n_rows), n),
                     to=as.integer(obj$indexes),
                     distance=as.numeric(obj$distances))[distance==0]
  while (!identical(previous, result)) {
    previous <- result
    result <- c(zero_d[from %in% result]$to, zero_d[to%in% result]$from)
    result <- sort(unique(result))
  }
  result
}


#' modify a umap.knn object by detecting degenerate clusters and
#' making sure that neighbors are randomly linked together
#'
#' A degenerate index is one that has distance 0 to all its neighbors.
#' A degenerate cluster is a set of indexes that all have distance 0 to
#' one another.
#'
#' @param obj object of class umap.knn
#' @param index integer, index of item with a degenerate cluster
#'  (end-users should leave this as NULL to process the entire dataset.
#'  The function itself uses this parameter internally)
#'
#' @return modified object of class umap.knn
randomize_degenerate_umap_knn <- function(obj, index=NULL) {
  # detect rows that have all neighbors with d=0
  if (is.null(index)) {
    degenerate <- as.integer(which(apply(obj$distance, 1, sum)==0))
    while (length(degenerate)>0) {
      group <- get_degenerate_cluster(obj, degenerate[1])
      obj <- randomize_degenerate_umap_knn(obj, degenerate[1])
      degenerate <- setdiff(degenerate, group)
    }
    return(obj)
  }
  # detect indexes within the degenerate cluster
  if (sum(obj$distance[index,])>0) {
    stop("index is not part of a degenerate cluster")
  }
  connected <- get_degenerate_cluster(obj, index)
  n <- ncol(obj$indexes)
  for (i in connected) {
    new_neighbors <- sample(connected, n, replace=FALSE)
    obj$indexes[i, ] <- c(i, setdiff(new_neighbors, i)[1:(n-1)])
  }
  obj
}


#' prepare a umap.knn object from a long-form table of knn
#'
#' @param d data table
#' @param n integer number of neighbors
#' @param columns character vector with three column names in d
#' @param levels vector of row names
#'
#' @return object of class umap.knn
umap_knn_from_long <- function(d, n=15,
                               columns=c("query", "target", "distance"),
                               levels=NULL) {
  knn <- data.table(copy(d))
  setnames(knn, columns, c("query", "target", "distance"))
  # ensure that each query maps to self with distance = 0 and at rank 1
  knn_items <- unique(knn$query)
  knn_self <- data.table(query=knn_items, target=knn_items, distance=-1.0)
  knn <- rbind(knn, knn_self)[!(query==target & distance>=0)]
  knn <- knn[order(distance)]
  # get exactly n nearest neighbors for each query, impute if necessary
  topN <- function(x, n) {
    if (nrow(x)>=n) return(head(x, n))
    other <- setdiff(knn_items, x$target)
    x.other <- data.table(target=sample(other, n, replace=FALSE), distance=1)
    head(rbind(x, x.other), n)
  }
  knn <- knn[, topN(.SD, n), by="query"]
  knn[distance<0, "distance"] <- 0
  # create matrices with indexes and distances
  knn$from <- match(knn$query, knn_items)
  knn_levels <- knn_items
  if (!is.null(levels)) {
    knn_levels <- levels
  }
  knn$to <- match(knn$target, knn_levels)
  knn <- knn[, list(to=to,
                    distance=distance,
                    rank=paste0("r_", seq_along(to))),
               by="from"]
  make.matrix <- function(x, value.var) {
    result <- dcast(x, from~rank, value.var=value.var)[order(from)]
    rank_cols <- paste0("r_", seq_len(n))
    result <- as.matrix(result)[, rank_cols]
    rownames(result) <- knn_items
    result
  }
  knn_indexes <- make.matrix(knn, value.var="to")
  knn_distances <- make.matrix(knn, value.var="distance")
  umap.knn(knn_indexes, knn_distances)
}


#' compute knn by brute force
#'
#' @param query_data numeric matrix with query vectors
#' @param target_data numeric matrix of data
#' @param config umap.config object
#'
#' @return object of class umap.knn but with items from query_data
#' linking to items in target_data
umap_knn_bruteforce <-function(query_data, target_data, config) {
  stopifnot(is(query_data, "matrix"))
  stopifnot(is(target_data, "matrix"))
  stopifnot(ncol(query_data) == ncol(target_data))
  print(date())
  k <- config$n_neighbors
  metric.fun <- config$metric.fun
  alldata <- t(rbind(target_data, query_data))
  distances <- matrix(0.0, ncol=nrow(query_data), nrow=k-1)
  indexes <- matrix(as.integer(0), ncol=nrow(query_data), nrow=k-1)
  offset <- nrow(target_data)
  to.offset <- seq_len(offset)
  print(date())
  for (i in seq_len(nrow(query_data))) {
    if (i %% 500 ==0 ) {
      print(paste0(date(), " ", i))
    }
    idist <- metric.fun(alldata, offset+i, to.offset)
    hits <- head(order(idist), k-1)
    indexes[,i] <- as.integer(hits)
    distances[,i] <- idist[hits]
  }
  indexes <- cbind(NA, t(indexes))
  distances <- cbind(0.0, t(distances))
  rownames(indexes) <- rownames(distances) <- rownames(query_data)
  umap.knn(indexes, distances)
}


#' similar to umap_knn_bruteforce, but using approximate nearest neighbors
#'
#' @param query_data numeric matrix with query vectors
#' @param target_data numeric matrix of data
#' @param config umap.config object
#'
#' @return object of class umap.knn but with items from query
#' data linking to items in target_data
umap_knn_approx <-function(query_data, target_data, config) {
  stopifnot(is(query_data, "matrix"))
  stopifnot(is(target_data, "matrix"))
  stopifnot(ncol(query_data) == ncol(target_data))
  # this uses a package-private function... not best practice but
  # saves computing time for large matrices
  result <- umap:::spectator.knn.info(query_data, target_data, config)
  result$indexes[,1] <- NA
  result
}
