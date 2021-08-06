# comparison of embeddings


#' jaccard index of two sets
#'
#' @param a vector
#' @param b vector
#'
#' @return numeric, jaccard index of two sets
ji <- function(a, b) {
  length(intersect(a, b)) / length(union(a, b))
}


#' compute mean Jaccard index of two knn objects
#'
#' @param k1 object of class umap.knn
#' @param k2 object of class umap.knn compatible with k1
#'
#' @return vector with jaccard indexes for all data items
paired_ji <- function(k1, k2) {
  indexes_1 <- t(k1$indexes[, 2:ncol(k1$indexes)])
  indexes_2 <- t(k2$indexes[, 2:ncol(k2$indexes)])
  stopifnot(identical(rownames(indexes_1), rownames(indexes_2)))
  result <- rep(0, nrow(k1$indexes))
  for (j in seq_along(result)) {
    result[j] <- ji(indexes_1[, j], indexes_2[, j])
  }
  result
}


#' make a comparison of embeddings based on jaccard-index of neighbors
#'
#' @param emblist list of 2d coordinates
make_embedding_comparison <- function(emblist, ids, n_neighbors=NA) {
  emblist <- lapply(emblist, function(x) {
    if (!is(x, "matrix")) {
      coords <- grep("UMAP_|node2vec_|PCA_", colnames(x), value=TRUE)
      result <- as.matrix(x[, coords, with=FALSE])
      rownames(result) <- x$id
    } else {
      result <- x
    }
    result[ids, ]
  })
  knn.list <- lapply(emblist, function(x) {
    result <- umap(x, config=knn.config,
                   n_neighbors=ifelse(is.na(n_neighbors),
                                      knn.config$n_neighbors,
                                      n_neighbors))
    result$knn
  })
  result <- matrix(0, ncol=length(emblist), nrow=length(emblist),
                   dimnames=list(names(emblist), names(emblist)))
  for (i in seq(1, length(emblist))) {
    for (j in seq(1, length(emblist))) {
      if (j>=i) {
        ij_comparison <- paired_ji(knn.list[[i]], knn.list[[j]])
        result[j,i] <- result[i,j] <- mean(ij_comparison)
      }
    }
  }
  result
}


if (!assignc("mp_embedding_comparison")) {
  make_mp_embedding_comparison <- function() {
    ids <- mp_info$names$id
    random_emb <- data.table(id=ids,
                             UMAP_1=runif(length(ids), -10, 10),
                             UMAP_2=runif(length(ids), -10, 10))
    emblist <- c(
      node2vec_embedding$mp_ontology,
      list(
        text_R0=mp_embedding$d2_R0,
        text_R1=mp_embedding$d2_R1,
        random=random_emb
      ))
    make_embedding_comparison(emblist, ids=ids)
  }
  mp_embedding_comparison <- make_mp_embedding_comparison()
  savec(mp_embedding_comparison)
}


if (!assignc("model_embedding_comparison")) {
  make_model_embedding_comparison <- function() {
    ids <- model_info[num_phenotypes>1]$model_id
    random_emb <- data.table(id=ids,
                             UMAP_1=runif(length(ids), -10, 10),
                             UMAP_2=runif(length(ids), -10, 10))
    emblist <- c(
      model_umap_embedding,
      node2vec_embedding$mouse_model_concise,
      list(
        pca=model_pca_embeddings_d$vector$d2,
        random=random_emb
      ))
    make_embedding_comparison(emblist, ids=ids)
  }
  model_embedding_comparison <- make_model_embedding_comparison()
  savec(model_embedding_comparison)
}

