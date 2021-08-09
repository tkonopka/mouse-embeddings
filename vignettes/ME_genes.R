# processing centered around genes


# ############################################################################
# using knn to find gene neighbors (model-to-model)

if (!assignc("model_knn_genes")) {
  assignc("model_knn")
  # identify which models link to the same gene at different k
  model_knn_genes <- lapply(model_knn, function(x) {
    k <- ncol(x$indexes)
    ids_genes <- merge(data.table(index=x$indexes[,1],
                                  model_id = rownames(x$indexes)),
                       model_info[, c("model_id", "marker_id")],
                       by="model_id", all.x=TRUE)
    ids_genes <- ids_genes[order(index)]
    stopifnot(identical(ids_genes$model_id, rownames(x$indexes)))
    markers <- ids_genes$marker_id
    result <- matrix(markers[x$indexes], ncol=k)
    rownames(result) <- rownames(x$indexes)
    colnames(result) <- paste0("k", seq_len(k))
    result_self <- matrix(as.integer(result == result[,1]), ncol=k)
    rownames(result_self) <- rownames(x$indexes)
    colnames(result_self) <- colnames(result)
    linked_to_self <- function(z) { as.integer(cumsum(z)>1) }
    result_cumself <- t(apply(result_self, 1, linked_to_self))
    rownames(result_cumself) <- rownames(x$indexes)
    colnames(result_cumself) <- colnames(result)
    list(markers=result, self=result_self, cumself=result_cumself)
  })
  savec(model_knn_genes)
}


if (!assignc("disease_knn_genes")) {
  rm(model_knn_genes)
  assignc("model_vectors_ids")
  assignc("disease_model_knn")
  assignc("disease_vectors_raw")
  gc()
  # get a subset of diseases associated with genes and phenotypes
  diseases_with_genes <- disease_info[disease_num_mgi_genes>0]$disease_id
  diseases_with_phen_genes <- intersect(diseases_with_genes,
                                        rownames(disease_vectors_raw$owlsim))
  # restrict to those diseases for which the genes have a mouse model
  diseases_with_phen_genes <-
    intersect(diseases_with_phen_genes,
              disease_genes[marker_id %in% model_info$marker_id]$id)
  # identify which diseases link to causative gene at different k
  link_markers_cumself <- function(x) {
    k <- ncol(x$indexes)
    indexes <- x$indexes
    missing.ids <- setdiff(diseases_with_phen_genes, rownames(indexes))
    if (length(missing.ids)) {
      x_missing <- matrix(NA, ncol=k, nrow=length(missing.ids))
      rownames(x_missing) <- missing.ids
      indexes <- rbind(indexes, x_missing)
    }
    indexes <- indexes[diseases_with_phen_genes, ]
    ids_genes <- merge(data.table(index=seq_along(model_vectors_ids),
                                  model_id=model_vectors_ids),
                       model_info[, c("model_id", "marker_id")],
                       by="model_id", all.x=TRUE)
    ids_genes <- ids_genes[order(index)]
    stopifnot(identical(ids_genes$model_id, model_vectors_ids))
    markers <- ids_genes$marker_id
    result <- matrix(markers[indexes], ncol=k)
    rownames(result) <- rownames(indexes)
    colnames(result) <- paste0("k", seq_len(k))
    result_self <- matrix(0, ncol=k, nrow=nrow(indexes))
    rownames(result_self) <- rownames(indexes)
    colnames(result_self) <- colnames(result)
    expected_genes <- split(disease_genes$marker_id, disease_genes$id)
    for (i in seq_len(nrow(result_self))) {
      i_expected <- expected_genes[rownames(result_self)[i]]
      result_self[i,] <- as.integer(result[i, ] %in% i_expected)
    }
    linked_to_self <- function(z) { as.integer(cumsum(z)>1) }
    result_cumself <- t(apply(result_self, 1, linked_to_self))
    rownames(result_cumself) <- rownames(indexes)
    colnames(result_cumself) <- colnames(result)
    list(markers=result, cumself=result_cumself)
  }
  disease_knn_genes <- lapply(disease_model_knn, function(xlist) {
    lapply(xlist, link_markers_cumself)
  })
  # add elements from alternative scoring methods
  disease_knn_genes$owlsim$phenodigm <-
    link_markers_cumself(disease_model_alt_knn$owlsim$phenodigm)
  disease_knn_genes$owlsim$phenoscoring <-
    link_markers_cumself(disease_model_alt_knn$owlsim$phenoscoring)
  disease_knn_genes$crossmap$phenoscoring <-
    link_markers_cumself(disease_model_alt_knn$crossmap$phenoscoring)
  savec(disease_knn_genes)
}


# ############################################################################
# summarizing knn links to disease genes

if (!exists("disease_knn_genes_summary")) {
  make_disease_knn_summmary <- function() {
    result <- expand.grid(list(translation=names(disease_model_knn),
                               encoding=c(names(disease_model_knn[[1]]),
                                          "phenodigm", "phenoscoring")),
                          stringsAsFactors=FALSE)
    result$gene_hit <- as.numeric(NA)
    for (i in seq_len(nrow(result))) {
      i_translation <- result$translation[i]
      i_encoding <- result$encoding[i]
      i_data <- disease_knn_genes[[i_translation]][[i_encoding]]$cumself
      if (!is(i_data, "NULL")) {
        result$gene_hit[i] <- mean(i_data[, ncol(i_data)], na.rm=TRUE)
      }
    }
    result <- data.table(result)
    result[!is.na(gene_hit)]
  }
  disease_knn_genes_summary <- make_disease_knn_summmary()
}

