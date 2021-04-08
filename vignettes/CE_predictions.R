# making predictions, evaluating prediction errors



# ############################################################################
# embeddings based on mouse model MP ontology vectors


if (!exists("models_w_phen")) {
  stop("'models_w_phen' not defined")
}


#' turn a data table into a matrix
#'
#' @param emb data table with an id column, coordinate columns
#'
#' @return numeric matrix
embdt2matrix <- function(emb) {
  ids <- emb$id
  coordinates_columns <- grep("UMAP|PCA|AVG", colnames(emb), value=TRUE)
  result <- as.matrix(emb[, coordinates_columns, with=FALSE])
  rownames(result) <- ids
  result
}


if (!assignc("model_prediction_stats")) {
  # load objects from cache (computed in CE_embeddings)
  assignc("model_vector_umap")
  assignc("model_knn")
  err_from_knn <- prediction_errors_from_knn
  # compute errors using direct knn neighbors
  comp_errors_from_knn <- function(ENCODING, raw_data=model_vector_umap$data) {
    outfile_knn <- glue(templates$prediction_errors,
                        WHAT="models", ENCODING=ENCODING, ERR="knn", DIM=2)
    if (!file.exists(outfile_knn)) {
      print(paste(date(), "computing errors (knn):", ENCODING))
      knn <- model_knn[[ENCODING]]
      if (identical(knn, NULL)) {
        knn <- model_vector_umap$knn
      }
      result <- err_from_knn(knn, raw_data[rownames(knn$indexes), ], mc.cores=3)
      fwrite(result, file=outfile_knn, sep="\t")
      rm(result, knn)
      gc()
    }
    outfile_knn
  }
  lapply(names(model_umap_embedding), comp_errors_from_knn)
  # compute errors using the embedding (various embedding methods)
  comp_errors_from_emb <- function(ENCODING,
                                   emblist=model_umap_embedding,
                                   raw_data=model_vector_umap$data) {
    outfile_emb <- glue(templates$prediction_errors,
                        WHAT="models", ENCODING=ENCODING, ERR="umap", DIM=2)
    if (!file.exists(outfile_emb)) {
      print(paste(date(), "computing errors (emb):", ENCODING))
      emb <- embdt2matrix(emblist[[ENCODING]][label=="model"])
      emb_knn <- umap(emb, config=knn.config)$knn
      result <- err_from_knn(emb_knn, raw_data[rownames(emb), ], mc.cores=3)
      fwrite(result, file=outfile_emb, sep="\t")
      rm(result, emb, emb_knn)
      gc()
    }
    outfile_emb
  }
  lapply(names(model_umap_embedding), comp_errors_from_emb)
  # compute errors using the embedding (in various dimensions)
  comp_errors_from_emb_d <- function(d, emblist,
                                     raw_data=model_vector_umap$data,
                                     ENCODING="vector", ERR="umap") {
    outfile_emb <- glue(templates$prediction_errors,
                        WHAT="models", ENCODING=ENCODING, ERR=ERR, DIM=d)
    if (!file.exists(outfile_emb)) {
      print(paste(date(), "computing errors (emb) d:", d))
      dstr <- paste0("d", d)
      emb <- embdt2matrix(emblist[[ENCODING]][[dstr]][label=="model"])
      emb_knn <- umap(emb, config=knn.config)$knn
      result <- err_from_knn(emb_knn, raw_data[rownames(emb), ], mc.cores=3)
      fwrite(result, file=outfile_emb, sep="\t")
      rm(result, emb, emb_knn)
      gc()
    }
    outfile_emb
  }
  lapply(embedding_d, comp_errors_from_emb_d,
         emblist=model_umap_embeddings_d, ENCODING="vector", ERR="umap")
  lapply(embedding_d, comp_errors_from_emb_d,
         emblist=model_umap_embeddings_d, ENCODING="binvector", ERR="umap")
  lapply(embedding_d, comp_errors_from_emb_d,
         emblist=model_pca_embeddings_d, ENCODING="vector", ERR="pca")
  lapply(embedding_d, comp_errors_from_emb_d,
         emblist=model_pca_embeddings_d, ENCODING="binvector", ERR="pca")
  lapply(embedding_d, comp_errors_from_emb_d,
         emblist=model_avg_embeddings_d, ENCODING="mp", ERR="avg")

  # load summaries of errors and compute summaries/stats
  summarize_prediction_stats <- function(model_ids) {
    encodings <- c("vector", "binvector", names(text_methods), "mp")
    result <- lapply(encodings, function(ENCODING) {
      comp_err_stats <- function(ERR="knn", DIM=2) {
        err_file <- glue(templates$prediction_errors,
                         WHAT="models", ENCODING=ENCODING, ERR=ERR, DIM=DIM)
        if (!file.exists(err_file)) return(NULL)
        err_data <- fread(err_file)
        err_data <- err_data[id %in% model_ids]
        summarize_quantiles_mean(err_data, "error", by="k")
      }
      comp_err_stats_d <- function(ERR="knn") {
        result <- list()
        for (d in embedding_d) {
          result[[paste0("d", d)]] <- comp_err_stats(ERR=ERR, DIM=d)
        }
        result
      }
      list(knn=comp_err_stats(ERR="knn"),
           umap=comp_err_stats_d(ERR="umap"),
           pca=comp_err_stats_d(ERR="pca"),
           avg=comp_err_stats_d(ERR="avg"))
    })
    names(result) <- encodings
    result
  }
  model_prediction_stats <- list(
    phen_gt_0=summarize_prediction_stats(models_w_phen$gt_0),
    phen_gt_1=summarize_prediction_stats(models_w_phen$gt_1)
  )
  savec(model_prediction_stats)
}


#
if (!exists("model_prediction_best")) {
  to_long <- function(x, label.col="label") {
    if (is(x, "data.frame")) {
      return(x)
    }
    if (is(x, "NULL")) {
      return(NULL)
    }
    if (is(x, "list") & length(x)==0) {
      return(NULL)
    }
    if (is(x, "list")) {
      result <- vector("list", length(x))
      for (i in seq_along(x)) {
        result_i <- to_long(x[[i]], label.col)
        if (!is.null(result_i)) {
          result[[i]] <- result_i
          new_labels <- paste0(names(x)[i], "_", result[[i]][[label.col]])
          result[[i]][[label.col]] <- gsub("_$", "", new_labels)
        }
      }
      return(rbindlist(result))
    }
    stop(paste0("unsupported object type: ", class(x)))
  }
  get_best_prediction_stats <- function(x) {
    result <- to_long(x, label.col="approach")
    result <- result[, list(best_k=k[which.min(error_mean)],
                            best_error=min(error_mean)),
                       by="approach"]
    result$neighbors <- "knn"
    result$neighbors[grepl("umap", result$approach)] <- "umap"
    result$neighbors[grepl("pca", result$approach)] <- "pca"
    result$neighbors[grepl("avg", result$approach)] <- "avg"
    result$data <- "vector"
    result$data[grepl("binvector", result$approach)] <- "binary vector"
    result$data[grepl("mp", result$approach)] <- "mp terms"
    result$data[grepl("text_concise", result$approach)] <- "text, concise"
    result$data[grepl("text_complete", result$approach)] <- "text, complete"
    result
  }
  model_prediction_best <- lapply(model_prediction_stats,
                                  get_best_prediction_stats)
}

