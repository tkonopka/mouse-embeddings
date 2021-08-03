# embeddings for ontology terms


# ############################################################################
# make ontology embeddings

canary_file <- glue(templates$mp_embedding,
                    DIM=max(embedding_d), ALGO="umap", SETTINGS="R1")
if (!file.exists(canary_file)) {
  source("ME_prep_knn.R")
  make_mp_embedding <- function(d, config=embedding.config, replicate=0) {
    outfile <- glue(templates$mp_embedding, DIM=d,
                    ALGO="umap", SETTINGS=paste0("R", replicate))
    if (!file.exists(outfile)) {
      knn <- mp_knn
      dummy_data <- matrix(0, ncol=1, nrow=nrow(knn$indexes))
      rownames(dummy_data) <- rownames(knn$indexes)
      result <- umap(dummy_data, config=config,
                     knn=knn, n_components=d,
                     random_state=config$random_state+replicate)
      write_embedding(result, file=outfile, label="mp")
    }
  }
  sapply(embedding_d, make_mp_embedding, replicate=0)
  sapply(embedding_d, make_mp_embedding, replicate=1)
}
if (!assignc("mp_embedding")) {
  #' create data frames with coordinates for mp terms, models, diseases
  #'
  #' @param x list with $d and $settings
  read_predict_mp_embedding <- function(x) {
    d <- x$d
    d_coordinates <- paste0("UMAP_", seq_len(d))
    mp_embedding <- fread(glue(templates$mp_embedding, DIM=d,
                               ALGO="umap", SETTINGS=x$settings))
    mp_disease_embedding <- rbindlist(lapply(
      translation_methods,
      function(translation_method) {
        predict_avg_coordinates(mp_embedding,
                                disease_phenotypes[[translation_method]],
                                coordinates=d_coordinates,
                                label=translation_method)
      }))
    mp_model_embedding <- predict_avg_coordinates(mp_embedding,
                                                  model_phenotypes$concise,
                                                  coordinates=d_coordinates,
                                                  label="model")
    rbind(mp_embedding, mp_disease_embedding, mp_model_embedding)
  }
  .configs <- expand.grid(list(d=embedding_d, settings=c("R0", "R1")))
  .configs <- split(.configs, paste0("d", .configs$d, "_", .configs$settings))
  mp_embedding <- lapply(.configs, read_predict_mp_embedding)
  savec(mp_embedding)
  rm(.configs)
}

gc()

