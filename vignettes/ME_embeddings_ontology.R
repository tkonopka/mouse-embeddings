# embeddings for ontology terms


# ############################################################################
# make ontology embeddings

canary_file <- glue(templates$mp_embedding,
                    DIM=max(embedding_d), ALGO="umap")
if (!file.exists(canary_file)) {
  source("ME_prep_knn.R")
  make_mp_embedding <- function(d) {
    outfile <- glue(templates$mp_embedding, DIM=d, ALGO="umap")
    if (!file.exists(outfile)) {
      knn <- mp_knn
      dummy_data <- matrix(0, ncol=1, nrow=nrow(knn$indexes))
      rownames(dummy_data) <- rownames(knn$indexes)
      result <- umap(dummy_data, config=embedding.config,
                     knn=knn, n_components=d)
      write_embedding(result, file=outfile, label="mp")
    }
  }
  sapply(embedding_d, make_mp_embedding)
}
if (!assignc("mp_embedding")) {
  read_predict_mp_embedding <- function(d) {
    d_coordinates <- paste0("UMAP_", seq_len(d))
    mp_embedding <- fread(glue(templates$mp_embedding, DIM=d, ALGO="umap"))
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
  mp_embedding <- lapply(embedding_d, read_predict_mp_embedding)
  names(mp_embedding) <- paste0("d", embedding_d)
  savec(mp_embedding)
}

gc()

