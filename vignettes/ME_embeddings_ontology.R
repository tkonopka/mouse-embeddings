# embeddings for ontology terms


# ############################################################################
# make ontology embeddings

canary_file <- glue(templates$mp_embedding,
                    DIM=max(embedding_d), ALGO="umap", SETTINGS="R1")
if (!file.exists(canary_file)) {
  source("ME_prep_knn.R")
  make_ontology_embedding <- function(d, template=templates$mp_embedding,
                                config=embedding.config,
                                knn=mp_knn, label="mp", replicate=0) {
    outfile <- glue(template, DIM=d,
                    ALGO="umap", SETTINGS=paste0("R", replicate))
    if (file.exists(outfile)) return(NULL)
    dummy_data <- matrix(0, ncol=1, nrow=nrow(knn$indexes))
    rownames(dummy_data) <- rownames(knn$indexes)
    result <- umap(dummy_data, config=config,
                   knn=knn, n_components=d,
                   random_state=config$random_state+replicate)
    write_embedding(result, file=outfile, label=label)
  }
  sapply(embedding_d, make_ontology_embedding, replicate=0)
  sapply(embedding_d, make_ontology_embedding, replicate=1)
  sapply(2, make_ontology_embedding, template=templates$hp_embedding,
         knn=hp_knn, label="hp", replicate=0)
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
if (!assignc("hp_embedding")) {
  hp_embedding <- fread(glue(templates$hp_embedding, DIM=2,
                             ALGO="umap", SETTINGS="R0"))
  hp_embedding <- list(d2_R0=rbind(
    hp_embedding,
    predict_avg_coordinates(hp_embedding,
                            disease_phenotypes_hp,
                            coordinates=paste0("UMAP_", c(1, 2)),
                            label="disease")
  ))
  savec(hp_embedding)
}

gc()

