# embeddings for mouse models
# (includes analysis of diseases projected into the embeddings of models)

#
# ! Some of these steps require a lot of memory. Recommended 64 GB.
#
# It may help not to run all the vignette from scratch.
# Instead, run previous parts to allow caching. Then quit R, run previous
# parts again (this will utilize cached objects and use less memory in practice)
#
# It may help to run parts of this script in stages as well.
#
# After all cache files are generated, re-running this script
# as part of vignette generation is very fast and uses little memory (< 8GB)
#


if (!assignc("model_vectors_ids")) {
  source("ME_prep_repr.R")
}


# ############################################################################
# embeddings based on mouse model MP ontology vectors

canary_file <- glue(templates$model_embedding,
                    WHAT="models", ENCODING="binvector", DIM=2,
                    ALGO="umap", SETTINGS="R1")
if (!file.exists(canary_file)) {
  # ensure that raw data is loaded
  source("ME_prep_repr.R")
  # create an embedding based on nonbinary vector representations
  # (create cache object "model_vector_umap" as side effect)
  make_model_vector_umap <- function(replicate=0) {
    output_file <- glue(templates$model_embedding,
                        WHAT="models", ENCODING="vector",
                        DIM=2, ALGO="umap", SETTINGS=paste0("R", replicate))
    if (file.exists(output_file)) return(NULL)
    config <- embedding.config
    model_vector_umap <- umap(model_vectors, config=config,
                              random_state=config$random_state+replicate)
    if (replicate==0) {
      savec(model_vector_umap)
    }
    write_embedding(model_vector_umap, file=output_file, label="model")
  }
  make_model_vector_umap(replicate=0)
  make_model_vector_umap(replicate=1)
  gc()
  # create embeddings based on binary vectors
  # (create cache object "model_vector_umap" as side effect)
  make_model_binvector_umap <- function(replicate=0) {
    output_file <- glue(templates$model_embedding,
                        WHAT="models", ENCODING="binvector",
                        DIM=2, ALGO="umap", SETTINGS=paste0("R", replicate))
    if (file.exists(output_file)) return(NULL)
    config <- embedding.config
    model_binvector_umap <- umap(model_binvectors, config=config,
                                 random_state=config$random_state+replicate)
    if (replicate==0) {
      savec(model_binvector_umap)
    }
    write_embedding(model_binvector_umap, file=output_file, label="model")
  }
  make_model_binvector_umap(replicate=0)
  make_model_binvector_umap(replicate=1)
  gc()
}


# ############################################################################
# project diseases into an embedding of models

canary_file <- glue(templates$model_embedding,
                    WHAT="diseases-owlsim", ENCODING="vector",
                    DIM=2, ALGO="umap", SETTINGS="R0")
if (!file.exists(canary_file)) {
  source("ME_prep_repr.R")
  assignc("model_vector_umap")
  sapply(names(disease_vectors), function(translation_type) {
    output_file <- glue(templates$model_embedding,
                        WHAT=paste0("diseases-", translation_type),
                        ENCODING="vector",
                        DIM=2, ALGO="umap", SETTINGS="R0")
    if (!file.exists(output_file)) {
      result <- predict(model_vector_umap, disease_vectors[[translation_type]])
      write_embedding(result, file=output_file, label=translation_type)
    }
  })
  rm(model_vector_umap, disease_vectors)
  assignc("model_binvector_umap")
  sapply(names(disease_binvectors), function(translation_type) {
    output_file <- glue(templates$model_embedding,
                        WHAT=paste0("diseases-", translation_type),
                        ENCODING="binvector",
                        DIM=2, ALGO="umap", SETTINGS="R0")
    if (!file.exists(output_file)) {
      result <- predict(model_binvector_umap, disease_binvectors[[translation_type]])
      write_embedding(result, file=output_file, label=translation_type)
    }
  })
  rm(model_binvector_umap, disease_binvectors)
  gc()
}


# ############################################################################
# embeddings in higher dimensions

if (!assignc("model_knn")) {
  source("ME_prep_knn.R")
  # wrapper to prepare knn for models
  prep_model_knn <- function(repr, diff, n=15) {
    prep_umap_knn(template=templates$model_search,
                  repr=repr, diff=diff, n=n)
  }
  # prepare knn object for text-based methods (from crossmap)
  model_knn <-
    list(text_concise_diff0=prep_model_knn("concise", diff=0),
         text_complete_diff0=prep_model_knn("complete", diff=0))
  # transfer knn objects from umap results
  assignc("model_vector_umap")
  model_knn$vector <- model_vector_umap$knn
  rm(model_vector_umap)
  assignc("model_binvector_umap")
  model_knn$binvector <- model_binvector_umap$knn
  rm(model_binvector_umap)
  savec(model_knn)
  gc()
}


if (!assignc("model_umap_embeddings_d")) {
  make_model_umap_embedding_d <- function(d, knn, ENCODING="vector") {
    output_file <- glue(templates$model_embedding,
                        WHAT="models", ENCODING=ENCODING,
                        DIM=d, ALGO="umap", SETTINGS="R0")
    if (!file.exists(output_file)) {
      print(paste0("computing umap embedding in d=", d))
      dummy_data <- matrix(0, ncol=1, nrow=nrow(knn$indexes))
      rownames(dummy_data) <- rownames(knn$indexes)
      result <- umap(dummy_data, config=embedding.config,
                     n_components=d, knn=knn)
      write_embedding(result, file=output_file, label="model")
    }
    fread(output_file)
  }
  model_umap_embeddings_d <- list(
    vector=sapply(embedding_d, make_model_umap_embedding_d,
                  knn=model_knn$vector, ENCODING="vector"),
    binvector=sapply(embedding_d, make_model_umap_embedding_d,
                     knn=model_knn$binvector, ENCODING="binvector")
  )
  names(model_umap_embeddings_d$vector) <- paste0("d", embedding_d)
  names(model_umap_embeddings_d$binvector) <- paste0("d", embedding_d)
  savec(model_umap_embeddings_d)
  gc()
}


# ############################################################################
# embeddings using average coordinates in mp space

if (!assignc("model_avg_embeddings_d")) {
  make_model_avg_embeddings_d <- function() {
    print("computing embedding by averaging")
    result <- list()
    for (d in embedding_d) {
      output_file <- glue(templates$model_embedding,
                          WHAT="models", ENCODING="mp",
                          DIM=d, ALGO="avg", SETTINGS="R0")
      if (!file.exists(output_file)) {
        mp_embedding <- fread(glue(templates$mp_embedding,
                                   DIM=d, ALGO="umap", SETTINGS="R0"))
        d_coordinates <- paste0("UMAP_", seq_len(d))
        avg_coordinates <- paste0("AVG_", seq_len(d))
        avg_embedding <- predict_avg_coordinates(mp_embedding,
                                                 model_phenotypes$concise,
                                                 coordinates=d_coordinates,
                                                 label="model")
        setnames(avg_embedding, d_coordinates, avg_coordinates)
        fwrite(avg_embedding, file=output_file, sep="\t")
      }
      result[[paste0("d", d)]] <- fread(output_file)
    }
    result
  }
  model_avg_embeddings_d <- list(mp=make_model_avg_embeddings_d())
  savec(model_avg_embeddings_d)
}


# ############################################################################
# embeddings using PCA

if (!assignc("model_pca_embeddings_d")) {
  make_model_pca_embeddings_d <- function(data_vectors, ENCODING="vector") {
    print("computing pca/svd embedding")
    data_pca <- svd_embedding(data_vectors, n_components=max(embedding_d),
                              max_features=5000)
    result <- list()
    for (d in embedding_d) {
      output_file <- glue(templates$model_embedding,
                          WHAT="models", ENCODING=ENCODING,
                          DIM=d, ALGO="pca", SETTINGS="R0")
      if (!file.exists(output_file)) {
        write_embedding(data_pca[, seq_len(d)], file=output_file,
                        label="model", col_prefix="PCA_")
      }
      result[[paste0("d", d)]] <- fread(output_file)
    }
    result
  }
  model_pca_embeddings_d <- list()
  assignc("model_vector_umap")
  model_pca_embeddings_d$vector <-
    make_model_pca_embeddings_d(model_vector_umap$data, ENCODING="vector")
  rm(model_vector_umap)
  gc()
  assignc("model_binvector_umap")
  model_pca_embeddings_d$binvector <-
    make_model_pca_embeddings_d(model_binvector_umap$data, ENCODING="binvector")
  rm(model_binvector_umap)
  savec(model_pca_embeddings_d)
  gc()
}


# ############################################################################
# embeddings based on text descriptions

canary_file <- glue(templates$model_embedding,
                    WHAT="models", ENCODING=names(text_methods)[1],
                    DIM=2, ALGO="umap", SETTINGS="R1")
if (!file.exists(canary_file)) {
  mclapply(text_methods, function(encoding) {
    knn <- model_knn[[encoding]]
    dummy_data <- matrix(0, ncol=1, nrow=nrow(knn$indexes))
    rownames(dummy_data) <- rownames(knn$indexes)
    for (replicate in c(0, 1)) {
      knn_umap <- umap(dummy_data, config=embedding.config, knn=knn,
                       random_state=embedding.config$random_state+replicate)
      output_file <- glue(templates$model_embedding,
                          WHAT="models", ENCODING=encoding,
                          DIM=2, ALGO="umap", SETTINGS=paste0("R", replicate))
      write_embedding(knn_umap, file=output_file, label="model")
    }
  }, mc.cores=2)
}


# ############################################################################
# read already-made embeddings

#' read a set of embeddings based on models
read_embedding_set <- function(template,
                               ENCODING="vector", TRANSLATION="",
                               ALGO="umap", DIM=2, SETTINGS="R0") {
  result <- lapply(translation_methods, function(tt) {
    # this embedding file should hit an actual file for ENCODING="vector"
    # for other ENCODINGs, there will be no files with diseases
    embedding_file <- glue(template,
                           WHAT=paste0("diseases-", tt),
                           ENCODING=ENCODING, TRANSLATION=TRANSLATION,
                           ALGO=ALGO, DIM=DIM, SETTINGS=SETTINGS)
    if (!file.exists(embedding_file)) return(NULL)
    fread(embedding_file)
  })
  embedding_file <- glue(template, WHAT="models", ENCODING=ENCODING,
                         TRANSLATION=TRANSLATION, DIM=DIM,
                         ALGO=ALGO, SETTINGS=SETTINGS)
  result$models <- fread(embedding_file)
  rbindlist(result)
}
if (!exists("model_umap_embedding")) {
  .config <- expand.grid(encoding=names(encoding_methods),
                         settings=c("R0", "R1"))
  .config <- split(.config, paste0(.config$encoding, "_", .config$settings))
  model_umap_embedding <- lapply(.config, function(x) {
    read_embedding_set(template=templates$model_embedding,
                       ENCODING=x$encoding, DIM=2, SETTINGS=x$settings)
  })
  rm(.config)
}


# ############################################################################
# project diseases into text-based embeddings

read_disease_search_file <- function(template,
                                     REPR="concise", DIFF=0,
                                     DISEASE="orphanet", TRANSLATION="") {
  search_file <- glue(template,
                      REPR=REPR, DIFF=DIFF,
                      DISEASE=DISEASE, TRANSLATION=TRANSLATION)
  if (!file.exists(search_file)) return(NULL)
  fread(search_file)
}
if (!exists("model_disease_umap_embedding")) {
  # read search results (tables with diseases and nearest models)
  disease_search <- lapply(translation_methods, function(tm) {
    lapply(disease_text_methods, function(dtm) {
      tmpl <- templates$disease_search
      rdsf <- read_disease_search_file
      list(
        text_concise_diff0=rdsf(tmpl, DISEASE=dtm, TRANSL=tm, DIFF=0, REPR="concise"),
        text_complete_diff0=rdsf(tmpl, DISEASE=dtm, TRANSL=tm, DIFF=0, REPR="complete")
      )
    })
  })
  savec(disease_search)
  model_disease_umap_embedding <- lapply(text_methods, function(tm) {
    rbindlist(lapply(translation_methods, function(transl_method) {
      rbindlist(lapply(disease_text_methods, function(dtm) {
        search_result <- copy(disease_search[[transl_method]][[dtm]][[tm]])
        setnames(search_result, "query", "id")
        .emb <- model_umap_embedding[[paste0(tm, "_R0")]]
        predict_avg_coordinates(.emb, search_result,
                                feature_col="target",
                                label=paste0(dtm, "-", transl_method))
      }))
    }))
  })
  savec(model_disease_umap_embedding)
  rm(disease_search)
}


# find which models are closes to diseases
if (!assignc("disease_model_knn")) {
  assignc("model_vector_umap")
  assignc("model_binvector_umap")
  assignc("disease_search")
  assignc("disease_vectors")
  assignc("disease_binvectors")
  make_disease_model_knn <- function(tm) {
    print(paste0(date(), " ", tm))
    .text <- disease_search[[tm]]$orphanet
    ids <- rownames(model_vector_umap$data)
    print(paste0(date(), " computing from vector"))
    .vector <- umap_knn_approx(disease_vectors[[tm]],
                               model_vector_umap$data,
                               model_vector_umap$config)
    print(paste0(date(), " computing from binvector"))
    .binvector <- umap_knn_approx(disease_binvectors[[tm]],
                                  model_binvector_umap$data,
                                  model_binvector_umap$config)
    print(paste0(date(), " computing from search"))
    list(
      vector=.vector,
      binvector=.binvector,
      text_concise_diff0=umap_knn_from_long(.text$text_concise_diff0,
                                            levels=ids),
      text_complete_diff0=umap_knn_from_long(.text$text_complete_diff0,
                                             levels=ids)
    )
  }
  # this is written like this to cache some results on disk
  if (!assignc("disease_model_knn_owlsim")) {
    disease_model_knn_owlsim <- make_disease_model_knn("owlsim")
    savec(disease_model_knn_owlsim)
  }
  if (!assignc("disease_model_knn_crossmap")) {
    disease_model_knn_crossmap <- make_disease_model_knn("crossmap")
    savec(disease_model_knn_crossmap)
  }
  disease_model_knn <- list(
    owlsim=disease_model_knn_owlsim,
    crossmap=disease_model_knn_crossmap
  )
  savec(disease_model_knn)
  rm(model_vector_umap, model_binvector_umap)
  rm(disease_vectors, disease_binvectors)
  rm(disease_model_knn_owlsim, disease_model_knn_crossmap)
  gc()
}


# ############################################################################
# alternative methods of linking diseases to models

search_phenodigm_file <-
  file.path(data.dir, "impc",
            "search-models-phenodigm--data-orphanet.tsv.gz")
search_phenoscoring_template <-
  file.path(data.dir, "phenoscoring",
            "search-models-phenoscoring--data-orphanet-{TRANSLATION}.tsv.gz")

if (!assignc("disease_model_alt_knn")) {
  assignc("model_vectors_raw")
  make_alt_knn <- function(template, TRANSLATION="owlsim", k=15) {
    search_file <- glue(template, TRANSLATION=TRANSLATION)
    scores <- fread(search_file)
    scores <- scores[query %in% disease_info$disease_id]
    scores <- scores[target %in% rownames(model_vectors_raw)]
    umap_knn_from_long(scores, n=k, levels=rownames(model_vectors_raw))
  }
  disease_model_alt_knn <- list(
    owlsim=list(phenodigm=make_alt_knn(search_phenodigm_file),
                phenoscoring=make_alt_knn(search_phenoscoring_template,
                                          TRANSLATION="owlsim")),
    crossmap=list(phenoscoring=make_alt_knn(search_phenoscoring_template,
                                            TRANSLATION="crossmap"))
  )
  savec(disease_model_alt_knn)
  rm(model_vectors_raw)
}

