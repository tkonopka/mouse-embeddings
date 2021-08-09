# preparation of data structures with MP ontology vector representations


# ############################################################################
# mouse model MP ontology vector representations

#' read a series of model representation files
#'
#' @param feature_names character vector
#' @return matrix with phenotypes in columns and model names in rows
prep_model_vectors <- function(feature_names=NULL, ids=NULL) {
  result <- list()
  i <- 1
  template <- templates$model_repr
  while (file.exists(glue(template, PART=i))) {
    .models <- as.data.frame(fread(glue(template, PART=i)))
    rownames(.models) <- .models$phenotype
    .models$phenotype <- NULL
    result[[i]] <- t(.models)[, feature_names]
    rm(.models)
    i <- i + 1
  }
  do.call(rbind, result)[ids, ]
}


if (!assignc("model_vectors_ids")) {
  model_vectors_ids <- unique(model_phenotypes$concise$id)
  savec(model_vectors_ids)
}


# model_vectors is what is needed downstream, but is a large file on disk
if (!assignc("model_vectors")) {
  model_vectors_raw <-
    prep_model_vectors(feature_names=mp_info$names$id,
                       ids=model_vectors_ids)
  savec(model_vectors_raw)
  model_vectors <- normalize_by_row(model_vectors_raw)
  savec(model_vectors)
}


# ############################################################################
# mouse model MP ontology binary vector representations

#' read a series of model representation files
#'
#' @param feature_names character vector
#' @return matrix with phenotypes in columns and model names in rows
prep_model_binvectors <- function(feature_names=NULL, ids=NULL) {
  data <- model_phenotypes$complete
  data$value <- 1
  data <- rbind(data, data.table(id=".", phenotype=feature_names, value=0))
  result <- dcast(data, id~phenotype, value.var="value")
  result[is.na(result)] <- 0
  result_ids <- result$id
  result$id <- NULL
  result <- as.matrix(result)
  rownames(result) <- result_ids
  feature_names <- intersect(feature_names, colnames(result))
  result[ids, feature_names]
}
if (!assignc("model_binvectors")) {
  model_binvectors_raw <-
    prep_model_binvectors(feature_names=mp_info$names$id,
                          ids=model_vectors_ids)
  model_binvectors <- normalize_by_row(model_binvectors_raw)
  savec(model_binvectors)
}


# ############################################################################
# disease MP ontology vector representations

#' read a table with disease representations
#'
#' @param translation character, code for the hp-mp translation method
#' @param feature_names character vector,
#'
#' @return matrix with phenotypes in columns, diseases in rows
prep_disease_vectors <- function(translation, feature_names=NULL) {
  path <- glue(templates$disease_repr, TRANSLATION=translation)
  result <- as.data.frame(fread(path))
  rownames(result) <- result$phenotype
  # remove phenotype column so that remainder can turn into a numeric matrix
  result$phenotype <- NULL
  # remove diseases that are not in the disease info
  # (removes "null" and other parsing-outliers, e.g. diseases with
  # declared phenotypes but undeclared disease name in nomenclature)
  disease_ids <- disease_info[disease_num_phenotypes>0]$disease_id
  disease_ids <- sort(intersect(colnames(result), disease_ids))
  result <- result[, disease_ids]
  t(as.matrix(result))[, feature_names]
}

# get matrices with disease phenotypes
if (!assignc("disease_vectors")) {
  disease_vectors_raw <- lapply(translation_methods, prep_disease_vectors,
                                feature_names=mp_info$names$id)
  savec(disease_vectors_raw)
  disease_vectors <- lapply(disease_vectors_raw, normalize_by_row)
  savec(disease_vectors)
}


# ############################################################################
# disease MP ontology binary vector representations

#' read a series of model representation files
#'
#' @param dp data table with id and phenotype (concise phenotype)
#' @param feature_names character vector with MP terms
#'
#' @return matrix with phenotypes in columns and model names in rows
prep_disease_binvectors <- function(dp, feature_names=NULL, ids=NULL) {
  # make complete phenotypes for the diseases
  ancestors <- copy(mp_info$ancestors)
  setnames(ancestors, "id", "phenotype")
  data <- unique(merge(dp, ancestors, by="phenotype", allow.cartesian=TRUE))
  data$phenotype <- NULL
  setnames(data, "ancestor", "phenotype")
  data$value <- 1
  data <- rbind(data, data.table(id=".", phenotype=feature_names, value=0))
  result <- dcast(unique(data), id~phenotype, value.var="value")
  result[is.na(result)] <- 0
  result_ids <- result$id
  result$id <- NULL
  result <- as.matrix(result)
  rownames(result) <- result_ids
  feature_names <- intersect(feature_names, colnames(result))
  result[ids, feature_names]
}
if (!assignc("disease_binvectors")) {
  disease_ids <- rownames(disease_vectors[[1]])
  disease_binvectors_raw <- lapply(disease_phenotypes, function(dp) {
    prep_disease_binvectors(dp, feature_names=mp_info$names$id,
                            ids=disease_ids)
  })
  savec(disease_binvectors_raw)
  disease_binvectors <- lapply(disease_binvectors_raw, normalize_by_row)
  savec(disease_binvectors)
}

