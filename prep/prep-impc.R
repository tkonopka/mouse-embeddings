# prep of datasets from IMPC databases

library(curl)
library(jsonlite)
library(yaml)
library(data.table)
suppressWarnings(library(R.utils))

# path to local directory for output files
data_dir <- file.path("..", "data", "impc")
obo_dir <- file.path("..", "data", "obo")


###############################################################################
# Download data about mouse models from IMPC servers

# url to an IMPC solr server
impc_solr_url <- "http://www.ebi.ac.uk/mi/impc/solr/phenodigm/"
impc_solr_url <- "http://wwwdev.ebi.ac.uk/mi/impc/dev/solr/phenodigm/"

#' download data from solr, or fetch from disk cache
#'
#' @param query character, solr query
#' @param base.url character, address of an IMPC solr core
#'
#' @return data.table with content of data from the download
download_impc_data <- function(query, base_url=impc_solr_url) {
  url <- paste0(base_url, "select?q=", query, "&rows=123123123&wt=csv")
  temp_path <- paste0("temp.csv")
  curl::curl_download(url, temp_path)
  data <- fread(temp_path)
  unlink(temp_path)
  na_cols <- sapply(data, function(x) { !all(is.na(x))})
  select_cols <- names(na_cols)[na_cols]
  data[, select_cols, with=FALSE]
}

#' adjust a raw downloaded data table - phenotypes into separate rows
#'
#' @param models data.table downloaded from solr
#' @return data.table with phenotypes split into sepearate rows
prep_phenotype_table <- function(models) {
  result <- copy(models)
  result$type <- NULL
  # parse phenotype fields from text "MP:123 description, MP:456 description"
  parse_phenotypes <- function(x) {
    parts <- unlist(strsplit(x, "MP"))
    parts <- parts[parts!=""]
    parts <- gsub(",$", "", paste0("MP", parts))
    parts <- strsplit(parts, " ")
    mp_id <- sapply(parts, head, n=1)
    mp_name <- sapply(parts, function(x) { paste(x[-1], collapse=" ")})
    list(mp_id=mp_id, mp_name=mp_name)
  }
  info_fields <- c("model_id", "model_description",
                   "model_genetic_background", "model_source",
                   "marker_id", "marker_symbol")
  result[, parse_phenotypes(model_phenotypes), by=info_fields]
}

# download gene names and gene-gene translations
gene_file <- file.path(data_dir, "solr-gene-raw.tsv.gz")
gene_gene_file <- file.path(data_dir, "solr-gene-gene-raw.tsv.gz")
if (!file.exists(gene_file)) {
  gene <- download_impc_data("type:gene")
  fwrite(gene, gene_file, sep="\t")
}
if (!file.exists(gene_gene_file)) {
  gene_gene <- download_impc_data("type:gene_gene")
  fwrite(gene_gene, gene_gene_file, sep="\t")
}
gene <- fread(gene_file)
gene_gene <- fread(gene_gene_file)


# download mouse model data
raw_models_file <- file.path(data_dir, "solr-models-raw.tsv.gz")
if (!file.exists(raw_models_file)) {
  raw_models <- download_impc_data("type:mouse_model")
  fwrite(raw_models, raw_models_file, sep="\t")
}
raw_models <- fread(raw_models_file)

# transform the raw model information into a related format
models_file <- file.path(data_dir, "solr-models.tsv.gz")
if (!file.exists(models_file)) {
  models <- prep_phenotype_table(raw_models)
  fwrite(models, file=models_file, sep="\t")
}
models <- fread(models_file)
models <- models[!grepl("^..#", model_id)]


###############################################################################
# Download phenodigm scores

#' adjust a raw downloaded data table - disease-model phenodigm scores
#'
#' @param scores data.table downloaded from solr
#' @return data.table with adjusted columns
prep_disease_models_table <- function(scores_tab, min_raw=1.3, min_norm=60) {
  scores_cols <- c("disease_id", "model_id",
                   paste0("disease_model_", c("avg_norm", "avg_raw",
                                              "max_norm", "max_raw")))
  result <- scores_tab[, scores_cols, with=FALSE]
  result <- result[grep("ORPHA", disease_id)]
  result <- result[disease_model_avg_norm > min_norm |
                     disease_model_max_norm > min_norm |
                     disease_model_avg_raw > min_raw |
                     disease_model_max_raw > min_raw]
  result$phenodigm_score <-
    (result$disease_model_avg_norm + result$disease_model_max_norm) / 2
  result$distance <- 100 - result$phenodigm_score
  setnames(result, c("disease_id", "model_id"), c("query", "target"))
  result <- result[order(query, distance)]
  result[, c("query", "target", "distance")]
}


raw_disease_models_file <- file.path(data_dir, "solr-disease-models-raw.tsv.gz")
if (!file.exists(raw_disease_models_file)) {
  raw_disease_models <- download_impc_data("type:disease_model_summary")
  fwrite(raw_disease_models, file=raw_disease_models_file, sep="\t")
}
raw_disease_models <- fread(raw_disease_models_file)

disease_models_file <- file.path(data_dir,
                                 "search-models-phenodigm--data-orphanet.tsv.gz")
if (!file.exists(disease_models_file)) {
  disease_models <- prep_disease_models_table(raw_disease_models)
  fwrite(disease_models, file=disease_models_file, sep="\t")
}
disease_models <- fread(disease_models_file)


###############################################################################
# Prepare the data into a format for phenoscoring.py

#' prepare a table with model id, category, and description
#'
#' @param models data.table prepared from previous steps
#' @return data.table with three columns
prep_description_tab <- function(models) {
  mm <- unique(models[, c("model_id", "model_description",
                          "model_genetic_background", "model_source",
                          "marker_symbol", "marker_id")])
  colnames(mm) <- gsub("model_", "", colnames(mm))
  prep_description <- function(x) { toJSON(x) }
  result <- mm[, list(description=prep_description(.SD)), by="id"]
  result$description <- gsub("^\\[|\\]$", "", result$description)
  mm$category <- ifelse(mm$source=="MGI", "MGI", "IMPC")
  result <- merge(result, mm[, c("id", "category")], by="id")
  setcolorder(result, c("id", "category", "description"))
  result
}

#' prepare a table with phenotypes for phenoscoring
#'
#' @param models data.table prepared from previous steps
#' @return data.table with columns (id, phenotype, timestamp, value, TPR, FPR)
prep_phenotype_tab <- function(models, timestamp="DR13") {
  result <- models[, c("model_id", "mp_id")]
  result$timestamp <- timestamp
  result$value <- 1
  result$TPR <- 0.8
  result$FPR <- 0.05
  setnames(result, c("model_id", "mp_id"), c("id", "phenotype"))
  setcolorder(result, c("id", "phenotype", "timestamp", "value", "TPR", "FPR"))
  result <- result[grepl("MP:", phenotype)]
}

# split information into model description and model phenotype files
# (format for phenoscoring.py)
description_file <- file.path(data_dir, "mouse-model-descriptions.tsv.gz")
if (!file.exists(description_file)) {
  description_tab <- prep_description_tab(models)
  fwrite(description_tab, file=description_file, sep="\t", quote=FALSE)
}

phenotype_file <- file.path(data_dir, "mouse-model-phenotypes.tsv.gz")
if (!file.exists(phenotype_file)) {
  phenotype_tab <- prep_phenotype_tab(models)
  fwrite(phenotype_tab, file=phenotype_file, sep="\t", quote=FALSE)
}


###############################################################################
# Prepare the data into a format for crossmap.py

mp_names <- fread(file.path(obo_dir, "mp-names.tsv.gz"))
mp_ancestors <- fread(file.path(obo_dir, "mp-ancestors.tsv.gz"))
mp_ancestors <- mp_ancestors[,
  list(ancestor=c(id, unlist(strsplit(ancestors, ";")))),
  by="id"]


#' prepare a list item for one model
prep_crossmap_item <- function(x, complete=FALSE) {
  mp_concise <- mp_names[id %in% x$mp_id][order(id)]
  result <- list(data=list(concise=mp_concise$name),
                 metadata=list(mp_id=mp_concise$id,
                               model_id=unique(x$model_id),
                               source=unique(x$model_source)))
  if (complete) {
    inferred <- setdiff(mp_ancestors[id %in% mp_concise$id]$ancestor,
                        mp_concise$id)
    result$data$inferred <- mp_names[id %in% inferred]$name
  }
  result
}

#' prepare a list object that can be used as a crossmap dataset
prep_crossmap_dataset <- function(models, complete=FALSE) {
  result <- split(as.data.frame(models), models$model_id)
  result <- lapply(result, prep_crossmap_item, complete=complete)
  num_concise <- sapply(result, function(x) { length(x$data$concise) })
  result[num_concise>0]
}
#' write a yaml dataset into a compressed file
write_yaml_gz <- function(x, file) {
  write_yaml(x, file=gsub(".gz", "", file))
  gzip(gsub(".gz", "", file))
}

crossmap_concise_file <- file.path(data_dir, "mouse-model-concise.yaml.gz")
if (!file.exists(crossmap_concise_file)) {
  crossmap_concise_data <- prep_crossmap_dataset(models, complete=FALSE)
  write_yaml_gz(crossmap_concise_data, file=crossmap_concise_file)
}
crossmap_complete_file <- file.path(data_dir, "mouse-model-complete.yaml.gz")
if (!file.exists(crossmap_complete_file)) {
  crossmap_complete_data <- prep_crossmap_dataset(models, complete=TRUE)
  write_yaml_gz(crossmap_complete_data, crossmap_complete_file)
}
