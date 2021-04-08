# preparation of data structures for mouse models


# ############################################################################
# mouse model information

model_descriptions_file <- file.path(data.dir, "impc",
                                     "mouse-model-descriptions.tsv.gz")
model_phenotypes_file <- file.path(data.dir, "impc",
                                   "mouse-model-phenotypes.tsv.gz")


if (!assignc("model_phenotypes")) {
  # create an object with concise and complete phenotypes
  make_model_phenotypes <- function() {
    concise <- fread(model_phenotypes_file)[, c("id", "phenotype")]
    ancestors <- copy(mp_info$ancestors)
    setnames(ancestors, "id", "phenotype")
    complete <- merge(concise, ancestors, by="phenotype", allow.cartesian=TRUE)
    complete$phenotype <- NULL
    setnames(complete, "ancestor", "phenotype")
    list(concise=unique(concise), complete=unique(complete))
  }
  model_phenotypes <- make_model_phenotypes()
  savec(model_phenotypes)
}


#' collect information about mouse models and their phenotypes
#' @return data table with model_id, model_description, etc.
prep_model_info <- function() {
  result <- fread(model_descriptions_file)
  result <- result[, fromJSON(description), by="id"]
  result$bin_source <- ifelse(result$source=="MGI", "MGI", "nonMGI")
  phenotypes <- fread(model_phenotypes_file)
  count_phenotypes <- function(x) {
    length(unique(x[x!="MP:0002169"]))
  }
  phenotypes <- phenotypes[, list(num_phenotypes=count_phenotypes(phenotype)),
                             by="id"]
  result <- merge(result, phenotypes, by="id", all=TRUE)
  setnames(result, c("id", "description"), c("model_id", "model_description"))
  result
}
if (!assignc("model_info")) {
  model_info <- prep_model_info()
  savec(model_info)
}


#' get a verbose table about model phenotypes
#'
#' @param model_ids character vector with model identifiers
get_model_phenotypes <- function(model_ids) {
  result <- model_phenotypes[id %in% model_ids, c("id", "phenotype")]
  mp_names <- copy(mp_info$names)
  setnames(mp_names, c("id", "name"), c("phenotype", "phenotype_name"))
  result <- merge(result, mp_names, by="phenotype", cartesian=TRUE)
  setcolorder(result, c("id", "phenotype", "phenotype_name"))
  result[order(id, phenotype)]
}



# subsets of models - based on number of phenotypes
if (!exists("models_w_phen")) {
  models_w_phen <- list(
    "gt_0"=model_info[num_phenotypes>0]$model_id,
    "gt_1"=model_info[num_phenotypes>1]$model_id
  )
}
