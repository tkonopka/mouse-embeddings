# prep for crossmap
# uses hp-mp translations to build disease datasets for crossmap

library(data.table)
library(yaml)
suppressWarnings(library(R.utils))

obo_dir <- file.path("..", "data", "obo")
crossmap_dir <- file.path("..", "data", "crossmap")
phenoscoring_dir <- file.path("..", "data", "phenoscoring")
orphanet_dir <- file.path("..", "data", "orphanet")


# load translations of hp-mp terms
translations <- list(owlsim="owlsim",
                     crossmap="crossmap")
translations <- lapply(translations, function(translation_method) {
  filename <- paste0("hp-mp-", translation_method, "-oomap.tsv.gz")
  fread(file.path(phenoscoring_dir, filename))
})

# load names of phenotypes
mp_names <- fread(file.path(obo_dir, "mp-names.tsv.gz"))

# load disease definitions
if (!exists("orphanet")) {
  orphanet <- read_yaml(file.path(orphanet_dir, "orphanet.yaml.gz"))
}


#' write a yaml dataset into a compressed file
write_yaml_gz <- function(x, file) {
  write_yaml(x, file=gsub(".gz", "", file))
  gzip(gsub(".gz", "", file))
}

if (!exists("orphanet_mp")) {
  translate_disease <- function(disease, translation) {
    # translate the disease phenotypes using ids and frequencies
    get_ids <- function(x) { sapply(strsplit(x, " "), head, 1) }
    metadata <- disease$metadata
    translate_terms <- function(phen_column) {
      ids <- metadata[[phen_column]]
      result <- data.table(term1="")[0]
      if (length(ids)) {
        result <- data.table(term1=get_ids(ids))
      }
      result <- merge(result, translation, by="term1")
      result <- merge(result, mp_names, by.x="term2", by.y="id")
      result <- result[, head(.SD, 1), by="term2"]
      result[order(term2)]
    }
    pos_mp <- translate_terms("phenotype_ids")
    neg_mp <- translate_terms("neg_phenotype_ids")
    # construct a mp disease object
    disease_mp <- disease
    disease_mp$metadata$phenotype_ids <- pos_mp$term2
    disease_mp$metadata$neg_phenotype_ids <- neg_mp$term2
    disease_mp$data$phenotypes <- pos_mp$name
    disease_mp$data_neg$phenotypes <- neg_mp$name
    disease_mp
  }
  # convert diseases into mp representations for crossmap
  orphanet_mp <- lapply(translations, function(translation) {
    lapply(orphanet, translate_disease, translation=translation)
  })

  for (translation_method in names(orphanet_mp)) {
    tm <- translation_method
    write_yaml_gz(orphanet_mp[[tm]],
                  file.path(crossmap_dir,
                            paste0("orphanet-", tm, ".yaml")))
    rm(tm)
  }
}

