# post-prep for phenoscoring

library(data.table)
library(glue)
library(RSQLite)
phenoscoring_dir <- file.path("..", "data", "phenoscoring")

model_scores_template <-
  file.path(phenoscoring_dir,
            "phenoscoring-orphanet-{OOMETHOD}-model-scores.tsv.gz")
model_scores_search_template <-
  file.path(phenoscoring_dir,
            "search-models-phenoscoring--data-orphanet-{OOMETHOD}.tsv.gz")


for (oo_method in c("owlsim", "crossmap")) {
  raw_file <- glue(model_scores_template, OOMETHOD=oo_method)
  out_file <- glue(model_scores_search_template, OOMETHOD=oo_method)
  if (!file.exists(out_file)) {
    raw_scores <- fread(raw_file)
    scores <- raw_scores[, c("reference", "model")]
    setnames(scores, c("reference", "model"), c("query", "target"))
    scores$distance <- 1 - raw_scores$general
    scores <- scores[order(distance)]
    scores <- scores[, head(.SD, 20), by="query"]
    fwrite(scores, file=out_file, sep="\t")
  }
}

