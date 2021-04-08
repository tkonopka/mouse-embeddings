# post-prep for crossmap and phenoscoring

library(data.table)
crossmap_dir <- file.path("..", "data", "crossmap")
phenoscoring_dir <- file.path("..", "data", "phenoscoring")

# crossmap translations n1
search_1 <- fread(file.path(crossmap_dir, "search-mp--data-hp-n1-diff1.tsv.gz"))
scores_1 <- copy(search_1)
setnames(scores_1, c("query", "target"), c("term1", "term2"))
scores_1$score <- 1-scores_1$distance
fwrite(scores_1[, c("term1", "term2", "score")],
       file=file.path(phenoscoring_dir, "hp-mp-crossmap-oomap.tsv.gz"),
       sep="\t")

