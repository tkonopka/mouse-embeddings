# embeddings for mouse models using node2vec


# datasets embedded with node2vec
node2vec_datasets <- c("mp-ontology", "mouse-model-concise")
node2vec_datasets <- setNames(node2vec_datasets, gsub("-", "_", node2vec_datasets))


# ############################################################################
# embeddings from node2vec

if (!assignc("node2vec_embedding")) {
  parse_node2vec_embedding <- function(dataset, settings) {
    nodes_file <- glue(templates$node2vec_nodes, DATASET=dataset)
    emb_file <- glue(templates$node2vec_embedding, DATASET=dataset,
                     DIM=2, SETTINGS=settings)
    if (!file.exists(emb_file)) return(NULL)
    result <- read_node2vec(nodes_file, emb_file)
    result$label <- ifelse(startsWith(result$id, "MP:"), "mp", "model")
    setcolorder(result, c("label", "id"))
    result
  }
  .snap <- lapply(node2vec_datasets, parse_node2vec_embedding, settings="snap")
  .short <- lapply(node2vec_datasets, parse_node2vec_embedding, settings="short")
  .defaults <- lapply(node2vec_datasets, parse_node2vec_embedding, settings="defaults")
  node2vec_embedding <- list(snap=.snap, short=.short, defaults=.defaults)
  savec(node2vec_embedding)
}
