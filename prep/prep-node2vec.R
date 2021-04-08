# post-prep for crossmap and phenoscoring

library(data.table)
node2vec_dir <- file.path("..", "data", "node2vec")
impc_dir <- file.path("..", "data", "impc")
obo_dir <- file.path("..", "data", "obo")


###############################################################################
# Load raw data

# phenotypes
mp_parents <- fread(file.path(obo_dir, "mp-parents.tsv.gz"))
mp_ancestors <- fread(file.path(obo_dir, "mp-ancestors.tsv.gz"))
mp_ancestors <- mp_ancestors[,
  list(ancestor=c(id, unlist(strsplit(ancestors, ";")))),
  by="id"]
# mouse models
mouse_phenotypes <- fread(file.path(impc_dir, "mouse-model-phenotypes.tsv.gz"))



###############################################################################
# Construct graphs for node2vec


#' create data files for node2vec
#'
#' @param x data frame with two columns $from and $to
#' @param prefix character, prefix for output files
#'
#' @return set of output files
make_node2vec_inputs <- function(x, prefix) {
  node_levels <- sort(unique(c(x$from, x$to)))
  fwrite(data.table(index=seq_along(node_levels), name=node_levels),
         file=paste0(prefix, "-nodes.txt"),
         sep="\t", col.names=FALSE, row.names=FALSE)
  fwrite(data.table(from=match(x$from, node_levels),
                    to=match(x$to, node_levels)),
         file=paste0(prefix, "-edges.txt"),
         sep="\t", col.names=FALSE, row.names=FALSE)
}


# network consisting only of ontology nodes
mp_graph <- data.table(from=mp_parents$id, to=mp_parents$parent)
make_node2vec_inputs(mp_graph, file.path(node2vec_dir, "mp-ontology"))


# network consisting of ontology nodes and concise mouse models
model_concise_graph <- data.table(from=mouse_phenotypes$id,
                                  to=mouse_phenotypes$phenotype)
make_node2vec_inputs(rbind(mp_graph, model_concise_graph),
                     file.path(node2vec_dir, "mouse-model-concise"))


# graph consisting of ontology node and complete mouse models
model_complete_graph <- merge(data.table(from=mouse_phenotypes$id,
                                         id=mouse_phenotypes$phenotype),
                              data.table(id=mp_ancestors$id,
                                         to=mp_ancestors$ancestor),
                              by="id", allow.cartesian=TRUE)
model_complete_graph <- unique(model_complete_graph[, c("from", "to")])
make_node2vec_inputs(rbind(mp_graph, model_complete_graph),
                     file.path(node2vec_dir, "mouse-model-complete"))
