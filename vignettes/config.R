# configuration variables


# ############################################################################
# libraries/packages

suppressMessages(library(data.table))
library(parallel)
suppressMessages(library(jsonlite))
suppressMessages(library(yaml))
suppressMessages(library(Rcssplot))
library(shrt)  # github.com/tkonopka/shrt
suppressMessages(library(glue))
suppressMessages(library(umap))
suppressMessages(library(batchFisher))  # github.com/tkonopka/batchFisher
suppressMessages(library(viridisLite))
setDTthreads(percent=100)


# ############################################################################
# paths to directories

# this assumes the script is executes from vignettes/
R.dir <- file.path("..", "R")
scripts.dir <- file.path("..", "scripts")
data.dir <- file.path("..", "data")
results.dir <- file.path("..", "results")


# ############################################################################
# custom functions from R directory

.rfiles <- c("enrichment", "avg_embedding", "knn", "predictions",
             "gate_embedding", "normalize", "pca",
             "plot_general", "plot_embedding", "plot_scatter", "plot_bars",
             "plot_enrichment", "plot_histboxes", "plot_linehist", "plot_list",
             "plot_schematics", "plot_heatmap", "plot_query", "plot_tab2x2",
             "write_embedding", "read_node2vec")
for (.rfile in .rfiles) {
  source(file.path(R.dir, paste0(.rfile, ".R")))
}
rm(.rfile, .rfiles)




# ############################################################################
# constants, thesholds, paths, etc

# path to graphics styles
RcssDefaultStyle <- Rcss(c("CE.css", "CE-tutorial.css", "CE-schematic.css"))

# labels to designate plot panels
panel.labels <- LETTERS

# cache directory
cachedir(file.path("..", "cache"))

# templates for files
templates <- list(
  disease_repr=file.path(data.dir, "phenoscoring",
                         paste0("phenoscoring-orphanet-",
                                "{TRANSLATION}-references_data")),
  model_repr=file.path(data.dir, "phenoscoring",
                       paste0("phenoscoring-orphanet-",
                              "owlsim-models-{PART}_data")),
  mp_search=file.path(data.dir, "crossmap",
                      "search-mp--data-mp-n15-diff{DIFF}"),
  model_search=file.path(data.dir, "crossmap",
                      "search-models-{REPR}--data-models-{REPR}-diff{DIFF}"),
  mp_embedding=file.path(results.dir,
                         "embedding-of-mp--{ALGO}-d{DIM}"),
  color_embedding=file.path(results.dir,
                            "embedding-of-colors-{ENCODING}--{ALGO}-d2"),
  model_embedding=file.path(results.dir,
                            "embedding-of-mouse-models-{ENCODING}--{ALGO}-d{DIM}--data-{WHAT}"),
  disease_embedding=file.path(results.dir,
                              "embedding-of-diseases-{TRANSLATION}-{ENCODING}--{ALGO}-d{DIM}--data-{WHAT}"),
  disease_search=file.path(data.dir, "crossmap",
                           "search-models-{REPR}--data-{DISEASE}-{TRANSLATION}-diff{DIFF}"),
  node2vec_nodes=file.path(results.dir, "{DATASET}-nodes"),
  node2vec_embedding=file.path(results.dir, "{DATASET}-node2vec-d{DIM}-{SETTINGS}"),
  prediction_errors=file.path(results.dir, "prediction-errors-for-{WHAT}--{ERR}-{ENCODING}-d{DIM}")
)
templates <- lapply(templates, function(x) { paste0(x, ".tsv.gz") })


# configuration for umap
embedding.config <- umap.defaults
embedding.config$min_dist <- 0.2
embedding.config$knn_repeats <- 3
embedding.config$random_state <- 12345


# configuration for umap for computing knn only
knn.config <- umap.defaults
knn.config$knn_repeats <- 3
knn.config$random_state <- 54321
# the embedding layout will not be used with this configuration
# so save time by initializing in a random fashion and avoiding epochs
knn.config$init <- "random"
knn.config$n_epochs <- 0


# helper to create a named vector
named_vector <- function(x) { setNames(x, x) }


# list of hp-mp translation methods
translation_methods <- named_vector(c("owlsim", "crossmap"))

hp_mp_prefix <- file.path(data.dir, "phenoscoring", "hp-mp-")
hp_mp_translations_files <- list(
  owlsim=paste0(hp_mp_prefix, "owlsim-oomap.tsv.gz"),
  crossmap=paste0(hp_mp_prefix, "crossmap-oomap.tsv.gz")
)

# text encoding methods for models
text_methods <- named_vector(paste0("text_",
                                    c("concise_diff0", "complete_diff0")))

# text encoding methods for diseases
disease_text_methods <- named_vector(c("orphanet"))

# embedding dimensions
embedding_d <- c(2, 4, 6, 8, 10)

