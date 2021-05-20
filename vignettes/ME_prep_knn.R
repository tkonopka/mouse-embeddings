# preparation of data structures with embeddings with knn


#' read a file with knn search results and create a umap.knn object
#'
#' @param template character, path to crossmap search result
#' @param repr character, choice of representations, {REPR} in template
#' @param diff numeric, setting for diffusion, {DIFF} in template
#' @param n integer, number of neighbors for the umap.knn object, {N} in template
#'
#' @return umap.knn object
prep_umap_knn <- function(template=templates$model_search,
                          repr=c("complete", "concise"), diff=0, n=15,
                          randomize_degenerate=TRUE) {
  repr <- match.arg(repr)
  knn <- fread(glue(template, REPR=repr, DIFF=diff, N=n))
  result <- umap_knn_from_long(knn)
  if (randomize_degenerate) {
    result <- randomize_degenerate_umap_knn(result)
  }
  result
}


if (!assignc("mp_knn")) {
  mp_knn <- prep_umap_knn(template=templates$mp_search, diff=0, n=15)
  savec(mp_knn)
}

