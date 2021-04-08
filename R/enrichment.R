# analysis functions that compute enrichment (over-representation)
#


#' retrieve a set of items within a gate rectangle
#'
#' @param emb data table with embedding
#' @param gate_rect numeric vector of length 4, coordinates of a rectangular
#' gate like for graphics::rect
#' @param xy character vector of length 2, column in emb with x,y coordinates
#'
#' @return character vector of ids within the gate
ids_in_gate_rect <- function(emb, gate_rect, xy=c("UMAP_1", "UMAP_2")) {
  result <- data.table(id=emb$id, x=emb[[xy[1]]], y=emb[[xy[2]]])
  xlim <- sort(gate_rect[c(1,3)])
  ylim <- sort(gate_rect[c(2,4)])
  result <- result[x >= xlim[1] & x < xlim[2]]
  result <- result[y >= ylim[1] & y < ylim[2]]
  result$id
}


#' compute enrichment of phenotypes
#'
#' @param d data table with columns id (model identifier) and phenotype
#' @param hits character vector, id items that are selected
#' @param bg character vector, ids of items in a comparison set
#'
#' @return data table with counts and enrichment values
#' id - phenotype identifier
#' count_11 - number of hits with the phenotype
#' count_10 - number of hits without the phenotype
#' count_01 - number of bg items with the phenotype
#' count_00 - number of bg items without the phenotype
#' p.value - p-value from Fisher test
#' odds.ratio - odds ratio from Fisher test
phenotype_enrichment <- function(d, hits, bg=NULL) {
  if (is.null(bg)) {
    bg <- setdiff(d$id, hits)
  }
  # compute values for contingency tables for all phenotypes
  hits_tab <- table(d[id %in% hits, "phenotype"]$phenotype)
  bg_tab <- table(d[id %in% bg, "phenotype"]$phenotype)
  hits_counts <- data.table(id=names(hits_tab), count_11=as.integer(hits_tab))
  bg_counts <- data.table(id=names(bg_tab), count_01=as.integer(bg_tab))
  counts <- merge(hits_counts, bg_counts, all=TRUE)
  counts[is.na(counts)] <- 0
  counts$count_10 <- length(hits) - counts$count_11
  counts$count_00 <- length(bg) - counts$count_01
  result <- batch_fisher(counts)
  result
}

