# calculation of positions within an embedding using coordinate averaging
#


#' predict coordinates in an embedding via averaging
#'
#' @param emb data table, an embedding of items, columns "id" and 'coordinates'
#' @param data data table, long table with an id column and one 'feature_col'
#' column linking ids to objects/features in the embedding
#' @param trim numeric, for trimmed mean
#' @param coordinates character vector, columns in emb with embedding coordinates
#' @param feature_col column in data that links to ids in emb
#' @param label character, will be added in final data table
#'
#' @return data table with predicted coordinates
predict_avg_coordinates <- function(emb, data, trim=0.1,
                                    coordinates=c("UMAP_1", "UMAP_2"),
                                    feature_col="phenotype",
                                    label="") {
  d <- data[, c("id", feature_col), with=FALSE]
  setnames(d, c("id", feature_col), c("id", "feature"))
  emb_features <- copy(emb)[, c("id", coordinates), with=FALSE]
  setnames(emb_features, "id", "feature")
  result <- merge(d, emb_features, by="feature", allow.cartesian=TRUE)
  result$feature <- NULL
  result <- result[, lapply(.SD, mean, trim=trim), by="id"]
  result$label <- label
  setcolorder(result, c("label", "id", coordinates))
  result
}


#' predict coordinates in an embedding via averaging, but only use
#' first-ranked neighbor
#'
#' @param emb data table with an embedding of items
#' @param data data table with column id and one column linking to objects in emb
#' @param ... other arguments passed to predict_avg_coordinates
#'
#' @return data table, same as output from predict_avg_coordinates
predict_avgk1_coordinates <- function(emb, data, ...) {
  data_first <- data[, head(.SD, 1), by="id"]
  predict_avg_coordinates(emb, data_first, ...)
}

