# helper functions/wrappers

#' draw two panels - an embedding diagram and an enrichment calculation
#'
#' @param embedding data table with an embedding
#' @param gate_rect numeric of length 4, limits
#' @param panel.label character, used for corner label
#' @param embedding_points data table with points to add on top of core
#' embedding
#' @param points_Rcssclass character, style class to apply to additional points
#' @param ... other arguments passed on to plot_enrichment
wrap_embedding_enrichment <- function(embedding, gate_rect,
                                      panel.labels=c("", ""),
                                      show_list=FALSE,
                                      main=" Selection: {n_hits} models",
                                      main_enrichment = "Enrichment analysis",
                                      embedding_points=NULL,
                                      points_Rcssclass=NULL,
                                      ...) {
  hits <- ids_in_gate_rect(embedding, gate_rect)
  plot_embedding(embedding, detail=1,
                 main=glue(main, n_hits=length(hits)),
                 xlim=gate_rect[c(1,3)], ylim=gate_rect[c(2,4)],
                 Rcssclass="enrich")
  if (!is.null(embedding_points)) {
    points_embedding(embedding_points, detail=1,
                     Rcssclass=c("enrich", points_Rcssclass))
  }
  multipanelLabel(panel.labels[1])
  enrichment <- phenotype_enrichment(model_phenotypes$complete, hits)
  plot_enrichment(enrichment, main=main_enrichment, ...)
  multipanelLabel(panel.labels[2])
  if (show_list) {
    top_hits <- enrichment[p.value < 0.05/nrow(enrichment)]
    top_hits$fraction <-  top_hits$count_11 / (top_hits$count_11 + top_hits$count_10)
    top_hits <- merge(top_hits, mp_info$names, by="id")
    top_hits <- top_hits[order(-fraction)]
    top_hits$text <- paste0(top_hits$name, " (", round(top_hits$fraction, 2), ")")
    plot_list(top_hits$text,
              main="Phenotype (fraction of models)",
              max.width=0.98)
    multipanelLabel(panel.labels[3])
  }
  invisible(enrichment)
}


#' draw four panels - a plain panel with disease name, an
#' embedding neighborhood, and enrichment data
wrap_disease_neighborhood <- function(embedding, disease_id, gate_rect,
                                      embedding_points=NULL,
                                      panel.label="",
                                      points_Rcssclass="owlsim") {
  parplot(c(0, 1), c(0, 1), Rcssclass="disease_name")
  .disease <- disease_id
  .main <- disease_info[disease_id==.disease]$disease_name
  text(0, 0, paste0(.main, " (", .disease, ")"), Rcssclass="disease_name")
  multipanelLabel(panel.label)
  wrap_embedding_enrichment(embedding[label=="model"], gate_rect=gate_rect,
                            panel.label="",
                            xlim=c(0, 1), ylim=c(0, 320),
                            show_list=TRUE,
                            embedding_points=embedding_points,
                            points_Rcssclass=points_Rcssclass)
}


#' draw three panels
#' - an embedding with predicted coordinates
#' - a correlation between radius and number of phenotypes
#' - a histogram of number of phenotypes
#'
#' @param embedding_bg data table with an embedding of background points
#' @param embedding_fg data table with an embedding of foregraound points
#' @param item_info data table with info about diseases (num phenotypes)
#' @param main character chart title
#' @param detail_bg numeric, fraction of embedding_bg to show in plot
#' @param detail_fg numeric, fraction of embedding_fg to show in plot
#' @param legend_pos numeric, coordinates for legend
#' @param legend_labels named vector with legend contents
#' @param Rcssclass character, style class
#' @param panel.labels character, labels for top-left corner
wrap_avg_coordinates <- function(embedding_bg, embedding_fg, item_info,
                                 main="", hist_label="", hist_label_x=0,
                                 detail_bg=0.5, detail_fg=0.5,
                                 density_ylim=c(0, 0.1),
                                 legend_pos="topleft", legend_labels=NULL,
                                 Rcssclass="owlsim", panel.labels=c("", "")) {
  # prep data
  .emb <- copy(embedding_fg)
  mean_1 <- mean(.emb$UMAP_1)
  mean_2 <- mean(.emb$UMAP_2)
  .emb$r <- sqrt((.emb$UMAP_1-mean_1)^2 + (.emb$UMAP_2-mean_2)^2)
  .emb <- merge(.emb, item_info[, c("id", "num_phenotypes")], by="id")
  # embedding
  plot_embedding(embedding_bg, detail=detail_bg, main=main)
  points_embedding(embedding_fg, detail=detail_fg, Rcssclass=Rcssclass)
  add_embedding_legend(legend_pos, legend_labels)
  multipanelLabel(panel.labels[1])
  # histogram/density plot of number of phenotypes
  plot_linehist(.emb$num_phenotypes,
                breaks=seq(0, 10+max(.emb$num_phenotypes), by=5),
                xlab="num. phenotypes",
                ylab="density",
                line_label=hist_label, line_label_x=hist_label_x,
                main="", xlim=c(0, 100), ylim=density_ylim,
                at=list(x=seq(0, 100, by=30), y=seq(0, 1, by=0.1)),
                Rcssclass=Rcssclass)
  multipanelLabel(panel.labels[2])
  # boxplot with distance from center
  plot_histboxes(.emb, xy=c("num_phenotypes", "r"),
                 breaks=seq(0, 100, by=10),
                 xlab="num. phenotypes", ylab="distance from center",
                 main="",
                 xlim=c(0, 100), ylim=c(0, 11),
                 at=list(x=seq(0, 100, by=30), y=seq(0, 12, by=3)),
                 Rcssclass=c("avg", Rcssclass))
  multipanelLabel(panel.labels[3])
}


#' draw a series of panels with lists of phenotypes for models
#'
#' @param model_ids character vector with model ids
#' @param panel.labels character vector with panel labels
#' @param show_marker_symbol logical, set TRUE to display marker symbol in title
#' @param Rcssclass character, style class
#'
wrap_model_phenotype_list <- function(model_ids, panel.labels=letters,
                                      show_marker=FALSE,
                                      Rcssclass=NULL) {
  for (i in seq_along(model_ids)) {
    .id <- model_id <- model_ids[i]
    model_data <- model_phenotypes$concise[id==model_id]

    phenotypes <- merge(data.table(id=model_data$phenotype),
                        mp_info$names, by="id")[order(id)]
    .main <- model_id
    if (show_marker) {
      .main <- paste0(.main, " (", model_info[model_id==.id]$marker_symbol, ")")
    }
    plot_list(phenotypes$name, main=.main, Rcssclass=c("model", Rcssclass))
    multipanelLabel(panel.labels[i])
  }
}


#' add a series of labels to an embedding
#'
#' @param d data table with embedding
#' @param highlight data table with $id, coordinates for label, and Rcssclass
#'
wrap_embedding_labels <- function(d, highlights, panel.labels=letters) {
  # add labels to an existing embedding plot
  if (!("label" %in% colnames(highlights))) {
    highlights$label <- highlights$id
  }
  for (i in 1:nrow(highlights)) {
    .id <- highlights$id[i]
    if (.id %in% d$id) {
      add_embedding_label(d[id==.id],
                          label_text=highlights$label[i],
                          label_coord=c(highlights$label_1[i],
                                        highlights$label_2[i]),
                          Rcssclass=highlights$Rcssclass[i])
    }
  }
}


#' draw a matrix with comparisons of embeddings
#'
#' @param m square matrix
#' @param labels character vector with names, mapping groups to text labels
#' @param legend.pos bounding coordinates for the legend scale
#'
wrap_embedding_comparison <- function(m, labels, col.limits=c(0, 1),
                                      legend.pos=c(1, -8, 4, -9),
                                      ...) {
  groups <- list()
  for (x in names(labels)) {
    groups[[x]] <- grep(paste0("^", x), rownames(m), value=TRUE)
  }
  plot_matrix(m, groups_x=groups, groups_y=groups,
              col.limits=col.limits, col.fun=c("#ffffff", "#000000"),
              labels=labels, ...)
  add_heatmap_scale(legend.pos, col.fun=c("#ffffff", "#000000"),
                    col.labels=seq(col.limits[1], col.limits[2], length=3),
                    main="Mean Jaccard Index of neighbors")
}


#' draw a series of panels with embeddings and with genes selected
#'
#' @param d data table with embedding
#' @param xy character, column in d with embedding coordinates
#' @param main character, label for title
#' @param model_ids character vector with model ids
#' @param noise_sd numeric, noise to add to the highlighted models' coordinates
#' @param detail numeric, downsampling detail for the background models
#' @param legend.pos numeric of length 2, coordinates for legend
#' @param panel.labels character vector with panel labels
#' @param ... other arguments passed to plot_embedding
#'
wrap_embedding_gene <- function(d, marker_ids,
                                detail=0.5, xy=c("UMAP_1", "UMAP_2"),
                                main="Mouse models",
                                panel.labels=letters,
                                noise_sd=1,
                                legend_pos="topleft", ...) {
  for (i in seq_along(marker_ids)) {
    marker <- marker_ids[i]
    marker_symbol <- d[marker_id==marker]$marker_symbol[1]
    plot_embedding(d, detail=detail, main=main, ...)
    d_gene <- d[marker_id==marker, ]
    d_gene$x <- d_gene[, xy[1], with=FALSE] + rnorm(nrow(d_gene), 0, noise_sd)
    d_gene$y <- d_gene[, xy[2], with=FALSE] + rnorm(nrow(d_gene), 0, noise_sd)
    points_embedding(d_gene, xy=c("x", "y"), detail=1, Rcssclass="gene", ...)
    add_embedding_legend(legend_pos, c(gene=marker_symbol, other="other"))
    multipanelLabel(panel.labels[i])
  }
}


#' draw a heatmap comparing model phenotypes with
#'
#' @param query_id character, id for one object in knn
#' @param knn object of class umap.knn
#' @param vectors_raw matrix with raw data for neighbors
#' @param query_data vector with data associated with query. If NULL,
#' the data is extracted from vectors_raw
#' @param known_phenotypes vector of phenotypes annotated to the query
#' @param k integer, number of neighbors to average
#' @param main character, used as part of title
#' @param neighbor_ids character vector that will map indexes to model ids
#' @param normalize function, used to normalize averaged vectors
#' @param max_rows integer, number of phenotypes to show in a heatmap
#'
#' @return matrix with model data and predictions
wrap_prediction <- function(query_id, knn, vectors_raw=NULL,
                            query_data=NULL, known_phenotypes=NULL,
                            k=2, main=NULL,
                            normalize=normalize_by_row,
                            neighbor_ids=NULL,
                            max_rows=12, ...) {
  if (is.null(neighbor_ids)) {
    neighbor_ids <- rownames(knn$indexes)
  }
  neighbor_indexes <- knn$indexes[query_id, seq(2, k+1)]
  neighbor_names <- neighbor_ids[neighbor_indexes]
  # get model and prediction values
  if (is.null(query_data)) {
    query_data <- vectors_raw[query_id, , drop=FALSE]
  }
  query_data <- normalize(query_data)
  neighbor_data <- normalize(vectors_raw[neighbor_names, , drop=FALSE])
  neighbor_avg <- matrix(colSums(neighbor_data)/ nrow(neighbor_data), nrow=1)
  neighbor_avg <- normalize(neighbor_avg)
  result <- data.table(query=as.numeric(query_data),
                       neighbor_avg=as.numeric(neighbor_avg))
  result$phenotype <- colnames(query_data)
  # annotate with phenotype names, known/novel
  result <- merge(result, mp_info$names, by.x="phenotype", by.y="id")
  if (is.null(known_phenotypes)) {
    known_phenotypes <- model_phenotypes$concise[id==query_id]$phenotype
  }
  known_ancestors <- mp_info$ancestors[id %in% known_phenotypes]$ancestor
  result$known <- as.integer(result$phenotype %in% known_phenotypes)
  result$known_ancestors <- as.integer(result$phenotype %in% known_ancestors)
  result$phenotype_class <- "prior"
  result$phenotype_class[result$known_ancestor>0] <- "ancestor_of_annotated"
  result$phenotype_class[result$known>0] <- "annotated"
  result <- result[abs(query - neighbor_avg)>1e-5 | phenotype_class == "annotated"]
  result <- result[order(-abs(query-neighbor_avg))]
  # plot a heatmap
  result$name_abbreviated <- abbreviate(result$name)
  if (is.null(main)) {
    model_marker <- model_info[model_id==query_id]$marker_symbol
    main <- paste0(query_id, " (", model_marker, ")")
  } else {
    main <- paste0(query_id, " (", main, ")")
    main <- gsub("\\(\\)", "", main)
  }
  plot_heatmap(head(result, max_rows),
               continuous.cols=c("query", "neighbor_avg"),
               categorical.cols=c("phenotype_class"),
               label.col="name_abbreviated",
               axis.labels=c(query="query",
                             neighbor_avg=paste0("avg of k=", k),
                             phenotype_class="Phenotype"),
               main=main, ...)
  invisible(result)
}



#' draw a boxed legend in a separate plot
wrap_separate_legend <- function(legend_pos, labels, main, width=0.35) {
  par(mai=c(0.1, 0.3, 0.3, 0.2));
  plot(c(0, 1), c(0, 1), xaxs="i", yaxs="i")
  text(0.02, 0.75, main, cex=0.85, adj=c(0, 0), col="#333333")
  add_embedding_legend(legend_pos, labels)
  rect(0, 0, width, 1.3, border="#aaaaaa", col=NA, lwd=0.5)
}