# embeddings for colors


# configuration for embeddings of colors
colors.config <- umap.defaults
colors.config$min_dist <- 0.2
colors.config$metric <- "euclidean"
colors.config$knn_repeats <- 10

templates$color_embedding <- file.path(results.dir,
                                       "embedding-of-colors-{ENCODING}--{ALGO}-d2"),

# column names that define three color systems - ways of enoding colors
color_systems <- list(RGB=c("RGB_red", "RGB_blue", "RGB_green"),
                      HSL=c("Hue", "HSL_saturation", "HSL_light"),
                      HSV=c("Hue", "HSV_saturation", "HSV_value"))


# ############################################################################
# embeddings based on color RGB values

colors_raw_file <- file.path(data.dir, "colors.tsv")
if (!assignc("colors_info")) {
  colors_info <- fread(colors_raw_file)
  colors_info <- colors_info[!duplicated(Name)]
  colors_info <- colors_info[!duplicated(RGB)]
  savec(colors_info)
}


# ############################################################################
# embeddings based on color RGB values

canary_file <- glue(templates$color_embedding, ENCODING="RGB", ALGO="umap")
if (!file.exists(canary_file)) {
  color_embedding <- lapply(names(color_systems), function(color_system) {
    colors_columns <- color_systems[[color_system]]
    colors_data <- as.matrix(colors_info[, colors_columns, with=FALSE])
    for (x in colors_columns) {
      colors_data[, x] <- colors_data[,x] / max(colors_data[,x])
    }
    rownames(colors_data) <- colors_info$Name
    colors_umap <- umap(colors_data, config=colors.config)
    output_file <- glue(templates$color_embedding,
                        ENCODING=color_system, ALGO="umap")
    write_embedding(colors_umap, file=output_file, label=color_system)
  })
}

if (!assignc("colors_embedding")) {
  colors_embedding <- rbindlist(
    lapply(names(color_systems), function(color_system) {
      embedding_file <- glue(templates$color_embedding,
                             ENCODING=color_system, ALGO="umap")
      result <- fread(embedding_file)
      result <- merge(result, colors_info[, c("Name", "RGB")],
                      by.x="id", by.y="Name")
      result
    }))
  savec(colors_embedding)
}

