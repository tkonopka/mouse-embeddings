# script part of CE_Tutorial.Rmd
# datasets suitable for tutorial figures


tutorial <- list()



# ############################################################################
# dataset with two orthogonal groups

tutorial_orthogonal_file <- file.path(results.dir, "tutorial-orthogonal.tsv")
if (!file.exists(tutorial_orthogonal_file)) {
  make_tutorial_orthogonal <- function() {
    n <- 40
    result <- data.table(label="tutorial_2", id=seq(1, 2*n),
                         group="A", UMAP_1=0, UMAP_2=0)
    result[1:n, "UMAP_1"] <- rnorm(n, 6, 0.9)
    result[1:n, "UMAP_2"] <- rnorm(n, 0, 0.9)
    result[(n+1):(2*n), "group"] <- "B"
    result[(n+1):(2*n), "UMAP_1"] <- rnorm(n, 0, 0.9)
    result[(n+1):(2*n), "UMAP_2"] <- rnorm(n, 6, 0.9)
    result
  }
  fwrite(make_tutorial_orthogonal(), file=tutorial_orthogonal_file, sep="\t")
}
tutorial$orthogonal <- fread(tutorial_orthogonal_file)


# ############################################################################
# dataset with a large and a small group

tutorial_spread_file <- file.path(results.dir, "tutorial-spread.tsv")
if (!file.exists(tutorial_spread_file)) {
  make_tutorial_spread <- function() {
    n <- 40
    result <- data.table(label="tutorial_spread", id=seq(1, 2.5*n),
                         group="A", UMAP_1=0, UMAP_2=0)
    result[1:n, "UMAP_1"] <- rnorm(n, 4.2, 1)
    result[1:n, "UMAP_2"] <- rnorm(n, 0, 1)
    result[(n+1):(2.5*n), "group"] <- "B"
    result[(n+1):(2.5*n), "UMAP_1"] <- rnorm(1.5*n, 3, 2)
    result[(n+1):(2.5*n), "UMAP_2"] <- rnorm(1.5*n, 6, 1)
    result
  }
  fwrite(make_tutorial_spread(), file=tutorial_spread_file, sep="\t")
}
tutorial$spread <- fread(tutorial_spread_file)



# ############################################################################
# dataset with many small groups

tutorial_many_file <- file.path(results.dir, "tutorial-many.tsv")
if (!file.exists(tutorial_many_file)) {
  make_tutorial_many <- function() {
    n <- 16
    centers <- expand.grid(list(UMAP_1=seq(-1, 7, length=4),
                               UMAP_2=seq(-1, 7, length=4)))
    centers <- centers[seq(2, nrow(centers)-1),]
    centers$group <- paste("G", seq_len(nrow(centers)))
    centers$group[6] <- "A"
    centers <- split(centers, centers$group)
    result <- rbindlist(lapply(centers, function(x) {
      data.table(label="tutorial_many", id=0, group=x$group,
                 UMAP_1=rnorm(n, x$UMAP_1, 0.35),
                 UMAP_2=rnorm(n, x$UMAP_2, 0.35))
    }))
    result
  }
  fwrite(make_tutorial_many(), file=tutorial_many_file, sep="\t")
}
tutorial$many <- fread(tutorial_many_file)

