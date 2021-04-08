# functions for computing neighbors (quasi-brute force)
#


#' compute neighbors_upto_distance by brute force
#'
#' @param xm matrix with data, features in columns, items in rows
#' @param max_dist numeric, maximal distance to report
#' @param mc.cores integer, number of cores to use in calculation
#' @param process_rows vector, rows in xm to include in the output
#'
#' @return data table with neighbor pairs: from, to, distance
neighbors_upto_distance_bruteforce <- function(xm, max_dist=1, mc.cores=2,
                                               process_rows=NULL) {
  if (is.null(process_rows)) {
    process_rows <- seq_len(nrow(xm))
  }
  if (is(process_rows, "character")) {
    process_rows <- match(process_rows, rownames(xm))
  }
  # compute neighbors for each item
  dimensions <- seq_len(ncol(xm))
  result <- rbindlist(mclapply(as.list(process_rows), function(i) {
    ivec <- xm[i, ]
    # trim the candidates down using single dimensions (works well in 2d)
    candidates <- xm
    for (d in dimensions) {
      vals <- candidates[, d]
      dlim <- ivec[d] + c(-max_dist, max_dist)
      candidates <- candidates[vals >= dlim[1] & vals <= dlim[2], , drop=FALSE]
    }
    # compute distances between ivec and the candidates
    err2 <- (t(candidates)-ivec)^2
    idist <- sqrt(colSums(err2))
    ihits <- idist <= max_dist
    data.table(from=i, to=rownames(candidates)[ihits], distance=idist[ihits])
  }, mc.cores=mc.cores))
  result <- result[order(from, distance)]
  result$from <- rownames(xm)[result$from]
  result
}


#' attempt to stratify a matrix by a single column
#'
#' @param x numeric matrix
#' @param max_dist numeric, size of slice
#'
#' @return list with information about slices along the dimension
#' that gives the min(max(slice_sizes))
pick_stratification <- function(x, max_dist=1) {
  largest_slice <- rep(0, ncol(x))
  result <- vector("list", ncol(x))
  for (i in seq_len(ncol(x))) {
    vals <- x[,i]
    limits <- range(vals)
    slices <- seq(limits[1], limits[2]+5*max_dist, by=2*max_dist)
    slices <- slices[slices < limits[2]+max_dist]
    slice_sizes <- table(cut(vals, breaks=slices))
    largest_slice[i] <- max(slice_sizes)
    result[[i]] <- list(i=i, vals=vals, slices=slices)
  }
  print(largest_slice)
  result[[which.min(largest_slice)]]
}

#' compute neighbors up to a certain distance (uses dataset slicing)
#'
#' This implementation always uses a euclidean distance
#'
#' @param x matrix or data frame, samples in rows and features in columns
#' @param max_dist numeric, maximal distance
#' @param id_column character, when x is a data frame, id_column is the column
#' that holds items names
#' @param mc.cores integer, number of cores for computation
#'
#' @return data table
neighbors_upto_distance <- function(x, max_dist=1, id_column=NULL, mc.cores=2) {
  # make sure input into a matrix, not a data frame
  if (is(x, "matrix")) {
    xm <- x
  } else {
    xnames <- x[[id_column]]
    xm <- copy(x)
    xm[[id_column]] <- NULL
    xm <- as.matrix(xm)
    rownames(xm) <- xnames
  }

  strat_info <- pick_stratification(xm, max_dist)
  print(strat_info$i)
  vals <- strat_info$vals
  slices <- strat_info$slices
  limits <- range(vals)
  width <- limits[2]-limits[1]
  if (nrow(xm)< 600 | (width < 3*max_dist)) {
    return(neighbors_upto_distance_bruteforce(xm, max_dist, mc.cores))
  }

  # slice the dataset into stripes (only along one dimension)
  d1 <- max_dist
  d2 <- 2*max_dist
  result <- vector("list", length(slices))
  for (i in seq_along(slices)) {
    i_center <- slices[i]
    i_ids <- rownames(xm)[vals>= i_center-d1 & vals < i_center+d1]
    if (length(i_ids)>0) {
      i_data <- xm[vals >= i_center-d2 & vals < i_center+d2, , drop=FALSE]
      i_result <- neighbors_upto_distance_bruteforce(i_data, max_dist=max_dist,
                                                     mc.cores=mc.cores,
                                                     process_rows=i_ids)
      i_result$from <- match(i_result$from, rownames(xm))
      i_result$to <- match(i_result$to, rownames(xm))
      result[[i]] <- i_result
    }
  }
  result <- rbindlist(result)[order(from, distance)]
  result$from <- rownames(xm)[result$from]
  result$to <- rownames(xm)[result$to]
  result
}

