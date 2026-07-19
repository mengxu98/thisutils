#' @title Find nearest neighbors with BiocNeighbors
#'
#' @description
#' Find nearest neighbors within a reference matrix or from a query matrix to a
#' reference matrix. The wrapper standardizes matrix coercion, distance metric,
#' and self-neighbor removal across consumers.
#'
#' @param reference Numeric reference matrix with observations in rows.
#' @param query Optional numeric query matrix with the same number of columns.
#' @param k Number of nearest neighbors to return.
#' @param metric Distance metric: `"euclidean"` or `"cosine"`.
#' @param exclude_self Whether to remove self-neighbors when `query` is `NULL`.
#' @param n_threads Number of BiocNeighbors threads.
#'
#' @return A list with integer matrix `idx` and numeric matrix `dist`.
#'
#' @export
#'
#' @examples
#' if (requireNamespace("BiocNeighbors", quietly = TRUE)) {
#'   run_biocneighbors_knn(matrix(rnorm(20), ncol = 2), k = 2)
#' }
run_biocneighbors_knn <- function(
  reference,
  query = NULL,
  k,
  metric = c("euclidean", "cosine"),
  exclude_self = FALSE,
  n_threads = 1L
) {
  check_r("BiocNeighbors", verbose = FALSE)
  metric <- match.arg(metric)
  distance <- switch(metric,
    euclidean = "Euclidean",
    cosine = "Cosine"
  )
  reference <- as.matrix(reference)
  storage.mode(reference) <- "double"
  k <- as.integer(k)
  if (is.null(query)) {
    k_search <- if (isTRUE(exclude_self)) min(k + 1L, nrow(reference)) else k
    knn <- BiocNeighbors::findKNN(
      reference,
      k = k_search,
      BNPARAM = BiocNeighbors::KmknnParam(distance = distance),
      num.threads = as.integer(n_threads)
    )
  } else {
    query <- as.matrix(query)
    storage.mode(query) <- "double"
    if (ncol(reference) != ncol(query)) {
      log_message(
        "{.arg reference} and {.arg query} must have the same number of columns",
        message_type = "error"
      )
    }
    knn <- BiocNeighbors::queryKNN(
      X = reference,
      query = query,
      k = k,
      BNPARAM = BiocNeighbors::KmknnParam(distance = distance),
      num.threads = as.integer(n_threads)
    )
  }
  idx <- knn[["index"]]
  dist <- knn[["distance"]]
  if (isTRUE(exclude_self) && is.null(query)) {
    idx_out <- matrix(NA_integer_, nrow = nrow(idx), ncol = k)
    dist_out <- matrix(NA_real_, nrow = nrow(dist), ncol = k)
    for (i in seq_len(nrow(idx))) {
      keep <- which(idx[i, ] != i)
      keep <- keep[seq_len(min(k, length(keep)))]
      if (length(keep) > 0L) {
        idx_out[i, seq_along(keep)] <- idx[i, keep]
        dist_out[i, seq_along(keep)] <- dist[i, keep]
      }
    }
    idx <- idx_out
    dist <- dist_out
  }
  list(idx = idx, dist = dist)
}
