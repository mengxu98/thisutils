#' @title Compute Local Inverse Simpson's Index (LISI)
#'
#' @description
#' Compute per-cell Local Inverse Simpson's Index (LISI) scores for one or more categorical variables.
#' This is a clean-room reimplementation of the `immunogenomics/LISI`.
#'
#' @md
#' @param X A matrix-like object with cells in rows and embedding/features in columns.
#' @param meta_data A data frame with one row per cell.
#' @param label_colnames Character vector of column names in `meta_data` to evaluate.
#' @param perplexity Effective neighborhood size. Defaults to `30`.
#' @param tol Tolerance used in the binary search for the target perplexity.
#' Defaults to `1e-5`.
#' @param max_iter Maximum number of binary-search iterations.
#' Defaults to `50`.
#' @param knn_algorithm Exact nearest-neighbor search strategy. `"auto"`
#'   uses clustered exact search when there are at least 4096 observations and
#'   the requested neighborhood is less than one quarter of the data; otherwise
#'   it uses brute force. `"brute_force"` and `"clustered"` force either exact
#'   strategy.
#' @param n_threads Number of C++ worker threads. `NULL` (the default) preserves
#'   the earlier automatic hardware-thread selection. Supply a positive integer
#'   to control CPU use explicitly.
#' @param max_dense_bytes Maximum estimated bytes allowed for the dense input
#'   and its C++ row-major copy. The default, `Inf`, preserves earlier behavior;
#'   supply a finite value to enable the guard.
#'
#' @details
#' Both nearest-neighbor strategies are exact. Sparse `X` is converted to a
#' dense matrix before neighbor search; this conversion is included in the
#' `max_dense_bytes` check.
#'
#' @return A data frame with one row per cell and one column per label.
#' @export
#'
#' @references
#' Korsunsky I, Millard N, Fan J, et al. Fast, sensitive and accurate
#' integration of single-cell data with Harmony. *Nature Methods* (2019).
#' \url{https://www.nature.com/articles/s41592-019-0619-0}
#'
#' LISI reference implementation:
#' \url{https://github.com/immunogenomics/LISI}
#'
#' @examples
#' set.seed(1)
#' X <- rbind(
#'   matrix(stats::rnorm(100, mean = -1), ncol = 2),
#'   matrix(stats::rnorm(100, mean = 1), ncol = 2)
#' )
#' meta_data <- data.frame(
#'   batch = rep(c("A", "B"), each = 50),
#'   group = sample(c("g1", "g2"), 100, replace = TRUE)
#' )
#'
#' res <- compute_lisi(
#'   X, meta_data,
#'   c("batch", "group"),
#'   perplexity = 10
#' )
#' head(res)
#' boxplot(res)
compute_lisi <- function(
  X,
  meta_data,
  label_colnames,
  perplexity = 30,
  tol = 1e-5,
  max_iter = 50,
  knn_algorithm = c("auto", "brute_force", "clustered"),
  n_threads = NULL,
  max_dense_bytes = Inf
) {
  knn_algorithm <- match.arg(knn_algorithm)
  if (
    !is.null(n_threads) && (
      !is.numeric(n_threads) || length(n_threads) != 1L ||
        is.na(n_threads) || !is.finite(n_threads) || n_threads < 1 ||
        n_threads > .Machine$integer.max || n_threads != floor(n_threads)
    )
  ) {
    log_message(
      "{.arg n_threads} must be NULL or a single positive integer",
      message_type = "error"
    )
  }
  if (
    !is.numeric(max_dense_bytes) || length(max_dense_bytes) != 1L ||
      is.na(max_dense_bytes) || max_dense_bytes < 0 ||
      (!is.finite(max_dense_bytes) && !identical(max_dense_bytes, Inf))
  ) {
    log_message(
      "{.arg max_dense_bytes} must be a single non-negative number or {.val Inf}",
      message_type = "error"
    )
  }
  if (is.null(dim(X))) {
    X <- as.matrix(X)
  }
  estimated_dense_bytes <- 2 * 8 * prod(dim(X))
  if (estimated_dense_bytes > max_dense_bytes) {
    log_message(
      "Estimated dense input and C++ copy require {format(round(estimated_dense_bytes / 1024^2, 1), nsmall = 1)} MiB, above {.arg max_dense_bytes}",
      message_type = "error"
    )
  }
  if (inherits(X, "sparseMatrix")) {
    X <- Matrix::as.matrix(X)
  }
  X <- as.matrix(X)
  if (!is.numeric(X)) {
    log_message(
      "X must be numeric",
      message_type = "error"
    )
  }
  meta_data <- as.data.frame(meta_data, stringsAsFactors = FALSE)

  if (nrow(X) != nrow(meta_data)) {
    log_message(
      "{.arg X} and {.arg meta_data} must have the same number of rows",
      message_type = "error"
    )
  }
  if (length(label_colnames) == 0) {
    log_message(
      "{.arg label_colnames} must contain at least one column name",
      message_type = "error"
    )
  }
  if (!all(label_colnames %in% colnames(meta_data))) {
    missing_cols <- setdiff(label_colnames, colnames(meta_data))
    log_message(
      "The following label columns are missing: {.val {missing_cols}}",
      message_type = "error"
    )
  }
  if (
    !is.numeric(perplexity) ||
      length(perplexity) != 1 ||
      is.na(perplexity) ||
      perplexity <= 0
  ) {
    log_message(
      "{.arg perplexity} must be a single positive number",
      message_type = "error"
    )
  }
  if (nrow(X) < 2) {
    log_message(
      "At least two cells are required to compute {.pkg LISI}",
      message_type = "error"
    )
  }

  n_neighbors <- min(
    nrow(X) - 1L,
    max(1L, as.integer(ceiling(perplexity * 3)) - 1L)
  )
  label_ids <- matrix(
    NA_integer_,
    nrow = nrow(X),
    ncol = length(label_colnames)
  )
  valid_label <- rep(TRUE, length(label_colnames))
  for (i in seq_along(label_colnames)) {
    label_colname <- label_colnames[[i]]
    labels <- meta_data[[label_colname]]

    if (any(is.na(labels))) {
      log_message(
        "Cannot compute {.pkg LISI} for {.val {label_colname}} because it contains missing values",
        message_type = "warning"
      )
      valid_label[[i]] <- FALSE
    } else {
      label_ids[, i] <- as.integer(factor(labels))
    }
  }

  lisi_mat <- matrix(NA_real_, nrow = nrow(X), ncol = length(label_colnames))
  if (any(valid_label)) {
    lisi_mat[, valid_label] <- compute_lisi_matrix(
      X = X,
      batch_labels = label_ids[, valid_label, drop = FALSE],
      n_neighbors = n_neighbors,
      perplexity = perplexity,
      tol = tol,
      max_iter = max_iter,
      knn_algorithm = knn_algorithm,
      n_threads = if (is.null(n_threads)) 0L else as.integer(n_threads)
    )
  }

  out <- as.data.frame(lisi_mat, optional = TRUE, stringsAsFactors = FALSE)
  colnames(out) <- label_colnames
  rownames(out) <- rownames(meta_data) %ss% rownames(X)
  out
}
