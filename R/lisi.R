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
#' @param nn_eps Approximation factor passed to [RANN::nn2] when `RANN` is available and `use_rann = TRUE`.
#' Defaults to `0`.
#' @param use_rann Whether to prefer [RANN::nn2] over `FNN::get.knn` when `nn_method = "auto"` decides not to use the package's built-in exact C++ backend.
#' Defaults to `TRUE`.
#' @param nn_method Nearest-neighbor backend. Defaults to `"auto"`,
#' which uses a simple heuristic: low-dimensional inputs use the package's exact C++ search,
#' while larger/higher-dimensional inputs fall back to `RANN`, then `FNN`,
#' then the built-in exact C++ backend.
#' Set to `"hnsw"` to use `RcppHNSW` for a faster, approximate search.
#' @param nn_threads Number of threads used by the HNSW backend.
#' Defaults to `0` which lets `RcppHNSW` choose automatically.
#' @param hnsw_ef Search breadth used by the HNSW backend.
#' Larger values are more accurate but slower. Defaults to `100`.
#' @param tol Tolerance used in the binary search for the target perplexity.
#' Defaults to `1e-5`.
#' @param max_iter Maximum number of binary-search iterations.
#' Defaults to `50`.
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
#'   matrix(stats::rnorm(100, mean = -2), ncol = 5),
#'   matrix(stats::rnorm(100, mean = 2), ncol = 5)
#' )
#' meta_data <- data.frame(
#'   batch = rep(c("A", "B"), each = 20),
#'   group = sample(c("g1", "g2"), 40, replace = TRUE)
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
  nn_eps = 0,
  use_rann = TRUE,
  nn_method = c("auto", "rann", "fnn", "hnsw", "exact"),
  nn_threads = 0,
  hnsw_ef = 100,
  tol = 1e-5,
  max_iter = 50
) {
  X <- validate_lisi_input_matrix(X)
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

  nn_method <- match.arg(nn_method)
  n_neighbors <- min(
    nrow(X) - 1L,
    max(1L, as.integer(ceiling(perplexity * 3)) - 1L)
  )
  knn <- lisi_knn(
    X = X,
    n_neighbors = n_neighbors,
    nn_eps = nn_eps,
    use_rann = use_rann,
    nn_method = nn_method,
    nn_threads = nn_threads,
    hnsw_ef = hnsw_ef
  )

  out <- lapply(label_colnames, function(label_colname) {
    labels <- meta_data[[label_colname]]

    if (any(is.na(labels))) {
      log_message(
        "Cannot compute {.pkg LISI} for {.val {label_colname}} because it contains missing values",
        message_type = "warning"
      )
      return(rep(NA_real_, nrow(X)))
    }

    label_ids <- as.integer(factor(labels))
    simpson <- compute_simpson_index(
      D = t(knn$nn.dists),
      knn_idx = t(knn$nn.idx),
      batch_labels = label_ids,
      perplexity = perplexity,
      tol = tol,
      max_iter = max_iter
    )

    1 / simpson
  })

  out <- as.data.frame(out, optional = TRUE, stringsAsFactors = FALSE)
  colnames(out) <- label_colnames
  rownames(out) <- rownames(meta_data) %ss% rownames(X)
  out
}

#' @title Compute Simpson index from a KNN graph
#'
#' @description
#' Given nearest-neighbor distances, nearest-neighbor indices,
#' and a categorical label for each cell,
#' compute the local Simpson index of each cell after matching the target perplexity.
#'
#' @md
#' @param D Numeric matrix of nearest-neighbor distances with neighbors in rows
#'   and cells in columns.
#' @param knn_idx Integer matrix of nearest-neighbor indices with the same shape as `D`.
#' One-based indices are expected; zero-based indices are also accepted and converted automatically.
#' @param batch_labels Integer-like label vector of length equal to the number of cells.
#' @param perplexity Effective neighborhood size.
#' Defaults to `15`.
#' @param tol Tolerance used in the binary search for the target perplexity.
#' Defaults to `1e-5`.
#' @param max_iter Maximum number of binary-search iterations.
#' Defaults to `50`.
#'
#' @return A numeric vector containing the local Simpson index for each cell.
#' @export
#'
#' @examples
#' D <- matrix(
#'   c(0.1, 0.2, 0.2, 0.1,
#'     0.3, 0.4, 0.4, 0.3),
#'   nrow = 2,
#'   byrow = TRUE
#' )
#' knn_idx <- matrix(
#'   c(2, 1, 4, 3,
#'     3, 4, 2, 1),
#'   nrow = 2,
#'   byrow = TRUE
#' )
#' batch_labels <- c(1, 1, 2, 2)
#' compute_simpson_index(D, knn_idx, batch_labels, perplexity = 2)
compute_simpson_index <- function(
  D,
  knn_idx,
  batch_labels,
  perplexity = 15,
  tol = 1e-5,
  max_iter = 50
) {
  D <- as.matrix(D)
  knn_idx <- as.matrix(knn_idx)
  batch_labels <- as.integer(as.vector(batch_labels))

  if (!is.numeric(D)) {
    log_message(
      "D must be a numeric matrix",
      message_type = "error"
    )
  }
  if (!is.numeric(knn_idx)) {
    log_message(
      "knn_idx must be an integer/numeric matrix",
      message_type = "error"
    )
  }
  if (!identical(dim(D), dim(knn_idx))) {
    log_message(
      "D and knn_idx must have the same dimensions",
      message_type = "error"
    )
  }
  if (ncol(D) != length(batch_labels)) {
    log_message(
      "length(batch_labels) must equal ncol(D)",
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
      "perplexity must be a single positive number",
      message_type = "error"
    )
  }

  storage.mode(knn_idx) <- "integer"

  compute_simpson_index_cpp(
    D = D,
    knn_idx = knn_idx,
    batch_labels = batch_labels,
    perplexity = perplexity,
    tol = tol,
    max_iter = as.integer(max_iter)
  )
}

validate_lisi_input_matrix <- function(X) {
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

  X
}

lisi_knn <- function(
  X,
  n_neighbors,
  nn_eps = 0,
  use_rann = TRUE,
  nn_method = "auto",
  nn_threads = 0,
  hnsw_ef = 100
) {
  include_self <- FALSE
  request_k <- as.integer(n_neighbors)

  if (identical(nn_method, "auto")) {
    if (nrow(X) <= 10000L && ncol(X) <= 50L) {
      nn_method <- "exact"
    } else if (isTRUE(use_rann) && requireNamespace("RANN", quietly = TRUE)) {
      nn_method <- "rann"
    } else if (requireNamespace("FNN", quietly = TRUE)) {
      nn_method <- "fnn"
    } else {
      nn_method <- "exact"
    }
  }

  log_message(
    "Using {.val {nn_method}} nearest-neighbor backend for {.pkg compute_lisi}",
    message_type = "running"
  )

  if (identical(nn_method, "hnsw")) {
    log_message(
      "{.val hnsw} uses approximate nearest neighbors and may slightly change LISI values",
      message_type = "warning"
    )
  }

  knn <- switch(
    EXPR = nn_method,
    "rann" = {
      include_self <- TRUE
      request_k <- request_k + 1L
      RANN::nn2(data = X, query = X, k = request_k, eps = nn_eps)
    },
    "fnn" = {
      res <- FNN::get.knn(X, k = request_k)
      list(nn.idx = res$nn.index, nn.dists = res$nn.dist)
    },
    "hnsw" = {
      include_self <- TRUE
      request_k <- request_k + 1L
      res <- RcppHNSW::hnsw_knn(
        X,
        k = request_k,
        distance = "euclidean",
        ef = max(as.integer(hnsw_ef), request_k),
        n_threads = as.integer(nn_threads)
      )
      list(nn.idx = res$idx, nn.dists = res$dist)
    },
    "exact" = {
      include_self <- TRUE
      request_k <- request_k + 1L
      lisi_exact_knn_cpp(X, as.integer(request_k))
    },
    log_message(
      "Unsupported nn_method: {.val {nn_method}}",
      message_type = "error"
    )
  )

  if (include_self) {
    storage.mode(knn$nn.idx) <- "integer"
    knn <- drop_self_from_knn_cpp(knn$nn.idx, as.matrix(knn$nn.dists))
  }

  knn
}
