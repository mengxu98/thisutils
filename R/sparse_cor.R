#' @title Resource-controlled sparse correlation
#'
#' @description
#' Compute correlations from matrix-like input and return a sparse matrix.
#' Pearson cross-products remain sparse and correlations are evaluated in
#' column blocks so block-local working memory is bounded independently of the
#' full output dimensions.
#'
#' @md
#' @param x A numeric matrix or sparse `Matrix` with observations in rows.
#' @param y An optional numeric matrix or sparse `Matrix` with the same number
#'   of rows as `x`.
#' @param method Correlation coefficient: `"pearson"`, `"spearman"`, or
#'   `"kendall"`.
#' @param allow_neg Logical. Whether to allow negative values or set them to 0.
#' @param remove_na Logical. Whether to replace NA values with 0.
#' @param remove_inf Logical. Whether to replace infinite values with 0.
#' @param threshold Non-negative absolute correlation threshold. Values with
#'   absolute magnitude below this threshold are omitted from the sparse output.
#' @param block_size Maximum number of target columns in each Pearson working
#'   block. The effective size may be reduced to honor `max_dense_bytes`.
#' @param max_dense_bytes Maximum estimated bytes allowed for block-local
#'   Pearson working arrays or dense rank-correlation arrays. The default is
#'   `Inf` for backward compatibility. Supply a finite value to enable this
#'   guard.
#' @param max_output_entries Maximum number of stored values allowed in the
#'   sparse result. The default, `Inf`, preserves the historical unbounded
#'   output behavior; set a finite value to fail before returning an output
#'   that exceeds the workflow's storage budget.
#' @param ... Other arguments passed to [stats::cor()] for Spearman and Kendall
#'   correlations.
#'
#' @details
#' Pearson input and cross-products remain sparse. Centering, scaling, and
#' thresholding are fused in a column-block scan rather than materializing a
#' dense correlation block. The final sparse result can nevertheless contain
#' up to `ncol(x) * ncol(y)` stored values when `threshold = 0`; use `threshold`
#' and `max_output_entries` to make this boundary explicit.
#'
#' Spearman and Kendall correlation require ranking and currently densify the
#' input and full correlation result. Their estimated peak working size is
#' checked against `max_dense_bytes` before conversion.
#'
#' @return A sparse correlation matrix.
#'
#' @export
#'
#' @examples
#' m1 <- simulate_sparse_matrix(
#'   500, 100
#' )
#' m2 <- simulate_sparse_matrix(
#'   500, 100,
#'   seed = 2025
#' )
#' a <- sparse_cor(m1)
#' b <- sparse_cor(m1, m2)
#' c <- as_matrix(
#'   cor(as_matrix(m1)),
#'   return_sparse = TRUE
#' )
#' d <- as_matrix(
#'   cor(as_matrix(m1), as_matrix(m2)),
#'   return_sparse = TRUE
#' )
#'
#' a[1:5, 1:5]
#' c[1:5, 1:5]
#' all.equal(a, c)
#'
#' b[1:5, 1:5]
#' d[1:5, 1:5]
#' all.equal(b, d)
#'
#' m1[sample(1:500, 10)] <- NA
#' m2[sample(1:500, 10)] <- NA
#'
#' sparse_cor(m1, m2)[1:5, 1:5]
#'
#' system.time(
#'   sparse_cor(m1)
#' )
#' system.time(
#'   cor(as_matrix(m1))
#' )
#'
#' system.time(
#'   sparse_cor(m1, m2)
#' )
#' system.time(
#'   cor(as_matrix(m1), as_matrix(m2))
#' )
sparse_cor <- function(
    x,
    y = NULL,
    method = c("pearson", "spearman", "kendall"),
    allow_neg = TRUE,
    remove_na = TRUE,
    remove_inf = TRUE,
    threshold = 0,
    block_size = 256L,
    max_dense_bytes = Inf,
    max_output_entries = Inf,
    ...) {
  method <- match.arg(method)
  .validate_sparse_cor_controls(
    allow_neg = allow_neg,
    remove_na = remove_na,
    remove_inf = remove_inf,
    threshold = threshold,
    block_size = block_size,
    max_dense_bytes = max_dense_bytes,
    max_output_entries = max_output_entries
  )

  if (!inherits(x, "sparseMatrix")) {
    x <- as_matrix(x, return_sparse = TRUE)
  }
  x <- .as_dgC_matrix(x)

  if (!is.null(y)) {
    if (!inherits(y, "sparseMatrix")) {
      y <- as_matrix(y, return_sparse = TRUE)
    }
    y <- .as_dgC_matrix(y)
    if (nrow(x) != nrow(y)) {
      log_message(
        "{.arg x} and {.arg y} must have the same number of rows",
        message_type = "error"
      )
    }
  }
  if (nrow(x) < 2L) {
    log_message(
      "At least two observations are required to compute correlations",
      message_type = "error"
    )
  }

  if (identical(method, "pearson")) {
    corr_mat <- .sparse_pearson_cor(
      x = x,
      y = y,
      allow_neg = allow_neg,
      remove_na = remove_na,
      remove_inf = remove_inf,
      threshold = threshold,
      block_size = as.integer(block_size),
      max_dense_bytes = max_dense_bytes,
      max_output_entries = max_output_entries
    )
  } else {
    corr_mat <- .dense_rank_cor(
      x = x,
      y = y,
      method = method,
      allow_neg = allow_neg,
      remove_na = remove_na,
      remove_inf = remove_inf,
      threshold = threshold,
      max_dense_bytes = max_dense_bytes,
      max_output_entries = max_output_entries,
      ...
    )
  }
  corr_mat
}

.validate_sparse_cor_controls <- function(
  allow_neg,
  remove_na,
  remove_inf,
  threshold,
  block_size,
  max_dense_bytes,
  max_output_entries
) {
  logical_controls <- list(
    allow_neg = allow_neg,
    remove_na = remove_na,
    remove_inf = remove_inf
  )
  for (name in names(logical_controls)) {
    value <- logical_controls[[name]]
    if (!is.logical(value) || length(value) != 1L || is.na(value)) {
      log_message(
        "{.arg {name}} must be a single non-missing logical value",
        message_type = "error"
      )
    }
  }
  if (
    !is.numeric(threshold) || length(threshold) != 1L ||
      !is.finite(threshold) || threshold < 0
  ) {
    log_message(
      "{.arg threshold} must be a single finite non-negative number",
      message_type = "error"
    )
  }
  if (
    !is.numeric(block_size) || length(block_size) != 1L ||
      is.na(block_size) || !is.finite(block_size) || block_size < 1 ||
      block_size > .Machine$integer.max || block_size != floor(block_size)
  ) {
    log_message(
      "{.arg block_size} must be a single positive integer",
      message_type = "error"
    )
  }
  for (name in c("max_dense_bytes", "max_output_entries")) {
    value <- get(name)
    if (
      !is.numeric(value) || length(value) != 1L || is.na(value) ||
        value < 0 || (!is.finite(value) && !identical(value, Inf))
    ) {
      log_message(
        "{.arg {name}} must be a single non-negative number or {.val Inf}",
        message_type = "error"
      )
    }
  }
  invisible(NULL)
}

.clean_cor_block <- function(
  x,
  allow_neg,
  remove_na,
  remove_inf,
  threshold
) {
  if (isTRUE(remove_na)) {
    x[is.na(x)] <- 0
  }
  if (isTRUE(remove_inf)) {
    x[is.infinite(x)] <- 0
  }
  if (!isTRUE(allow_neg)) {
    negative <- !is.na(x) & x < 0
    x[negative] <- 0
  }
  if (threshold > 0) {
    below_threshold <- !is.na(x) & abs(x) < threshold
    x[below_threshold] <- 0
  }
  x
}

.sparse_column_moments <- function(x) {
  n <- nrow(x)
  means <- as.numeric(Matrix::colMeans(x))
  stored_per_column <- diff(x@p)
  centered_sq <- x
  centered_sq@x <- (
    centered_sq@x - rep(means, times = stored_per_column)
  )^2
  sum_squares <- as.numeric(Matrix::colSums(centered_sq)) +
    (n - stored_per_column) * means^2
  list(mean = means, sd = sqrt(sum_squares / (n - 1L)))
}

.sparse_pearson_cor <- function(
  x,
  y,
  allow_neg,
  remove_na,
  remove_inf,
  threshold,
  block_size,
  max_dense_bytes,
  max_output_entries
) {
  y_use <- y %ss% x
  n <- nrow(x)
  p <- ncol(x)
  q <- ncol(y_use)
  x_moments <- .sparse_column_moments(x)
  y_moments <- if (is.null(y)) x_moments else .sparse_column_moments(y_use)

  # Sparse cross-products and temporary sparse-output triplets can coexist.
  # Four doubles per potential output cell conservatively bounds the block
  # even when both structures become effectively dense.
  bytes_per_target_column <- 4 * 8 * max(1, p)
  memory_limited_block <- if (is.infinite(max_dense_bytes)) {
    q
  } else {
    floor(max_dense_bytes / bytes_per_target_column)
  }
  if (q > 0L && memory_limited_block < 1L) {
    log_message(
      "{.arg max_dense_bytes} is too small for one Pearson correlation block with {.val {p}} source columns",
      message_type = "error"
    )
  }
  effective_block_size <- max(
    1L,
    min(block_size, max(1L, q), memory_limited_block)
  )

  i_chunks <- list()
  x_chunks <- list()
  output_counts <- integer(q)
  output_entries <- 0
  if (q > 0L && p > 0L) {
    starts <- seq.int(1L, q, by = effective_block_size)
    for (chunk in seq_along(starts)) {
      begin <- starts[[chunk]]
      end <- min(q, begin + effective_block_size - 1L)
      target <- begin:end

      cross_products <- Matrix::crossprod(
        x,
        y_use[, target, drop = FALSE]
      )
      cross_products <- .as_dgC_matrix(cross_products)
      sparse_block <- sparse_pearson_block_cpp(
        cross_products = cross_products,
        x_means = x_moments$mean,
        y_means = y_moments$mean[target],
        x_sds = x_moments$sd,
        y_sds = y_moments$sd[target],
        n_observations = n,
        allow_neg = allow_neg,
        remove_na = remove_na,
        remove_inf = remove_inf,
        threshold = threshold
      )
      block_entries <- length(sparse_block$x)
      output_entries <- output_entries + block_entries
      if (output_entries > max_output_entries) {
        log_message(
          "Pearson correlation output exceeded {.arg max_output_entries} ({.val {max_output_entries}}). Increase {.arg threshold} or the limit.",
          message_type = "error"
        )
      }
      if (block_entries > 0L) {
        i_chunks[[length(i_chunks) + 1L]] <- sparse_block$i
        x_chunks[[length(x_chunks) + 1L]] <- sparse_block$x
      }
      output_counts[target] <- sparse_block$counts
    }
  }

  output_i <- unlist(i_chunks, use.names = FALSE)
  output_x <- unlist(x_chunks, use.names = FALSE)
  if (is.null(output_i)) {
    output_i <- integer()
    output_x <- numeric()
  }
  if (output_entries > .Machine$integer.max) {
    log_message(
      "Pearson correlation output exceeds the maximum number of entries supported by a sparse R matrix",
      message_type = "error"
    )
  }
  output_p <- as.integer(c(0, cumsum(output_counts)))

  result <- methods::new(
    "dgCMatrix",
    i = output_i,
    p = output_p,
    x = output_x,
    Dim = as.integer(c(p, q)),
    Dimnames = list(colnames(x), colnames(y_use))
  )
  if (is.null(y)) {
    result <- Matrix::forceSymmetric(result, uplo = "U")
  }
  result
}

.dense_rank_cor <- function(
  x,
  y,
  method,
  allow_neg,
  remove_na,
  remove_inf,
  threshold,
  max_dense_bytes,
  max_output_entries,
  ...
) {
  y_use <- y %ss% x
  n <- nrow(x)
  p <- ncol(x)
  q <- ncol(y_use)
  estimated_bytes <- 4 * 8 * (n * p + if (is.null(y)) 0 else n * q + p * q)
  if (estimated_bytes > max_dense_bytes) {
    log_message(
      "Estimated dense working memory for {.val {method}} correlation is {format(round(estimated_bytes / 1024^2, 1), nsmall = 1)} MiB, above {.arg max_dense_bytes}. Use Pearson blocks, reduce the inputs, or raise the limit explicitly.",
      message_type = "error"
    )
  }

  corr_mat <- if (is.null(y)) {
    stats::cor(as_matrix(x), method = method, ...)
  } else {
    stats::cor(as_matrix(x), as_matrix(y), method = method, ...)
  }
  corr_mat <- .clean_cor_block(
    corr_mat,
    allow_neg = allow_neg,
    remove_na = remove_na,
    remove_inf = remove_inf,
    threshold = threshold
  )
  corr_mat <- Matrix::drop0(Matrix::Matrix(corr_mat, sparse = TRUE))
  corr_mat <- if (is.null(y)) {
    Matrix::forceSymmetric(corr_mat, uplo = "U")
  } else {
    methods::as(corr_mat, "generalMatrix")
  }
  if (length(corr_mat@x) > max_output_entries) {
    log_message(
      "Correlation output exceeded {.arg max_output_entries} ({.val {max_output_entries}}). Increase {.arg threshold} or the limit.",
      message_type = "error"
    )
  }
  dimnames(corr_mat) <- list(colnames(x), colnames(y_use))
  corr_mat
}

#' @title Correlation and covariance calculation for sparse matrix
#'
#' @inheritParams sparse_cor
#' @param max_dense_bytes Maximum estimated bytes allowed for the dense
#'   covariance, correlation, and cross-product matrices. The default is
#'   `Inf` for backward compatibility. Supply a finite value to enable the
#'   guard.
#'
#' @details
#' This lower-level helper deliberately returns dense covariance and
#' correlation matrices and therefore requires memory proportional to
#' `ncol(x) * ncol(y)`. Prefer [sparse_cor()] when a sparse, blockwise result is
#' sufficient.
#'
#' @return A list with covariance and correlation matrices.
#'
#' @export
#'
#' @examples
#' m1 <- simulate_sparse_matrix(
#'   100, 100
#' )
#' m2 <- simulate_sparse_matrix(
#'   100, 100,
#'   sparsity = 0.05
#' )
#' a <- pearson_correlation(m1, m2)
#' a$cov[1:5, 1:5]
#' a$cor[1:5, 1:5]
pearson_correlation <- function(
  x,
  y = NULL,
  max_dense_bytes = Inf
) {
  if (!inherits(x, "sparseMatrix")) {
    log_message(
      "x should be a sparse matrix",
      message_type = "error"
    )
  }
  if (!is.null(y) && !inherits(y, "sparseMatrix")) {
    log_message(
      "y should be a sparse matrix",
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
  x <- .as_dgC_matrix(x)
  if (!is.null(y)) {
    y <- .as_dgC_matrix(y)
  }

  n <- nrow(x)
  if (n < 2L) {
    log_message(
      "At least two observations are required to compute correlations",
      message_type = "error"
    )
  }
  q <- if (is.null(y)) ncol(x) else ncol(y)
  estimated_bytes <- 3 * 8 * ncol(x) * q
  if (estimated_bytes > max_dense_bytes) {
    log_message(
      "Estimated dense output and working memory is {format(round(estimated_bytes / 1024^2, 1), nsmall = 1)} MiB, above {.arg max_dense_bytes}. Use {.fn sparse_cor} or raise the limit explicitly.",
      message_type = "error"
    )
  }

  mu_x <- Matrix::colMeans(x)
  if (is.null(y)) {
    covmat <- (
      (as.matrix(Matrix::crossprod(x)) - n * Matrix::tcrossprod(mu_x)) / (n - 1)
    )
    sdvec <- sqrt(diag(covmat))
    cormat <- covmat / tcrossprod(sdvec)
  } else {
    if (nrow(x) != nrow(y)) {
      log_message(
        "x and y should have the same number of rows",
        message_type = "error"
      )
    }

    mu_y <- Matrix::colMeans(y)
    covmat <- (
      (as.matrix(Matrix::crossprod(x, y)) - n * Matrix::tcrossprod(mu_x, mu_y)) / (n - 1)
    )
    sdvecX <- sqrt((Matrix::colSums(x^2) - n * mu_x^2) / (n - 1))
    sdvecY <- sqrt((Matrix::colSums(y^2) - n * mu_y^2) / (n - 1))
    cormat <- covmat / Matrix::tcrossprod(sdvecX, sdvecY)
  }

  return(
    list(
      cov = covmat,
      cor = cormat
    )
  )
}
