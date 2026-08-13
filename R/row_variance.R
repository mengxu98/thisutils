#' @title Compute row variances
#'
#' @description
#' Compute sample or population variances for each row of a dense or sparse
#' matrix without densifying sparse input.
#'
#' @param x A matrix or a `Matrix` sparse matrix.
#' @param unbiased Whether to compute the unbiased sample variance (the
#'   default) instead of the population variance.
#'
#' @return A numeric vector of row variances. Row names are retained when
#'   available. Matrices with zero columns return `NA` for every row. A
#'   one-column matrix returns `NA` for sample variance and `0` for population
#'   variance.
#'
#' @export
#'
#' @examples
#' fast_row_vars(matrix(c(1, 2, 3, 4), nrow = 2))
fast_row_vars <- function(x, unbiased = TRUE) {
  if (!is.logical(unbiased) || length(unbiased) != 1L || is.na(unbiased)) {
    log_message(
      "{.arg unbiased} must be a single non-missing logical value",
      message_type = "error"
    )
  }

  n <- ncol(x)
  if (is.null(n) || n == 0L || (n == 1L && isTRUE(unbiased))) {
    out <- rep(NA_real_, nrow(x))
    names(out) <- rownames(x)
    return(out)
  }
  if (n == 1L) {
    out <- rep(0, nrow(x))
    names(out) <- rownames(x)
    return(out)
  }

  if (inherits(x, "sparseMatrix")) {
    x <- .as_dgC_matrix(x)
    row_sum <- as.numeric(Matrix::rowSums(x))
    squared <- x
    squared@x <- squared@x * squared@x
    row_sum_squares <- as.numeric(Matrix::rowSums(squared))
  } else {
    x <- as.matrix(x)
    storage.mode(x) <- "double"
    row_sum <- rowSums(x)
    row_sum_squares <- rowSums(x * x)
  }

  correction <- row_sum * row_sum / n
  centered_sum_squares <- row_sum_squares - correction
  cancellation_risk <- which(
    is.finite(row_sum_squares) & row_sum_squares > 0 &
      centered_sum_squares <=
        sqrt(.Machine$double.eps) * row_sum_squares
  )

  denominator <- if (isTRUE(unbiased)) n - 1L else n
  out <- pmax(centered_sum_squares / denominator, 0)
  if (length(cancellation_risk)) {
    stable_input <- x[cancellation_risk, , drop = FALSE]
    out[cancellation_risk] <- if (inherits(x, "sparseMatrix")) {
      row_mean <- as.numeric(Matrix::rowMeans(stable_input))
      stored_per_row <- tabulate(
        stable_input@i + 1L,
        nbins = nrow(stable_input)
      )
      centered <- stable_input
      centered@x <- (
        centered@x - row_mean[centered@i + 1L]
      )^2
      (
        as.numeric(Matrix::rowSums(centered)) +
          (n - stored_per_row) * row_mean^2
      ) / denominator
    } else {
      row_mean <- rowMeans(stable_input)
      centered <- sweep(stable_input, 1L, row_mean, FUN = "-")
      rowSums(centered * centered) / denominator
    }
  }

  names(out) <- rownames(x)
  out
}

#' @title Keep matrix rows with nonzero variance
#'
#' @description
#' Subset a matrix to selected rows and discard rows with missing or zero
#' variance. Sparse input remains sparse.
#'
#' @param x A matrix or a `Matrix` sparse matrix.
#' @param features Row names or indices to retain before variance filtering.
#'
#' @return `x` restricted to selected rows whose variance is positive.
#'
#' @export
#'
#' @examples
#' filter_nonzero_variance_features(
#'   matrix(c(1, 1, 1, 1, 1, 2), nrow = 2),
#'   features = 1:2
#' )
filter_nonzero_variance_features <- function(x, features) {
  if (length(features) == 0L) {
    return(x[features, , drop = FALSE])
  }
  x_sub <- x[features, , drop = FALSE]
  features_var <- fast_row_vars(x_sub)
  keep <- !is.na(features_var) & features_var > 0
  x_sub[keep, , drop = FALSE]
}
