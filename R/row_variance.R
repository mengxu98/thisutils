#' @title Compute unbiased row variances
#'
#' @description
#' Compute unbiased sample variances for each row of a dense or sparse matrix
#' without densifying sparse input.
#'
#' @param x A matrix or a `Matrix` sparse matrix.
#'
#' @return A numeric vector of row variances. Row names are retained when
#'   available. Matrices with zero or one column return `NA` for every row.
#'
#' @export
#'
#' @examples
#' fast_row_vars(matrix(c(1, 2, 3, 4), nrow = 2))
fast_row_vars <- function(x) {
  n <- ncol(x)
  if (is.null(n) || n <= 1L) {
    out <- rep(NA_real_, nrow(x))
    names(out) <- rownames(x)
    return(out)
  }

  if (inherits(x, "sparseMatrix")) {
    if (!inherits(x, "dgCMatrix")) {
      x <- methods::as(x, "dgCMatrix")
    }
    row_sum <- Matrix::rowSums(x)
    x_sq <- x
    x_sq@x <- x_sq@x * x_sq@x
    row_sum_sq <- Matrix::rowSums(x_sq)
  } else {
    x <- as.matrix(x)
    storage.mode(x) <- "double"
    row_sum <- rowSums(x)
    row_sum_sq <- rowSums(x * x)
  }

  out <- (row_sum_sq - (row_sum * row_sum) / n) / (n - 1L)
  out <- pmax(as.numeric(out), 0)
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
