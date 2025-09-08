#' @title Sparse correlation function
#'
#' @description
#' Safe correlation function which returns a sparse matrix.
#'
#' @md
#' @param x Sparse matrix or character vector.
#' @param y Sparse matrix or character vector.
#' @param method Method to use for calculating the correlation coefficient.
#' @param allow_neg Logical. Whether to allow negative values or set them to 0.
#' @param remove_na Logical. Whether to replace NA values with 0.
#' @param remove_inf Logical. Whether to replace infinite values with 1.
#' @param ... Other arguments passed to [stats::cor] function.
#'
#' @return A correlation matrix.
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
    method = "pearson",
    allow_neg = TRUE,
    remove_na = TRUE,
    remove_inf = TRUE,
    ...) {
  if (!inherits(x, "sparseMatrix")) {
    x <- as_matrix(x, return_sparse = TRUE)
  }

  if (!is.null(y)) {
    if (!inherits(y, "sparseMatrix")) {
      y <- as_matrix(y, return_sparse = TRUE)
    }
    if (nrow(x) != nrow(y)) {
      stop("x and y must have the same number of rows.")
    }
  }

  corr_mat <- switch(
    EXPR = method,
    "pearson" = pearson_correlation(x, y)$cor,
    "spearman" = {
      if (is.null(y)) {
        stats::cor(
          as_matrix(x),
          method = "spearman",
          ...
        )
      } else {
        stats::cor(
          as_matrix(x),
          as_matrix(y),
          method = "spearman",
          ...
        )
      }
    },
    "kendall" = {
      if (is.null(y)) {
        stats::cor(
          as_matrix(x),
          method = "kendall",
          ...
        )
      } else {
        stats::cor(
          as_matrix(x),
          as_matrix(y),
          method = "kendall",
          ...
        )
      }
    }
  )

  if (is.null(y)) {
    colnames(corr_mat) <- colnames(x)
  } else {
    colnames(corr_mat) <- colnames(y)
  }
  rownames(corr_mat) <- colnames(x)

  if (remove_na) {
    corr_mat[is.na(corr_mat)] <- 0
  }
  if (remove_inf) {
    corr_mat[is.infinite(corr_mat)] <- 1
  }

  corr_mat <- as_matrix(corr_mat, return_sparse = TRUE)

  if (!allow_neg) {
    corr_mat[corr_mat < 0] <- 0
  }

  return(corr_mat)
}

#' @title Correlation and covariance calculation for sparse matrix
#'
#' @inheritParams sparse_cor
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
pearson_correlation <- function(x, y = NULL) {
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

  n <- nrow(x)
  mu_x <- Matrix::colMeans(x)
  if (is.null(y)) {
    covmat <- (
      (as.matrix(Matrix::crossprod(x)) - n * Matrix::tcrossprod(mu_x)) / (n - 1)
    )
    sdvec <- sqrt(diag(covmat))
    cormat <- covmat / tcrossprod(sdvec)
  } else {
    if (!inherits(y, "sparseMatrix")) {
      log_message(
        "y should be a sparse matrix",
        message_type = "error"
      )
    }
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
