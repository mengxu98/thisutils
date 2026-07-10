#' @title Sparse matrix top-k by column
#'
#' @description
#' For each column of a sparse `dgCMatrix`, extract the top `k` entries (rows
#' and values) sorted by value.
#'
#' @md
#' @param x A `dgCMatrix` (or something coercible to one).
#' @param k Number of top entries to retain per column. Must be a positive
#'   integer.
#' @param decreasing Whether to sort in decreasing order (largest values
#'   first). Default is `TRUE`.
#'
#' @return A list with two components:
#' \describe{
#'   \item{idx}{Integer matrix (`ncol × k`) of 1-based row indices.}
#'   \item{value}{Numeric matrix (`ncol × k`) of corresponding values.}
#' }
#'
#' @export
#'
#' @examples
#' m <- Matrix::rsparsematrix(10, 20, density = 0.3)
#' run_sparse_topk_by_column(m, k = 3)
run_sparse_topk_by_column <- function(x, k, decreasing = TRUE) {
  if (!inherits(x, "dgCMatrix")) {
    x <- methods::as(Matrix::Matrix(x, sparse = TRUE), "dgCMatrix")
  }
  sparse_topk_by_column(
    mat = x,
    k = as.integer(k),
    decreasing = isTRUE(decreasing)
  )
}

#' @title Dense matrix top-k by column
#'
#' @description
#' For each column of a dense numeric matrix, extract the top `k` entries
#' (rows and values) sorted by value.
#'
#' @md
#' @param x A numeric matrix (or something coercible to one).
#' @param k Number of top entries to retain per column. Must be a positive
#'   integer.
#' @param decreasing Whether to sort in decreasing order (largest values
#'   first). Default is `FALSE`.
#'
#' @return A list with two components:
#' \describe{
#'   \item{idx}{Integer matrix (`ncol × k`) of 1-based row indices.}
#'   \item{value}{Numeric matrix (`ncol × k`) of corresponding values.}
#' }
#'
#' @export
#'
#' @examples
#' m <- matrix(rnorm(100), nrow = 10)
#' run_dense_topk_by_column(m, k = 3)
run_dense_topk_by_column <- function(x, k, decreasing = FALSE) {
  if (!is.matrix(x)) {
    x <- as.matrix(x)
  }
  storage.mode(x) <- "double"
  dense_topk_by_column(
    mat = x,
    k = as.integer(k),
    decreasing = isTRUE(decreasing)
  )
}
