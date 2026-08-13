#' @title Sparse matrix top-k selection
#'
#' @description
#' For each column or row of a sparse `dgCMatrix`, extract the top `k` matrix
#' elements and their indices. By default, unstored positions participate with
#' their matrix value of zero. For column-wise selection, this gives the same
#' semantics as [run_dense_topk()].
#'
#' @md
#' @param x A `dgCMatrix` (or something coercible to one).
#' @param k Number of top entries to retain per column or row. Must be a positive
#'   integer.
#' @param by Direction of selection: `"col"` ranks row entries independently
#'   within each column, while `"row"` ranks column entries independently within
#'   each row.
#' @param decreasing Whether to sort in decreasing order (largest values
#'   first). Default is `TRUE`.
#' @param include_implicit_zeros Whether unstored sparse positions participate
#'   as zeros. The default, `TRUE`, gives ordinary matrix semantics. Set to
#'   `FALSE` to rank only stored entries; for that use case, prefer the more
#'   explicit [run_sparse_topk_stored()].
#'
#' @return A list with two components:
#' \describe{
#'   \item{idx}{Integer matrix of 1-based indices into the opposite dimension.}
#'   \item{value}{Numeric matrix of corresponding values.}
#' }
#' Both matrices have `ncol(x)` rows when `by = "col"` and `nrow(x)` rows
#' when `by = "row"`.
#'
#' @export
#'
#' @examples
#' m <- Matrix::rsparsematrix(10, 20, density = 0.3)
#' run_sparse_topk(m, k = 3, by = "col")
#' run_sparse_topk(m, k = 3, by = "row")
run_sparse_topk <- function(
  x,
  k,
  by = c("col", "row"),
  decreasing = TRUE,
  include_implicit_zeros = TRUE
) {
  x <- .as_dgC_matrix(x)
  by <- match.arg(by)
  if (identical(by, "row")) {
    x <- .as_dgC_matrix(Matrix::t(x))
  }
  sparse_topk_by_column(
    mat = x,
    k = as.integer(k),
    decreasing = isTRUE(decreasing),
    include_implicit_zeros = isTRUE(include_implicit_zeros)
  )
}

#' @title Stored sparse entries top-k selection
#'
#' @md
#'
#' @description
#' For each column or row of a sparse `dgCMatrix`, rank only explicitly stored
#' entries. Unstored zeros never become candidates. This is useful for sparse
#' adjacency matrices, where stored entries represent graph edges rather than
#' samples from a full numeric matrix.
#'
#' @inheritParams run_sparse_topk
#'
#' @return A list with `idx` and `value` matrices as described in
#'   [run_sparse_topk()]. Groups with fewer than `k` stored entries are padded
#'   with `NA` in both matrices.
#'
#' @export
#'
#' @examples
#' graph <- Matrix::sparseMatrix(
#'   i = c(1, 3, 2), j = c(1, 1, 2), x = c(1, 3, -2),
#'   dims = c(3, 2)
#' )
#' run_sparse_topk_stored(graph, k = 2, by = "col")
run_sparse_topk_stored <- function(
  x,
  k,
  by = c("col", "row"),
  decreasing = TRUE
) {
  run_sparse_topk(
    x = x,
    k = k,
    by = by,
    decreasing = decreasing,
    include_implicit_zeros = FALSE
  )
}

#' @title Compatibility wrapper for column-wise sparse top-k selection
#'
#' @md
#'
#' @description
#' `run_sparse_topk_by_column()` preserves the column-wise interface used in
#' earlier releases: only explicitly stored entries participate, and missing
#' ranks are padded with `NA` indices and zero values. New code should call
#' [run_sparse_topk()] or [run_sparse_topk_stored()] to select the intended
#' sparse-zero semantics explicitly.
#'
#' @param x A `dgCMatrix` (or something coercible to one).
#' @param k Number of stored entries to retain per column.
#' @param decreasing Whether to sort in decreasing order. Defaults to `TRUE`.
#'
#' @return A list with `idx` and `value` matrices. Columns with fewer than `k`
#'   stored entries use `NA` indices and zero values for the remaining ranks.
#'
#' @export
run_sparse_topk_by_column <- function(x, k, decreasing = TRUE) {
  result <- run_sparse_topk(
    x = x,
    k = k,
    by = "col",
    decreasing = decreasing,
    include_implicit_zeros = FALSE
  )
  result$value[is.na(result$idx)] <- 0
  result
}

#' @title Compatibility wrapper for stored column-wise sparse top-k selection
#'
#' @md
#'
#' @description
#' `run_sparse_stored_topk_by_column()` preserves the stored-entry column-wise
#' interface used in earlier releases. New code should call
#' [run_sparse_topk_stored()] with `by = "col"`.
#'
#' @inheritParams run_sparse_topk
#'
#' @return A list with `idx` and `value` matrices as described in
#'   [run_sparse_topk_stored()].
#'
#' @export
run_sparse_stored_topk_by_column <- function(x, k, decreasing = TRUE) {
  run_sparse_topk_stored(
    x = x,
    k = k,
    by = "col",
    decreasing = decreasing
  )
}

#' @title Dense matrix top-k selection
#'
#' @description
#' For each column or row of a dense numeric matrix, extract the top `k`
#' elements and their indices. The default returns the largest values; set
#' `decreasing = FALSE` to retain the smallest values, as in nearest-neighbor
#' selection from a distance matrix.
#'
#' @md
#' @param x A numeric matrix (or something coercible to one).
#' @param k Number of entries to retain per column or row. Must be a positive
#'   integer.
#' @param by Direction of selection: `"col"` ranks row entries independently
#'   within each column, while `"row"` ranks column entries independently within
#'   each row.
#' @param decreasing Whether to sort in decreasing order (largest values
#'   first). Default is `TRUE`.
#'
#' @return A list with two components:
#' \describe{
#'   \item{idx}{Integer matrix of 1-based indices into the opposite dimension.}
#'   \item{value}{Numeric matrix of corresponding values.}
#' }
#' Both matrices have `ncol(x)` rows when `by = "col"` and `nrow(x)` rows
#' when `by = "row"`.
#'
#' @export
#'
#' @examples
#' m <- matrix(rnorm(100), nrow = 10)
#' run_dense_topk(m, k = 3, by = "col")
#' run_dense_topk(m, k = 3, by = "row")
#' run_dense_topk(m, k = 3, by = "col", decreasing = FALSE)
run_dense_topk <- function(
  x,
  k,
  by = c("col", "row"),
  decreasing = TRUE
) {
  if (!is.matrix(x)) {
    x <- as.matrix(x)
  }
  storage.mode(x) <- "double"
  by <- match.arg(by)
  if (identical(by, "row")) {
    x <- t(x)
  }
  dense_topk_by_column(
    mat = x,
    k = as.integer(k),
    decreasing = isTRUE(decreasing)
  )
}
