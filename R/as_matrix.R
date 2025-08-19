#' @title Convert matrix into dense/sparse matrix
#'
#' @md
#' @param x A matrix.
#' @param return_sparse Whether to output a sparse matrix.
#' Default is `FALSE`.
#'
#' @return
#' A dense or sparse matrix.
#'
#' @export
#'
#' @examples
#' m <- simulate_sparse_matrix(
#'   1000, 1000,
#'   decimal = 3
#' )
#'
#' a <- as_matrix(m)
#' a[1:5, 1:5]
#'
#' b <- as_matrix(m, return_sparse = TRUE)
#' b[1:5, 1:5]
as_matrix <- function(
    x,
    return_sparse = FALSE) {
  if (!methods::is(x, "sparseMatrix")) {
    if (return_sparse) {
      return(
        Matrix::Matrix(x, sparse = TRUE, dimnames = dimnames(x))
      )
    } else {
      return(
        Matrix::as.matrix(x)
      )
    }
  }

  if (return_sparse) {
    return(x)
  }

  return(
    Matrix::as.matrix(x)
  )
}
