#' @title Convert sparse matrix into dense matrix
#'
#' @md
#' @param x A matrix.
#' @param parallel Logical value, default is *`FALSE`*.
#' Setting to parallelize the computation with [RcppParallel::setThreadOptions].
#' @param sparse Logical value, default is *`FALSE`*, whether to output a sparse matrix.
#'
#' @return A dense or sparse matrix
#'
#' @export
#'
#' @examples
#' m <- simulate_sparse_matrix(
#'   1000, 1000,
#'   decimal = 3
#' )
#'
#' system.time(
#'   a <- as.matrix(m)
#' )
#' system.time(
#'   b <- as_matrix(m)
#' )
#' system.time(
#'   c <- as_matrix(m, parallel = TRUE)
#' )
#' system.time(
#'   d <- as_matrix(m, sparse = TRUE)
#' )
#'
#' m[1:5, 1:5]
#' a[1:5, 1:5]
#' b[1:5, 1:5]
#' c[1:5, 1:5]
#'
#' identical(a, b)
#' identical(a, c)
#' identical(b, c)
#' identical(a, d)
#' identical(b, d)
as_matrix <- function(
    x,
    parallel = FALSE,
    sparse = FALSE) {
  if (!methods::is(x, "sparseMatrix")) {
    if (sparse) {
      return(
        Matrix::Matrix(
          x,
          sparse = TRUE,
          dimnames = dimnames(x)
        )
      )
    } else {
      return(Matrix::as.matrix(x))
    }
  } else {
    row_pos <- x@i
    col_pos <- findInterval(seq_along(x@x) - 1, x@p[-1])
    if (parallel) {
      matrix <- asMatrixParallel(
        row_pos,
        col_pos,
        x@x,
        x@Dim[1],
        x@Dim[2]
      )
    } else {
      matrix <- asMatrix(
        row_pos,
        col_pos,
        x@x,
        x@Dim[1],
        x@Dim[2]
      )
    }

    attr(matrix, "dimnames") <- list(x@Dimnames[[1]], x@Dimnames[[2]])

    return(matrix)
  }
}
