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
#' sparse_matrix <- simulate_sparse_matrix(
#'   1000,
#'   1000,
#'   density = 0.01
#' )
#'
#' system.time(
#'   a <- as.matrix(sparse_matrix)
#' )
#' system.time(
#'   b <- as_matrix(sparse_matrix)
#' )
#' system.time(
#'   c <- as_matrix(sparse_matrix, parallel = TRUE)
#' )
#'
#' identical(a, b)
#' identical(a, c)
#' identical(b, c)
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
