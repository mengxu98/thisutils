#' @title Collapse sparse matrix rows by group
#'
#' @param matrix A sparse matrix.
#' @param group A vector defining the output row groups.
#'
#' @return A sparse matrix with rows collapsed by `group`.
#'
#' @export
#' @examples
#' mat <- Matrix::Matrix(
#'   matrix(c(1, 0, 2, 0, 3, 4), nrow = 3, byrow = TRUE),
#'   sparse = TRUE
#' )
#' collapse_sparse_rows(mat, c("g1", "g1", "g2"))
collapse_sparse_rows <- function(matrix, group) {
  if (length(group) != nrow(matrix)) {
    cli::cli_abort(
      "{.arg group} length must match the number of rows of {.arg matrix}"
    )
  }

  group <- as.character(group)
  keep <- !is.na(group) & nzchar(group)
  matrix <- matrix[keep, , drop = FALSE]
  group <- group[keep]

  levels_use <- unique(group)
  matrix_summary <- Matrix::summary(matrix)
  i_new <- match(group[matrix_summary$i], levels_use)

  Matrix::sparseMatrix(
    i = i_new,
    j = matrix_summary$j,
    x = matrix_summary$x,
    dims = c(length(levels_use), ncol(matrix)),
    dimnames = list(levels_use, colnames(matrix))
  )
}
