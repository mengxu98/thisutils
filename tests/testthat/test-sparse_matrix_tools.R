test_that("collapse_sparse_rows aggregates rows by group", {
  mat <- Matrix::Matrix(
    matrix(
      c(1, 0, 2, 0, 3, 4),
      nrow = 3,
      byrow = TRUE,
      dimnames = list(c("a", "b", "c"), c("x", "y"))
    ),
    sparse = TRUE
  )

  out <- collapse_sparse_rows(mat, c("g1", "g1", "g2"))

  expect_s4_class(out, "dgCMatrix")
  expect_identical(rownames(out), c("g1", "g2"))
  expect_equal(
    unname(as.matrix(out)),
    matrix(c(3, 0, 3, 4), nrow = 2, byrow = TRUE)
  )
})

test_that("collapse_sparse_rows drops missing and empty groups", {
  mat <- Matrix::Matrix(
    matrix(
      c(1, 0, 2, 0, 3, 4, 5, 6),
      nrow = 4,
      byrow = TRUE,
      dimnames = list(c("a", "b", "c", "d"), c("x", "y"))
    ),
    sparse = TRUE
  )

  out <- collapse_sparse_rows(mat, c("g1", NA, "", "g2"))

  expect_identical(rownames(out), c("g1", "g2"))
  expect_equal(
    unname(as.matrix(out)),
    matrix(c(1, 0, 5, 6), nrow = 2, byrow = TRUE)
  )
})

test_that("collapse_sparse_rows errors when group length mismatches matrix rows", {
  mat <- Matrix::Matrix(matrix(1:4, nrow = 2), sparse = TRUE)

  expect_error(
    collapse_sparse_rows(mat, "g1"),
    "length must match"
  )
})
