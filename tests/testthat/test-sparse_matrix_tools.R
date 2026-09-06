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

test_that("col_maxs and row_maxs match dense maxima and honor implicit zeros", {
  dense <- matrix(
    c(
      -3, 0, NA, 2,
      0, -4, 0, 1,
      -1, 0, 5, 0
    ),
    nrow = 3,
    byrow = TRUE,
    dimnames = list(paste0("r", 1:3), paste0("c", 1:4))
  )
  sparse <- Matrix::Matrix(dense, sparse = TRUE)

  expect_equal(
    col_maxs(sparse),
    apply(dense, 2, max, na.rm = TRUE)
  )
  expect_equal(
    row_maxs(sparse),
    apply(dense, 1, max, na.rm = TRUE)
  )
  expect_identical(names(col_maxs(sparse)), colnames(dense))
  expect_identical(names(row_maxs(sparse)), rownames(dense))

  sparse_neg <- Matrix::sparseMatrix(
    i = 1L,
    j = 1L,
    x = -5,
    dims = c(3, 2),
    dimnames = list(c("r1", "r2", "r3"), c("c1", "c2"))
  )
  expect_equal(unname(col_maxs(sparse_neg)), c(0, 0))
  expect_equal(unname(row_maxs(sparse_neg)), c(0, 0, 0))
})

test_that("large sparse maxima do not require a dense representation", {
  sparse <- Matrix::sparseMatrix(
    i = c(1L, 50000L),
    j = c(1L, 20000L),
    x = c(1, 2),
    dims = c(50000L, 20000L)
  )

  column_maxima <- col_maxs(sparse)
  row_maxima <- row_maxs(sparse)
  expect_length(column_maxima, 20000L)
  expect_length(row_maxima, 50000L)
  expect_equal(column_maxima[c(1L, 20000L)], c(1, 2))
  expect_equal(row_maxima[c(1L, 50000L)], c(1, 2))
  expect_equal(sum(column_maxima != 0), 2L)
  expect_equal(sum(row_maxima != 0), 2L)
})

test_that("aggregate_matrix and fast_aggregate collapse rows by group", {
  mat <- matrix(
    c(1, 2, 3, 4, 5, 6),
    nrow = 3,
    byrow = TRUE,
    dimnames = list(c("r1", "r2", "r3"), c("x", "y"))
  )
  groups <- c("a", "a", "b")

  summed <- as.matrix(fast_aggregate(mat, groupings = groups, fun = "sum"))
  expect_equal(unname(summed["a", ]), c(4, 6))
  expect_equal(unname(summed["b", ]), c(5, 6))

  averaged <- as.matrix(aggregate_matrix(mat, groups = groups, fun = "mean"))
  expect_equal(unname(averaged["a", ]), c(2, 3))
  expect_equal(unname(averaged["b", ]), c(5, 6))
})
