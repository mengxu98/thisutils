test_that("fast_row_vars matches stats::var for dense and sparse matrices", {
  dense <- matrix(
    c(1, 2, 3, 4, 5, 6, 0, 0, 0),
    nrow = 3,
    dimnames = list(c("a", "b", "c"), NULL)
  )
  expected <- apply(dense, 1, stats::var)

  expect_equal(fast_row_vars(dense), expected, tolerance = 1e-14)
  expect_equal(
    fast_row_vars(Matrix::Matrix(dense, sparse = TRUE)),
    expected,
    tolerance = 1e-14
  )
  expect_identical(names(fast_row_vars(dense)), rownames(dense))
})

test_that("row-variance helpers preserve edge cases and sparse type", {
  one_col <- Matrix::Matrix(
    matrix(c(1, 2), dimnames = list(c("a", "b"), "sample")),
    sparse = TRUE
  )
  expect_identical(fast_row_vars(one_col), c(a = NA_real_, b = NA_real_))

  mat <- Matrix::Matrix(
    matrix(c(1, 1, 1, 1, 1, 2), nrow = 2),
    sparse = TRUE
  )
  filtered <- filter_nonzero_variance_features(mat, 1:2)
  expect_s4_class(filtered, "dgCMatrix")
  expect_identical(dim(filtered), c(1L, 3L))
  expect_identical(filter_nonzero_variance_features(mat, integer()), mat[integer(), , drop = FALSE])
})
