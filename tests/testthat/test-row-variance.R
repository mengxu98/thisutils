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

  expected_population <- apply(dense, 1, function(x) mean((x - mean(x))^2))
  expect_equal(
    fast_row_vars(dense, unbiased = FALSE),
    expected_population,
    tolerance = 1e-14
  )
  expect_equal(
    fast_row_vars(Matrix::Matrix(dense, sparse = TRUE), unbiased = FALSE),
    expected_population,
    tolerance = 1e-14
  )
})

test_that("row-variance helpers preserve edge cases and sparse type", {
  one_col <- Matrix::Matrix(
    matrix(c(1, 2), dimnames = list(c("a", "b"), "sample")),
    sparse = TRUE
  )
  expect_identical(fast_row_vars(one_col), c(a = NA_real_, b = NA_real_))
  expect_identical(
    fast_row_vars(one_col, unbiased = FALSE),
    c(a = 0, b = 0)
  )

  mat <- Matrix::Matrix(
    matrix(c(1, 1, 1, 1, 1, 2), nrow = 2),
    sparse = TRUE
  )
  filtered <- filter_nonzero_variance_features(mat, 1:2)
  expect_s4_class(filtered, "dgCMatrix")
  expect_identical(dim(filtered), c(1L, 3L))
  expect_identical(filter_nonzero_variance_features(mat, integer()), mat[integer(), , drop = FALSE])
})

test_that("fast_row_vars is stable for data with a large offset", {
  dense <- matrix(
    1e12 + c(1, 2, 3, 4, 10, 12, 14, 16),
    nrow = 2,
    byrow = TRUE,
    dimnames = list(c("a", "b"), NULL)
  )
  expected <- apply(dense, 1L, stats::var)

  expect_equal(fast_row_vars(dense), expected, tolerance = 1e-12)
  expect_equal(
    fast_row_vars(Matrix::Matrix(dense, sparse = TRUE)),
    expected,
    tolerance = 1e-12
  )
})

test_that("fast_row_vars propagates missing and non-finite rows", {
  dense <- rbind(
    missing = c(1, NA, 3),
    infinite = c(1, Inf, 3),
    finite = c(1, 2, 3)
  )

  dense_result <- fast_row_vars(dense)
  sparse_result <- fast_row_vars(Matrix::Matrix(dense, sparse = TRUE))

  expect_true(is.na(dense_result[["missing"]]))
  expect_true(is.nan(dense_result[["infinite"]]))
  expect_equal(sparse_result, dense_result)
})
