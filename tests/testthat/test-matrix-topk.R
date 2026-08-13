test_that("run_dense_topk orders ties and missing values", {
  mat <- matrix(c(2, NA, 2, 1, 4, 4), nrow = 3)
  result <- run_dense_topk(mat, k = 3, by = "col")

  expect_identical(result$idx[1, ], c(1L, 3L, 2L))
  expect_equal(result$value[1, ], c(2, 2, NA_real_))
  expect_identical(result$idx[2, ], c(2L, 3L, 1L))
  expect_equal(result$value[2, ], c(4, 4, 1))
})

test_that("run_dense_topk pads results beyond the selected dimension", {
  result <- run_dense_topk(matrix(c(2, 1), nrow = 2), k = 3)

  expect_identical(result$idx[1, ], c(1L, 2L, NA_integer_))
  expect_equal(result$value[1, ], c(2, 1, NA_real_))
})

test_that("run_dense_topk supports row-wise selection", {
  mat <- matrix(c(3, 1, 2, 4, 6, 5), nrow = 2, byrow = TRUE)
  result <- run_dense_topk(mat, k = 2, by = "row")

  expect_identical(result$idx[1, ], c(1L, 3L))
  expect_equal(result$value[1, ], c(3, 2))
  expect_identical(result$idx[2, ], c(2L, 3L))
  expect_equal(result$value[2, ], c(6, 5))
})

test_that("run_sparse_topk includes implicit zeros by column", {
  mat <- Matrix::sparseMatrix(
    i = c(1, 3, 2),
    j = c(1, 1, 2),
    x = c(1, 3, -2),
    dims = c(3, 2)
  )
  result <- run_sparse_topk(
    mat,
    k = 2,
    by = "col",
    decreasing = TRUE
  )

  expect_identical(result$idx[1, ], c(3L, 1L))
  expect_equal(result$value[1, ], c(3, 1))
  expect_identical(result$idx[2, ], c(1L, 3L))
  expect_equal(result$value[2, ], c(0, 0))
})

test_that("run_sparse_topk_stored excludes implicit zeros and pads with NA", {
  mat <- Matrix::sparseMatrix(
    i = c(1, 3, 2),
    j = c(1, 1, 2),
    x = c(1, 3, -2),
    dims = c(3, 2)
  )
  result <- run_sparse_topk_stored(mat, k = 2, by = "col")

  expect_identical(result$idx[1, ], c(3L, 1L))
  expect_equal(result$value[1, ], c(3, 1))
  expect_identical(result$idx[2, ], c(2L, NA_integer_))
  expect_equal(result$value[2, ], c(-2, NA_real_))
})

test_that("sparse top-k supports row-wise selection", {
  mat <- Matrix::sparseMatrix(
    i = c(1, 3, 2),
    j = c(1, 1, 2),
    x = c(1, 3, -2),
    dims = c(3, 2)
  )

  numeric_result <- run_sparse_topk(mat, k = 2, by = "row")
  stored_result <- run_sparse_topk_stored(mat, k = 2, by = "row")

  expect_identical(dim(numeric_result$idx), c(3L, 2L))
  expect_identical(numeric_result$idx[2, ], c(1L, 2L))
  expect_equal(numeric_result$value[2, ], c(0, -2))
  expect_identical(stored_result$idx[2, ], c(2L, NA_integer_))
  expect_equal(stored_result$value[2, ], c(-2, NA_real_))
})

test_that("sparse and dense top-k have matching matrix semantics", {
  dense <- matrix(c(-3, 0, 2, 0, NA, 0, -1, 4), nrow = 4)
  sparse <- Matrix::Matrix(dense, sparse = TRUE)

  for (decreasing in c(TRUE, FALSE)) {
    sparse_result <- run_sparse_topk(
      sparse,
      k = 4,
      by = "col",
      decreasing = decreasing
    )
    dense_result <- run_dense_topk(
      dense,
      k = 4,
      by = "col",
      decreasing = decreasing
    )
    expect_identical(sparse_result$idx, dense_result$idx)
    expect_equal(sparse_result$value, dense_result$value)
  }
})

test_that("legacy sparse top-k preserves stored-entry behavior", {
  mat <- Matrix::sparseMatrix(
    i = c(1, 3, 2),
    j = c(1, 1, 2),
    x = c(1, 3, -2),
    dims = c(3, 2)
  )

  legacy <- run_sparse_topk_by_column(mat, k = 2)
  expect_identical(legacy$idx[1, ], c(3L, 1L))
  expect_equal(legacy$value[1, ], c(3, 1))
  expect_identical(legacy$idx[2, ], c(2L, NA_integer_))
  expect_equal(legacy$value[2, ], c(-2, 0))
  expect_identical(
    run_sparse_stored_topk_by_column(mat, k = 2),
    run_sparse_topk_stored(mat, k = 2, by = "col")
  )
})

test_that("top-k helpers reject non-positive k", {
  expect_error(run_dense_topk(matrix(1), k = 0), "positive")
  expect_error(
    run_sparse_topk(
      Matrix::sparseMatrix(i = 1, j = 1, x = 1, dims = c(1, 1)),
      k = 0
    ),
    "positive"
  )
})

test_that("top-k helpers validate the selection direction", {
  mat <- Matrix::sparseMatrix(i = 1, j = 1, x = 1, dims = c(1, 1))
  expect_error(run_dense_topk(matrix(1), k = 1, by = "column"), "arg")
  expect_error(run_dense_topk(matrix(1), k = 1, by = "layer"), "arg")
  expect_error(run_sparse_topk(mat, k = 1, by = "column"), "arg")
  expect_error(run_sparse_topk(mat, k = 1, by = "layer"), "arg")
  expect_error(run_sparse_topk_stored(mat, k = 1, by = "layer"), "arg")
})
