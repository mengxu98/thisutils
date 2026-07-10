test_that("run_dense_topk_by_column orders ties and missing values", {
  mat <- matrix(c(2, NA, 2, 1, 4, 4), nrow = 3)
  result <- run_dense_topk_by_column(mat, k = 3, decreasing = TRUE)

  expect_identical(result$idx[1, ], c(1L, 3L, 2L))
  expect_equal(result$value[1, ], c(2, 2, NA_real_))
  expect_identical(result$idx[2, ], c(2L, 3L, 1L))
  expect_equal(result$value[2, ], c(4, 4, 1))
})

test_that("run_dense_topk_by_column pads results beyond available rows", {
  result <- run_dense_topk_by_column(matrix(c(2, 1), nrow = 2), k = 3)

  expect_identical(result$idx[1, ], c(2L, 1L, NA_integer_))
  expect_equal(result$value[1, ], c(1, 2, NA_real_))
})

test_that("run_sparse_topk_by_column orders stored sparse entries", {
  mat <- Matrix::sparseMatrix(
    i = c(1, 3, 2),
    j = c(1, 1, 2),
    x = c(1, 3, -2),
    dims = c(3, 2)
  )
  result <- run_sparse_topk_by_column(mat, k = 2, decreasing = TRUE)

  expect_identical(result$idx[1, ], c(3L, 1L))
  expect_equal(result$value[1, ], c(3, 1))
  expect_identical(result$idx[2, ], c(2L, NA_integer_))
  expect_equal(result$value[2, ], c(-2, 0))
})

test_that("top-k helpers reject non-positive k", {
  expect_error(run_dense_topk_by_column(matrix(1), k = 0), "positive")
  expect_error(
    run_sparse_topk_by_column(
      Matrix::sparseMatrix(i = 1, j = 1, x = 1, dims = c(1, 1)),
      k = 0
    ),
    "positive"
  )
})
