test_that("pearson_correlation works for single matrix", {
  m <- simulate_sparse_matrix(50, 10, sparsity = 0.5)
  result <- pearson_correlation(m)
  expect_true(is.list(result))
  expect_true("cov" %in% names(result))
  expect_true("cor" %in% names(result))
  expect_equal(nrow(result$cor), 10)
  expect_equal(ncol(result$cor), 10)
})

test_that("pearson_correlation works for two matrices", {
  m1 <- simulate_sparse_matrix(50, 10, sparsity = 0.5)
  m2 <- simulate_sparse_matrix(50, 8, sparsity = 0.5, seed = 2)
  result <- pearson_correlation(m1, m2)
  expect_equal(nrow(result$cor), 10)
  expect_equal(ncol(result$cor), 8)
})

test_that("pearson_correlation errors on non-sparse input", {
  expect_error(pearson_correlation(matrix(1:20, 4, 5)))
})

test_that("sparse_cor works with pearson method", {
  m <- simulate_sparse_matrix(50, 10, sparsity = 0.5)
  result <- sparse_cor(m, method = "pearson")
  expect_true(inherits(result, "sparseMatrix"))
  expect_equal(nrow(result), 10)
})

test_that("sparse_cor works with two matrices", {
  m1 <- simulate_sparse_matrix(50, 10, sparsity = 0.5)
  m2 <- simulate_sparse_matrix(50, 8, sparsity = 0.5, seed = 2)
  result <- sparse_cor(m1, m2, method = "pearson")
  expect_equal(nrow(result), 10)
  expect_equal(ncol(result), 8)
})

test_that("sparse_cor errors on row mismatch", {
  m1 <- simulate_sparse_matrix(50, 10, sparsity = 0.5)
  m2 <- simulate_sparse_matrix(30, 10, sparsity = 0.5, seed = 2)
  expect_error(sparse_cor(m1, m2))
})

test_that("sparse_cor removes NA values by default", {
  m <- simulate_sparse_matrix(50, 10, sparsity = 0.5)
  result <- sparse_cor(m, remove_na = TRUE)
  expect_false(any(is.na(as.matrix(result))))
})

test_that("sparse_cor allow_neg = FALSE removes negatives", {
  m <- simulate_sparse_matrix(50, 10, sparsity = 0.5)
  result <- sparse_cor(m, allow_neg = FALSE)
  expect_true(all(result >= 0))
})
