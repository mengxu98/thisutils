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
  result <- sparse_cor(m, method = "pearson", block_size = 3)
  expect_true(inherits(result, "sparseMatrix"))
  expect_true(inherits(result, "symmetricMatrix"))
  expect_equal(nrow(result), 10)
  expect_equal(
    as.matrix(result),
    stats::cor(as.matrix(m)),
    tolerance = 1e-12
  )
})

test_that("sparse_cor works with two matrices", {
  m1 <- simulate_sparse_matrix(50, 10, sparsity = 0.5)
  m2 <- simulate_sparse_matrix(50, 8, sparsity = 0.5, seed = 2)
  expected <- stats::cor(as.matrix(m1), as.matrix(m2))
  for (block_size in c(1L, 3L, ncol(m2))) {
    result <- sparse_cor(
      m1,
      m2,
      method = "pearson",
      block_size = block_size
    )
    expect_equal(nrow(result), 10)
    expect_equal(ncol(result), 8)
    expect_equal(as.matrix(result), expected, tolerance = 1e-12)
  }
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

test_that("sparse_cor threshold controls stored output", {
  m <- simulate_sparse_matrix(80, 12, sparsity = 0.6)
  result <- sparse_cor(m, threshold = 0.25, block_size = 2)
  expected <- stats::cor(as.matrix(m))
  expected[is.na(expected)] <- 0
  expected[abs(expected) < 0.25] <- 0

  expect_equal(as.matrix(result), expected, tolerance = 1e-12)
  expect_true(all(abs(result@x) >= 0.25))
})

test_that("rank correlations are guarded before dense conversion", {
  m <- simulate_sparse_matrix(30, 6, sparsity = 0.5)

  observed <- sparse_cor(m, method = "spearman")
  expect_true(inherits(observed, "symmetricMatrix"))
  expect_equal(
    as.matrix(observed),
    stats::cor(as.matrix(m), method = "spearman"),
    tolerance = 1e-12
  )
  expect_error(
    sparse_cor(m, method = "spearman", max_dense_bytes = 1),
    "Estimated dense working memory"
  )
})

test_that("sparse_cor enforces working and output limits", {
  m <- simulate_sparse_matrix(30, 6, sparsity = 0.5)

  expect_error(
    sparse_cor(m, max_dense_bytes = 1),
    "too small for one Pearson"
  )
  expect_error(
    sparse_cor(m, max_output_entries = 1),
    "exceeded"
  )
  expect_error(sparse_cor(m, method = "invalid"), "arg")
})

test_that("correlation cleaning maps infinities to zero", {
  input <- matrix(c(Inf, -Inf, NA, 0.5), nrow = 2)
  result <- .clean_cor_block(
    input,
    allow_neg = TRUE,
    remove_na = FALSE,
    remove_inf = TRUE,
    threshold = 0
  )

  expect_equal(result[1:2], c(0, 0))
  expect_true(is.na(result[[3]]))
})

test_that("pearson_correlation exposes its dense memory boundary", {
  m <- simulate_sparse_matrix(30, 6, sparsity = 0.5)

  expect_error(
    pearson_correlation(m, max_dense_bytes = 1),
    "Estimated dense output"
  )
})
