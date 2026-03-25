test_that("compute_lisi produces correct output shape", {
  set.seed(1)
  X <- rbind(
    matrix(rnorm(100, mean = -2), ncol = 5),
    matrix(rnorm(100, mean = 2), ncol = 5)
  )
  meta_data <- data.frame(
    batch = rep(c("A", "B"), each = 20)
  )
  result <- suppressMessages(
    compute_lisi(X, meta_data, "batch", perplexity = 10)
  )
  expect_true(is.data.frame(result))
  expect_equal(nrow(result), 40)
  expect_equal(ncol(result), 1)
  expect_equal(colnames(result), "batch")
})

test_that("compute_lisi works with multiple labels", {
  set.seed(1)
  X <- rbind(
    matrix(rnorm(100, mean = -2), ncol = 5),
    matrix(rnorm(100, mean = 2), ncol = 5)
  )
  meta_data <- data.frame(
    batch = rep(c("A", "B"), each = 20),
    group = sample(c("g1", "g2"), 40, replace = TRUE)
  )
  result <- suppressMessages(
    compute_lisi(X, meta_data, c("batch", "group"), perplexity = 10)
  )
  expect_equal(ncol(result), 2)
  expect_equal(colnames(result), c("batch", "group"))
})

test_that("compute_lisi errors on mismatched rows", {
  X <- matrix(rnorm(20), ncol = 2)
  meta <- data.frame(batch = c("A", "B", "A"))
  expect_error(
    suppressMessages(compute_lisi(X, meta, "batch")),
    "same number of rows"
  )
})

test_that("compute_lisi errors on missing label column", {
  X <- matrix(rnorm(20), ncol = 2)
  meta <- data.frame(batch = rep("A", 10))
  expect_error(
    suppressMessages(compute_lisi(X, meta, "nonexistent")),
    "missing"
  )
})

test_that("compute_lisi errors on invalid perplexity", {
  X <- matrix(rnorm(20), ncol = 2)
  meta <- data.frame(batch = rep("A", 10))
  expect_error(
    suppressMessages(compute_lisi(X, meta, "batch", perplexity = -1)),
    "positive"
  )
})

test_that("compute_simpson_index works", {
  D <- matrix(
    c(0.1, 0.2, 0.2, 0.1,
      0.3, 0.4, 0.4, 0.3),
    nrow = 2, byrow = TRUE
  )
  knn_idx <- matrix(
    c(2, 1, 4, 3,
      3, 4, 2, 1),
    nrow = 2, byrow = TRUE
  )
  batch_labels <- c(1, 1, 2, 2)
  result <- compute_simpson_index(D, knn_idx, batch_labels, perplexity = 2)
  expect_true(is.numeric(result))
  expect_equal(length(result), 4)
  expect_true(all(result > 0 & result <= 1, na.rm = TRUE))
})

test_that("compute_simpson_index errors on dimension mismatch", {
  D <- matrix(1:6, nrow = 2)
  knn_idx <- matrix(1:4, nrow = 2)
  expect_error(
    compute_simpson_index(D, knn_idx, c(1, 1, 1)),
    "same dimensions"
  )
})
