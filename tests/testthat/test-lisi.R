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

test_that("choose_lisi_nn_method defaults to exact C++", {
  expect_equal(
    choose_lisi_nn_method(n = 200, p = 10, n_neighbors = 30),
    "exact"
  )
  expect_equal(
    choose_lisi_nn_method(n = 300, p = 10, n_neighbors = 30),
    "exact"
  )
  expect_equal(
    choose_lisi_nn_method(n = 1000, p = 30, n_neighbors = 90),
    "exact"
  )
  expect_equal(
    choose_lisi_nn_method(n = 1000, p = 30, n_neighbors = 90),
    "exact"
  )
})

test_that("compute_lisi computes multiple labels in one pass", {
  set.seed(12)
  X <- matrix(rnorm(3000), ncol = 10)
  meta <- data.frame(
    batch = sample(c("A", "B"), nrow(X), replace = TRUE),
    group = sample(c("g1", "g2", "g3"), nrow(X), replace = TRUE),
    condition = sample(c("ctrl", "stim"), nrow(X), replace = TRUE)
  )

  multi_res <- suppressMessages(
    compute_lisi(X, meta, c("batch", "group", "condition"), perplexity = 10, nn_method = "exact")
  )
  single_res <- do.call(cbind, lapply(c("batch", "group", "condition"), function(label) {
    suppressMessages(compute_lisi(X, meta, label, perplexity = 10, nn_method = "exact"))[[1]]
  }))
  colnames(single_res) <- c("batch", "group", "condition")

  expect_equal(unname(as.matrix(multi_res)), unname(single_res), tolerance = 1e-12)
})

test_that("compute_lisi fused C++ path matches explicit KNN and Simpson path", {
  set.seed(13)
  X <- matrix(rnorm(3000), ncol = 10)
  meta <- data.frame(
    batch = sample(c("A", "B"), nrow(X), replace = TRUE),
    group = sample(c("g1", "g2", "g3"), nrow(X), replace = TRUE)
  )
  perplexity <- 10
  n_neighbors <- min(
    nrow(X) - 1L,
    max(1L, as.integer(ceiling(perplexity * 3)) - 1L)
  )

  fused <- suppressMessages(
    compute_lisi(X, meta, c("batch", "group"), perplexity = perplexity)
  )
  knn <- suppressMessages(
    lisi_knn(X, n_neighbors = n_neighbors, nn_method = "exact")
  )
  labels <- cbind(
    batch = as.integer(factor(meta$batch)),
    group = as.integer(factor(meta$group))
  )
  explicit <- 1 / compute_simpson_index_matrix(
    D = t(knn$nn.dists),
    knn_idx = t(knn$nn.idx),
    batch_labels = labels,
    perplexity = perplexity
  )

  expect_equal(unname(as.matrix(fused)), unname(explicit), tolerance = 1e-12)
})

test_that("compute_lisi auto and exact agree on small data", {
  set.seed(11)
  X <- rbind(
    matrix(rnorm(1000, mean = -1), ncol = 10),
    matrix(rnorm(1000, mean = 1), ncol = 10)
  )
  meta <- data.frame(
    batch = rep(c("A", "B"), each = nrow(X) / 2),
    group = sample(c("g1", "g2", "g3"), nrow(X), replace = TRUE)
  )

  auto_res <- suppressMessages(compute_lisi(X, meta, c("batch", "group"), perplexity = 10, nn_method = "auto"))
  exact_res <- suppressMessages(compute_lisi(X, meta, c("batch", "group"), perplexity = 10, nn_method = "exact"))

  expect_equal(auto_res, exact_res, tolerance = 1e-10)
})

test_that("removed RANN and FNN backends are rejected", {
  X <- matrix(rnorm(100), ncol = 5)
  meta <- data.frame(batch = rep(c("A", "B"), each = 10))

  expect_error(
    suppressMessages(compute_lisi(X, meta, "batch", perplexity = 5, nn_method = "rann")),
    "should be one of"
  )
  expect_error(
    suppressMessages(compute_lisi(X, meta, "batch", perplexity = 5, nn_method = "fnn")),
    "should be one of"
  )
})

test_that("compute_lisi auto matches explicit backend strategy on different sizes", {
  set.seed(22)
  X_small <- rbind(
    matrix(rnorm(1000, mean = -1), ncol = 10),
    matrix(rnorm(1000, mean = 1), ncol = 10)
  )
  meta_small <- data.frame(
    batch = rep(c("A", "B"), each = nrow(X_small) / 2)
  )

  auto_small <- suppressMessages(compute_lisi(X_small, meta_small, "batch", perplexity = 10, nn_method = "auto"))
  exact_small <- suppressMessages(compute_lisi(X_small, meta_small, "batch", perplexity = 10, nn_method = "exact"))
  expect_equal(auto_small, exact_small, tolerance = 1e-10)

  X_medium <- rbind(
    matrix(rnorm(6000, mean = -1), ncol = 20),
    matrix(rnorm(6000, mean = 1), ncol = 20)
  )
  meta_medium <- data.frame(
    batch = rep(c("A", "B"), each = nrow(X_medium) / 2)
  )

  auto_medium <- suppressMessages(compute_lisi(X_medium, meta_medium, "batch", perplexity = 15, nn_method = "auto"))
  exact_medium <- suppressMessages(compute_lisi(X_medium, meta_medium, "batch", perplexity = 15, nn_method = "exact"))
  expect_equal(auto_medium, exact_medium, tolerance = 1e-10)
})
