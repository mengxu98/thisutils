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

test_that("compute_lisi no longer accepts nearest-neighbor backend selection", {
  X <- matrix(rnorm(100), ncol = 5)
  meta <- data.frame(batch = rep(c("A", "B"), each = 10))

  expect_error(
    compute_lisi(X, meta, "batch", perplexity = 5, nn_method = "exact"),
    "unused argument"
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
    compute_lisi(X, meta, c("batch", "group", "condition"), perplexity = 10)
  )
  single_res <- do.call(cbind, lapply(c("batch", "group", "condition"), function(label) {
    suppressMessages(compute_lisi(X, meta, label, perplexity = 10))[[1]]
  }))
  colnames(single_res) <- c("batch", "group", "condition")

  expect_equal(unname(as.matrix(multi_res)), unname(single_res), tolerance = 1e-12)
})

test_that("compute_lisi wrapper matches fused C++ path", {
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
  labels <- cbind(
    batch = as.integer(factor(meta$batch)),
    group = as.integer(factor(meta$group))
  )
  direct <- compute_lisi_matrix(
    X = X,
    batch_labels = labels,
    n_neighbors = n_neighbors,
    perplexity = perplexity
  )

  expect_equal(unname(as.matrix(fused)), unname(direct), tolerance = 1e-12)
})

test_that("compute_lisi is deterministic on small data", {
  set.seed(11)
  X <- rbind(
    matrix(rnorm(1000, mean = -1), ncol = 10),
    matrix(rnorm(1000, mean = 1), ncol = 10)
  )
  meta <- data.frame(
    batch = rep(c("A", "B"), each = nrow(X) / 2),
    group = sample(c("g1", "g2", "g3"), nrow(X), replace = TRUE)
  )

  first <- suppressMessages(compute_lisi(X, meta, c("batch", "group"), perplexity = 10))
  second <- suppressMessages(compute_lisi(X, meta, c("batch", "group"), perplexity = 10))

  expect_equal(first, second, tolerance = 1e-10)
})

test_that("removed backend argument is rejected", {
  X <- matrix(rnorm(100), ncol = 5)
  meta <- data.frame(batch = rep(c("A", "B"), each = 10))

  expect_error(
    suppressMessages(compute_lisi(X, meta, "batch", perplexity = 5, nn_method = "rann")),
    "unused argument"
  )
  expect_error(
    suppressMessages(compute_lisi(X, meta, "batch", perplexity = 5, nn_method = "fnn")),
    "unused argument"
  )
})

test_that("compute_lisi remains stable on different sizes", {
  set.seed(22)
  X_small <- rbind(
    matrix(rnorm(1000, mean = -1), ncol = 10),
    matrix(rnorm(1000, mean = 1), ncol = 10)
  )
  meta_small <- data.frame(
    batch = rep(c("A", "B"), each = nrow(X_small) / 2)
  )

  first_small <- suppressMessages(compute_lisi(X_small, meta_small, "batch", perplexity = 10))
  second_small <- suppressMessages(compute_lisi(X_small, meta_small, "batch", perplexity = 10))
  expect_equal(first_small, second_small, tolerance = 1e-10)

  X_medium <- rbind(
    matrix(rnorm(6000, mean = -1), ncol = 20),
    matrix(rnorm(6000, mean = 1), ncol = 20)
  )
  meta_medium <- data.frame(
    batch = rep(c("A", "B"), each = nrow(X_medium) / 2)
  )

  first_medium <- suppressMessages(compute_lisi(X_medium, meta_medium, "batch", perplexity = 15))
  second_medium <- suppressMessages(compute_lisi(X_medium, meta_medium, "batch", perplexity = 15))
  expect_equal(first_medium, second_medium, tolerance = 1e-10)
})
