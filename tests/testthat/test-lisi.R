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

compute_lisi_original_like_test <- function(X, meta_data, label_colnames, perplexity = 30, nn_eps = 0) {
  N <- nrow(meta_data)
  dknn <- RANN::nn2(X, k = perplexity * 3, eps = nn_eps)

  out <- lapply(label_colnames, function(label_colname) {
    labels <- data.frame(meta_data)[, label_colname, drop = TRUE]
    if (any(is.na(labels))) {
      return(rep(NA_real_, N))
    }

    nn_idx <- dknn$nn.idx[, 2:ncol(dknn$nn.idx), drop = FALSE]
    nn_dists <- dknn$nn.dists[, 2:ncol(dknn$nn.dists), drop = FALSE]
    labels <- as.integer(factor(labels)) - 1L
    n_batches <- length(unique(labels))

    simpson <- compute_simpson_index_original_like_test(
      t(nn_dists),
      t(nn_idx) - 1L,
      labels,
      n_batches,
      perplexity
    )

    1 / simpson
  })

  out <- as.data.frame(out, optional = TRUE, stringsAsFactors = FALSE)
  colnames(out) <- label_colnames
  out
}

compute_simpson_index_original_like_test <- function(D, knn_idx, batch_labels, n_batches, perplexity = 15, tol = 1e-5) {
  D <- as.matrix(D)
  knn_idx <- as.matrix(knn_idx)
  n <- ncol(D)
  simpson <- numeric(n)
  logU <- log(perplexity)

  hbeta <- function(D, beta, idx) {
    P <- exp(-D[, idx] * beta)
    sumP <- sum(P)
    if (sumP == 0) {
      list(H = 0, P = D[, idx] * 0)
    } else {
      H <- log(sumP) + beta * sum(D[, idx] * P) / sumP
      list(H = H, P = P / sumP)
    }
  }

  for (cell in seq_len(n)) {
    beta <- 1
    betamin <- -Inf
    betamax <- Inf
    hb <- hbeta(D, beta, cell)
    H <- hb$H
    P <- hb$P
    Hdiff <- H - logU
    tries <- 0

    while (abs(Hdiff) > tol && tries < 50) {
      if (Hdiff > 0) {
        betamin <- beta
        beta <- if (!is.finite(betamax)) beta * 2 else (beta + betamax) / 2
      } else {
        betamax <- beta
        beta <- if (!is.finite(betamin)) beta / 2 else (beta + betamin) / 2
      }

      hb <- hbeta(D, beta, cell)
      H <- hb$H
      P <- hb$P
      Hdiff <- H - logU
      tries <- tries + 1
    }

    if (H == 0) {
      simpson[cell] <- -1
      next
    }

    for (b in 0:(n_batches - 1L)) {
      q <- which(batch_labels[knn_idx[, cell] + 1L] == b)
      if (length(q) > 0) {
        sumP <- sum(P[q])
        simpson[cell] <- simpson[cell] + sumP * sumP
      }
    }
  }

  simpson
}

test_that("choose_lisi_nn_method prefers exact only for very small problems", {
  expect_equal(
    choose_lisi_nn_method(n = 200, p = 10, n_neighbors = 30, use_rann = TRUE),
    "exact"
  )
  expect_equal(
    choose_lisi_nn_method(n = 300, p = 10, n_neighbors = 30, use_rann = TRUE),
    "rann"
  )
  expect_equal(
    choose_lisi_nn_method(n = 1000, p = 30, n_neighbors = 90, use_rann = TRUE),
    "rann"
  )
  expect_equal(
    choose_lisi_nn_method(n = 1000, p = 30, n_neighbors = 90, use_rann = FALSE),
    "fnn"
  )
})

test_that("compute_lisi exact, rann and fnn agree on small data", {
  set.seed(11)
  X <- rbind(
    matrix(rnorm(1000, mean = -1), ncol = 10),
    matrix(rnorm(1000, mean = 1), ncol = 10)
  )
  meta <- data.frame(
    batch = rep(c("A", "B"), each = nrow(X) / 2),
    group = sample(c("g1", "g2", "g3"), nrow(X), replace = TRUE)
  )

  exact_res <- suppressMessages(compute_lisi(X, meta, c("batch", "group"), perplexity = 10, nn_method = "exact"))
  rann_res <- suppressMessages(compute_lisi(X, meta, c("batch", "group"), perplexity = 10, nn_method = "rann"))
  fnn_res <- suppressMessages(compute_lisi(X, meta, c("batch", "group"), perplexity = 10, nn_method = "fnn"))

  expect_equal(rann_res, exact_res, tolerance = 1e-10)
  expect_equal(fnn_res, exact_res, tolerance = 1e-10)
})

test_that("compute_lisi rann matches original LISI implementation path", {
  set.seed(44)
  X <- rbind(
    matrix(rnorm(2000, mean = -1), ncol = 10),
    matrix(rnorm(2000, mean = 1), ncol = 10)
  )
  meta <- data.frame(
    batch = rep(c("A", "B"), each = nrow(X) / 2),
    group = sample(c("g1", "g2", "g3"), nrow(X), replace = TRUE)
  )

  ours <- suppressMessages(compute_lisi(X, meta, c("batch", "group"), perplexity = 10, nn_method = "rann"))
  orig <- compute_lisi_original_like_test(X, meta, c("batch", "group"), perplexity = 10, nn_eps = 0)

  expect_equal(colnames(ours), colnames(orig))
  expect_equal(unname(as.matrix(ours)), unname(as.matrix(orig)), tolerance = 1e-12)
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
  rann_medium <- suppressMessages(compute_lisi(X_medium, meta_medium, "batch", perplexity = 15, nn_method = "rann"))
  expect_equal(auto_medium, rann_medium, tolerance = 1e-10)
})
