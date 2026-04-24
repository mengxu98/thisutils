test_that("simulate_sparse_matrix creates correct dimensions", {
  m <- simulate_sparse_matrix(100, 50)
  expect_equal(nrow(m), 100)
  expect_equal(ncol(m), 50)
  expect_true(inherits(m, "sparseMatrix"))
})

test_that("simulate_sparse_matrix respects sparsity", {
  m <- simulate_sparse_matrix(100, 100, sparsity = 0.9)
  sp <- check_sparsity(m)
  expect_true(sp > 0.85 && sp < 0.95)
})

test_that("simulate_sparse_matrix with decimal", {
  m <- simulate_sparse_matrix(10, 10, decimal = 2)
  nonzero <- m@x
  expect_true(all(nonzero == round(nonzero, 2)))
})

test_that("check_sparsity works for dense matrix", {
  m <- matrix(0, 10, 10)
  m[1, 1] <- 1
  expect_equal(check_sparsity(m), 0.99)
})

test_that("check_sparsity works for sparse matrix", {
  m <- simulate_sparse_matrix(100, 100, sparsity = 0.95)
  sp <- check_sparsity(m)
  expect_true(sp > 0.9)
})

test_that("r_square computes correctly", {
  y <- 1:10
  y_pred <- y
  expect_equal(r_square(y, y_pred), 1)
})

test_that("r_square handles imperfect predictions", {
  set.seed(42)
  y <- rnorm(100)
  y_pred <- y + rnorm(100, sd = 0.5)
  r2 <- r_square(y, y_pred)
  expect_true(r2 > 0 && r2 < 1)
})

test_that("normalization max_min works", {
  x <- c(1, 2, 3, 4, 5)
  result <- normalization(x, method = "max_min")
  expect_equal(min(result), 0)
  expect_equal(max(result), 1)
})

test_that("normalization z_score works", {
  x <- c(1, 2, 3, 4, 5)
  result <- normalization(x, method = "z_score")
  expect_equal(mean(result), 0, tolerance = 1e-10)
  expect_equal(sd(result), 1, tolerance = 1e-10)
})

test_that("normalization handles NA with na_rm = TRUE", {
  x <- c(1, NA, 3, 4, 5)
  result <- normalization(x, method = "max_min")
  expect_equal(result[2], 0)
  expect_false(is.na(result[2]))
})

test_that("normalization handles NA with na_rm = FALSE", {
  x <- c(1, NA, 3, 4, 5)
  result <- normalization(x, method = "max_min", na_rm = FALSE)
  expect_true(is.na(result[2]))
})

test_that("normalization NA handling does not pollute calculations", {
  x <- c(10, NA, 30, 40, 50)
  result <- normalization(x, method = "max_min", na_rm = FALSE)
  expect_equal(result[1], 0, tolerance = 1e-10)
  expect_equal(result[5], 1, tolerance = 1e-10)
  expect_true(is.na(result[2]))
})

test_that("normalization softmax works", {
  x <- c(1, 2, 3)
  result <- normalization(x, method = "softmax")
  expect_equal(sum(result), 1, tolerance = 1e-10)
  expect_true(all(result > 0))
})

test_that("normalization mad works", {
  x <- c(1, 2, 3, 4, 5)
  result <- normalization(x, method = "mad")
  expect_equal(
    result,
    (x - median(x)) / mad(x),
    tolerance = 1e-10
  )
})

test_that("normalization unit_vector works", {
  x <- c(3, 4)
  result <- normalization(x, method = "unit_vector")
  expect_equal(sqrt(sum(result^2)), 1, tolerance = 1e-10)
})

test_that("normalization robust_scale works", {
  x <- c(1, 2, 3, 4, 5)
  result <- normalization(x, method = "robust_scale")
  q1 <- quantile(x, 0.25)
  q3 <- quantile(x, 0.75)
  iqr <- q3 - q1
  med <- median(x)
  expected <- as.numeric((x - med) / iqr)
  expect_equal(result, expected, tolerance = 1e-10)
})

test_that("normalization errors on invalid method", {
  expect_error(normalization(1:5, method = "invalid"))
})

test_that("normalization handles degenerate inputs without NaN", {
  expect_equal(normalization(c(1, 1, 1), method = "max_min"), c(0, 0, 0))
  expect_equal(normalization(c(0, 0, 0), method = "sum"), c(0, 0, 0))
  expect_equal(normalization(c(1, 1, 1), method = "z_score"), c(0, 0, 0))
  expect_equal(
    normalization(c(1, 1, NA), method = "softmax", na_rm = FALSE),
    c(0.5, 0.5, NA)
  )
})

test_that("matrix_process works with string methods", {
  m <- simulate_sparse_matrix(10, 10)
  expect_no_error(matrix_process(m, method = "raw"))
  expect_no_error(matrix_process(m, method = "log1p"))
})

test_that("matrix_process works with custom function", {
  m <- matrix(1:20, nrow = 4)
  result <- matrix_process(m, method = function(x) x * 2)
  expect_equal(result, m * 2)
})

test_that("matrix_process errors on invalid string method", {
  m <- matrix(1:20, nrow = 4)
  expect_error(matrix_process(m, method = "invalid"))
})

test_that("matrix_process errors when dimensions change", {
  m <- matrix(1:20, nrow = 4)
  expect_error(
    matrix_process(m, method = function(x) x[1:2, ]),
    "dimensions"
  )
})
