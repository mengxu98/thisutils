test_that("as_matrix converts sparse to dense", {
  m <- simulate_sparse_matrix(10, 10)
  result <- as_matrix(m)
  expect_true(is.matrix(result))
  expect_false(inherits(result, "sparseMatrix"))
})

test_that("as_matrix converts dense to sparse", {
  m <- matrix(1:20, nrow = 4)
  result <- as_matrix(m, return_sparse = TRUE)
  expect_true(inherits(result, "sparseMatrix") || inherits(result, "Matrix"))
})

test_that("as_matrix returns sparse as-is when return_sparse = TRUE", {
  m <- simulate_sparse_matrix(10, 10)
  result <- as_matrix(m, return_sparse = TRUE)
  expect_identical(m, result)
})

test_that("matrix_to_table works on dense matrix", {
  m <- matrix(c(1, 2, 3, 4), nrow = 2)
  rownames(m) <- c("r1", "r2")
  colnames(m) <- c("c1", "c2")
  result <- matrix_to_table(m)
  expect_true(is.data.frame(result))
  expect_equal(ncol(result), 3)
  expect_true(all(c("row", "col", "value") %in% colnames(result)))
})

test_that("matrix_to_table works on sparse matrix", {
  m <- simulate_sparse_matrix(10, 10)
  colnames(m) <- paste0("c", 1:10)
  rownames(m) <- paste0("r", 1:10)
  result <- matrix_to_table(m, keep_zero = FALSE)
  expect_true(is.data.frame(result))
  expect_true(all(result$value != 0))
})

test_that("matrix_to_table with threshold", {
  m <- matrix(c(0.1, 0.5, 1.0, 2.0), nrow = 2)
  rownames(m) <- c("r1", "r2")
  colnames(m) <- c("c1", "c2")
  result <- matrix_to_table(m, threshold = 0.5)
  expect_true(all(abs(result$value) >= 0.5))
})

test_that("matrix_to_table with row/col filters", {
  m <- matrix(1:9, nrow = 3)
  rownames(m) <- c("r1", "r2", "r3")
  colnames(m) <- c("c1", "c2", "c3")
  result <- matrix_to_table(m, row_names = c("r1"), col_names = c("c1"))
  expect_equal(nrow(result), 1)
})

test_that("table_to_matrix roundtrips with matrix_to_table", {
  m <- matrix(c(1, 2, 3, 4, 5, 6), nrow = 2)
  rownames(m) <- c("r1", "r2")
  colnames(m) <- c("c1", "c2", "c3")
  tbl <- matrix_to_table(m)
  m2 <- table_to_matrix(tbl)
  m_reordered <- m[rownames(m2), colnames(m2)]
  expect_equal(m_reordered, m2, tolerance = 1e-10)
})

test_that("table_to_matrix with threshold", {
  tbl <- data.frame(
    row = c("r1", "r2", "r3"),
    col = c("c1", "c1", "c1"),
    value = c(0.1, 0.5, 1.0)
  )
  result <- table_to_matrix(tbl, threshold = 0.5)
  expect_equal(result["r1", "c1"], 0)
  expect_equal(result["r2", "c1"], 0.5)
})

test_that("table_to_matrix with return_sparse", {
  tbl <- data.frame(
    row = c("r1", "r2"),
    col = c("c1", "c2"),
    value = c(1.0, 2.0)
  )
  result <- table_to_matrix(tbl, return_sparse = TRUE)
  expect_true(inherits(result, "sparseMatrix"))
})

test_that("split_indices works", {
  result <- split_indices(c(1, 2, 1, 2, 3))
  expect_true(is.list(result))
  expect_equal(length(result), 3)
  expect_equal(sort(result[[1]]), c(1, 3))
  expect_equal(sort(result[[2]]), c(2, 4))
  expect_equal(result[[3]], 5)
})

test_that("split_indices with n parameter", {
  result <- split_indices(c(1, 2, 1), 5)
  expect_equal(length(result), 5)
})
