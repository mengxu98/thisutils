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

test_that("matrix_to_table keep_zero TRUE on sparse matches dense output", {
  m_sparse <- simulate_sparse_matrix(5, 4, sparsity = 0.7, seed = 2)
  m_dense <- as.matrix(m_sparse)

  sparse_result <- matrix_to_table(m_sparse, keep_zero = TRUE)
  dense_result <- matrix_to_table(m_dense, keep_zero = TRUE)

  expect_equal(sparse_result, dense_result)
  expect_equal(nrow(sparse_result), nrow(m_sparse) * ncol(m_sparse))
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

test_that("matrix_to_table sparse keep_zero TRUE respects filters consistently", {
  m_sparse <- simulate_sparse_matrix(6, 5, sparsity = 0.8, seed = 3)
  m_dense <- as.matrix(m_sparse)

  sparse_result <- matrix_to_table(
    m_sparse,
    row_names = c("row_2", "row_4"),
    col_names = c("col_1", "col_3"),
    keep_zero = TRUE
  )
  dense_result <- matrix_to_table(
    m_dense,
    row_names = c("row_2", "row_4"),
    col_names = c("col_1", "col_3"),
    keep_zero = TRUE
  )

  expect_equal(sparse_result, dense_result)
  expect_equal(nrow(sparse_result), 4)
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

test_that("table_to_matrix handles duplicate coordinates consistently", {
  tbl <- data.frame(
    row = c("r1", "r1"),
    col = c("c1", "c1"),
    value = c(1.0, 2.0)
  )

  dense_result <- table_to_matrix(tbl)
  sparse_result <- table_to_matrix(tbl, return_sparse = TRUE)

  expect_equal(dense_result["r1", "c1"], 3)
  expect_equal(as.matrix(sparse_result)["r1", "c1"], 3)
  expect_equal(dense_result, as.matrix(sparse_result))
})

test_that("table_to_matrix sparse path handles names containing separators", {
  tbl <- data.frame(
    row = c("r|1", "r2", "r|1"),
    col = c("c|1", "c2", "c2"),
    value = c(5, 7, 11)
  )

  dense_result <- table_to_matrix(tbl)
  sparse_result <- table_to_matrix(tbl, return_sparse = TRUE)

  expect_equal(as.matrix(sparse_result), dense_result)
  expect_equal(dense_result["r|1", "c|1"], 5)
  expect_equal(dense_result["r2", "c2"], 7)
  expect_equal(dense_result["r|1", "c2"], 11)
})

test_that("table_to_matrix applies threshold before duplicate aggregation consistently", {
  tbl <- data.frame(
    row = c("r1", "r1", "r1", "r2"),
    col = c("c1", "c1", "c1", "c2"),
    value = c(0.2, 0.4, -0.1, 1.0)
  )

  dense_result <- table_to_matrix(tbl, threshold = 0.3)
  sparse_result <- table_to_matrix(tbl, threshold = 0.3, return_sparse = TRUE)

  expect_equal(as.matrix(sparse_result), dense_result)
  expect_equal(dense_result["r1", "c1"], 0.4)
  expect_equal(dense_result["r2", "c2"], 1.0)
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
