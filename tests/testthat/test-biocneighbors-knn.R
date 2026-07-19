test_that("run_biocneighbors_knn matches BiocNeighbors for reference and query searches", {
  skip_if_not_installed("BiocNeighbors")
  reference <- rbind(c(0, 0), c(1, 0), c(0, 2), c(3, 3))
  query <- rbind(c(0.1, 0), c(2.5, 3))
  param <- BiocNeighbors::KmknnParam(distance = "Euclidean")

  expected_ref <- BiocNeighbors::findKNN(reference, k = 2, BNPARAM = param)
  actual_ref <- run_biocneighbors_knn(reference, k = 2)
  expect_identical(actual_ref$idx, expected_ref$index)
  expect_identical(actual_ref$dist, expected_ref$distance)

  expected_query <- BiocNeighbors::queryKNN(reference, query = query, k = 2, BNPARAM = param)
  actual_query <- run_biocneighbors_knn(reference, query = query, k = 2)
  expect_identical(actual_query$idx, expected_query$index)
  expect_identical(actual_query$dist, expected_query$distance)
})

test_that("run_biocneighbors_knn removes self-neighbors without changing matrix shape", {
  skip_if_not_installed("BiocNeighbors")
  reference <- rbind(c(0, 0), c(1, 0), c(0, 2), c(3, 3))
  actual <- run_biocneighbors_knn(reference, k = 2, exclude_self = TRUE)

  expect_identical(dim(actual$idx), c(4L, 2L))
  expect_true(all(actual$idx[, 1] != seq_len(nrow(reference))))
})
