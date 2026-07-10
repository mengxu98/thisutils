test_that("classification_metrics_compute handles labels and missing values", {
  result <- classification_metrics_compute(
    predicted = c("A", "A", "B", "C", NA),
    truth = c("A", "B", "B", "B", "C")
  )

  expect_equal(result$accuracy, 0.5)
  expect_equal(result$macro_f1, 7 / 12)
  expect_equal(result$purity, 0.75)
  expect_identical(result$class_table$class, c("A", "B", "C"))
  expect_identical(result$class_table$support, c(1L, 3L, 0L))
})

test_that("classification_metrics_compute returns stable empty results", {
  result <- classification_metrics_compute(
    predicted = c(NA_character_, NA_character_),
    truth = c("A", NA_character_)
  )

  expect_true(all(is.na(unlist(result[names(result) != "class_table"]))))
  expect_null(result$class_table)
})
