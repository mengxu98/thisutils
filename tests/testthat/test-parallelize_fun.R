test_that("parallelize_fun works with single core", {
  result <- suppressMessages(
    parallelize_fun(1:3, function(x) x^2, verbose = FALSE)
  )
  expect_equal(length(result), 3)
  expect_equal(result[[1]], 1)
  expect_equal(result[[2]], 4)
  expect_equal(result[[3]], 9)
})

test_that("parallelize_fun handles errors with clean_result = FALSE", {
  result <- suppressMessages(
    parallelize_fun(1:3, function(x) {
      if (x == 2) stop("fail")
      x
    }, verbose = FALSE, throw_error = FALSE)
  )
  expect_equal(length(result), 3)
  expect_true(inherits(result[[2]], "parallelize_error"))
})

test_that("parallelize_fun handles errors with clean_result = TRUE", {
  result <- suppressMessages(
    parallelize_fun(1:3, function(x) {
      if (x == 2) stop("fail")
      x
    }, clean_result = TRUE, verbose = FALSE, throw_error = FALSE)
  )
  expect_equal(length(result), 2)
})

test_that("parallelize_fun preserves names for named vectors", {
  x <- c(a = 1, b = 2, c = 3)
  result <- suppressMessages(
    parallelize_fun(x, function(v) v^2, verbose = FALSE)
  )
  expect_equal(names(result), c("a", "b", "c"))
})

test_that("parallelize_fun names output with values for unnamed vectors", {
  result <- suppressMessages(
    parallelize_fun(1:3, function(x) x^2, verbose = FALSE)
  )
  expect_equal(names(result), c("1", "2", "3"))
})

test_that("parallelize_fun does not set names for list inputs", {
  result <- suppressMessages(
    parallelize_fun(list(1, 2, 3), function(x) x^2, verbose = FALSE)
  )
  expect_null(names(result))
})

test_that("parallelize_fun restores cli options on error", {
  old_show <- getOption("cli.progress_show_after")
  old_clear <- getOption("cli.progress_clear")
  tryCatch(
    parallelize_fun(1:3, function(x) {
      stop("intentional error")
    }, verbose = TRUE, throw_error = FALSE),
    error = function(e) NULL,
    message = function(m) NULL
  )
  expect_equal(getOption("cli.progress_show_after"), old_show)
  expect_equal(getOption("cli.progress_clear"), old_clear)
})

test_that("parallelize_fun with verbose progress bar", {
  expect_message(
    parallelize_fun(1:3, function(x) x^2, verbose = TRUE)
  )
})
