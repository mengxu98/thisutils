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

test_that("parallel progress bar uses supplied width", {
  bar <- parallel_progress_bar(6, 10, 12L)
  expect_equal(nchar(cli::ansi_strip(bar)), 12)
})

test_that("parallel progress bar falls back to default width", {
  bar <- parallel_progress_bar(6, 10)
  expect_equal(nchar(cli::ansi_strip(bar)), 10)
})

test_that("parallelize_fun accepts progress_bar_width argument", {
  expect_no_error(
    suppressMessages(
      parallelize_fun(1:2, function(x) x^2, verbose = FALSE, progress_bar_width = 9L)
    )
  )
})

test_that("parallelize_fun preserves input order for multi-core execution", {
  result <- suppressMessages(
    parallelize_fun(1:6, function(x) x, cores = 2, verbose = TRUE)
  )

  expect_equal(unname(unlist(result)), 1:6)
  expect_equal(names(result), as.character(1:6))
})

test_that("parallelize_fun preserves input order for multi-core execution without verbose", {
  result <- suppressMessages(
    parallelize_fun(1:6, function(x) x * 2, cores = 2, verbose = FALSE)
  )

  expect_equal(unname(unlist(result)), (1:6) * 2)
  expect_equal(names(result), as.character(1:6))
})

test_that("parallelize_fun handles uneven multi-core workloads in verbose mode", {
  delays <- c(0.15, 0.01, 0.12, 0.02, 0.08, 0.01)
  result <- suppressMessages(
    parallelize_fun(1:6, function(x) {
      Sys.sleep(delays[[x]])
      x
    }, cores = 2, verbose = TRUE)
  )

  expect_equal(unname(unlist(result)), 1:6)
  expect_equal(names(result), as.character(1:6))
})

test_that("parallelize_fun exports requested dependencies in multi-core mode", {
  offset <- 5
  add_offset <- function(x) x + offset

  result <- suppressMessages(
    parallelize_fun(
      1:4,
      add_offset,
      cores = 2,
      verbose = FALSE,
      export_fun = "offset"
    )
  )

  expect_equal(unname(unlist(result)), 6:9)
  expect_equal(names(result), as.character(1:4))
})

test_that("cores_detect falls back to at least one core", {
  expect_gte(cores_detect(cores = 2, num_session = 4), 1)
})

test_that("parallelize_fun preserves NULL results on a single core", {
  result <- suppressMessages(
    parallelize_fun(
      1:3,
      function(x) if (x == 2) NULL else x,
      verbose = FALSE
    )
  )

  expect_length(result, 3)
  expect_equal(result[[1]], 1)
  expect_null(result[[2]])
  expect_equal(result[[3]], 3)
})
