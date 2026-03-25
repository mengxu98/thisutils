test_that("%ss% returns left side when not NULL", {
  expect_equal(5 %ss% 10, 5)
  expect_equal("a" %ss% "b", "a")
  expect_equal(0 %ss% 10, 0)
  expect_equal(FALSE %ss% TRUE, FALSE)
})

test_that("%ss% returns right side when left is NULL", {
  expect_equal(NULL %ss% 10, 10)
  expect_equal(NULL %ss% "b", "b")
  expect_equal(NULL %ss% NULL, NULL)
})

test_that("get_namespace_fun returns function from namespace", {
  fn <- get_namespace_fun("base", "mean")
  expect_true(is.function(fn))
  expect_equal(fn(1:10), mean(1:10))
})

test_that("get_namespace_fun returns NULL for missing function", {
  expect_message(
    result <- get_namespace_fun("base", "nonexistent_function_xyz"),
    "not found"
  )
  expect_null(result)
})

test_that("invoke_fun works with function objects", {
  f <- function(x, y) x + y
  expect_equal(invoke_fun(f, list(x = 1, y = 2)), 3)
})

test_that("invoke_fun works with function name strings", {
  f <- function(x, y) x * y
  expect_equal(invoke_fun("f", list(x = 3, y = 4)), 12)
})

test_that("invoke_fun works with ... args", {
  f <- function(x, y) x - y
  expect_equal(invoke_fun("f", x = 10, y = 3), 7)
})

test_that("invoke_fun works with no args", {
  f <- function() 42
  expect_equal(invoke_fun(f), 42)
})

test_that("capitalize works on basic strings", {
  expect_equal(capitalize("hello world"), "Hello world")
  expect_equal(capitalize("Hello World"), "Hello World")
})

test_that("capitalize handles NULL and factor", {
  expect_null(capitalize(NULL))
  expect_equal(capitalize(factor("hello")), "Hello")
})

test_that("capitalize with force_tolower", {
  expect_equal(capitalize("hELLO", force_tolower = TRUE), "Hello")
})

test_that("capitalize errors on non-character", {
  expect_error(capitalize(123))
})

test_that("unnest_fun works on basic case", {
  data <- data.frame(id = 1:2, x = c("a", "b"), stringsAsFactors = FALSE)
  data$vals <- list(c(1, 2), c(3, 4, 5))
  result <- unnest_fun(data, cols = "vals")
  expect_equal(nrow(result), 5)
  expect_equal(result$vals, c(1, 2, 3, 4, 5))
})

test_that("unnest_fun handles empty data", {
  data <- data.frame(id = integer(0), x = character(0), stringsAsFactors = FALSE)
  result <- unnest_fun(data, cols = character(0))
  expect_equal(nrow(result), 0)
})

test_that("unnest_fun with keep_empty", {
  data <- data.frame(id = 1:2, x = c("a", "b"), stringsAsFactors = FALSE)
  data$vals <- list(c(1, 2), numeric(0))
  result <- unnest_fun(data, cols = "vals", keep_empty = TRUE)
  expect_equal(nrow(result), 3)
  expect_true(is.na(result$vals[3]))
})

test_that("remove_space works correctly", {
  expect_equal(remove_space(" hello  world "), "hello world ")
  expect_equal(
    remove_space(" hello  world ", trim_end = TRUE),
    "hello world"
  )
  expect_equal(
    remove_space(" hello  world ", trim_start = FALSE),
    " hello world "
  )
  expect_equal(
    remove_space(" hello  world ", collapse_multiple = FALSE),
    "hello  world "
  )
})

test_that("remove_space handles NULL and factor", {
  expect_null(remove_space(NULL))
  expect_equal(remove_space(factor(" a  b ")), "a b ")
})

test_that("remove_space errors on non-character", {
  expect_error(remove_space(123))
})

test_that("remove_space preserves newlines", {
  expect_equal(
    remove_space("hello\n  world"),
    "hello\nworld"
  )
  expect_equal(
    remove_space("hello\n  world", preserve_newlines = FALSE),
    "hello world"
  )
})

test_that("max_depth works correctly", {
  expect_equal(max_depth(list(a = 1)), 1)
  expect_equal(max_depth(list(a = list(b = 1))), 2)
  expect_equal(max_depth(list(a = list(b = list(c = 1)))), 3)
  expect_equal(max_depth(1), 0)
})

test_that("is_outlier detects outliers", {
  x <- c(1, 2, 3, 4, 5, 100)
  out <- is_outlier(x)
  expect_true(6 %in% out)
})

test_that("is_outlier handles type parameter", {
  x <- c(-100, 1, 2, 3, 4, 5, 100)
  lower_out <- is_outlier(x, type = "lower")
  higher_out <- is_outlier(x, type = "higher")
  expect_true(1 %in% lower_out)
  expect_true(7 %in% higher_out)
})

test_that("is_outlier includes NA indices", {
  x <- c(1, NA, 3, 4, 5)
  out <- is_outlier(x)
  expect_true(2 %in% out)
})

test_that("check_ci_env returns logical", {
  result <- check_ci_env()
  expect_true(is.logical(result))
})

test_that("wilkinsonp returns correct structure", {
  p <- c(0.01, 0.02, 0.03, 0.04, 0.05)
  result <- wilkinsonp(p)
  expect_true(is.list(result))
  expect_true("p" %in% names(result))
  expect_true("pr" %in% names(result))
  expect_true("r" %in% names(result))
})

test_that("wilkinsonp warns with too few valid p-values", {
  expect_message(wilkinsonp(c(-1, 2)), "at least two")
})

test_that("maximump and minimump work", {
  p <- c(0.01, 0.02, 0.03, 0.04, 0.05)
  max_res <- maximump(p)
  min_res <- minimump(p)
  expect_true(is.list(max_res))
  expect_true(is.list(min_res))
  expect_true(!is.na(max_res$p))
  expect_true(!is.na(min_res$p))
})

test_that("meanp returns correct structure", {
  p <- c(0.01, 0.02, 0.03, 0.04, 0.05)
  result <- meanp(p)
  expect_true(is.list(result))
  expect_true("z" %in% names(result))
  expect_true("p" %in% names(result))
})

test_that("meanp warns with too few valid p-values", {
  expect_message(meanp(c(0.01, 0.02, 0.03)), "at least four")
})

test_that("sump returns correct structure", {
  p <- c(0.01, 0.02, 0.03, 0.04, 0.05)
  result <- sump(p)
  expect_true(is.list(result))
  expect_true("p" %in% names(result))
  expect_true("conservativep" %in% names(result))
})

test_that("votep returns correct structure", {
  p <- c(0.01, 0.02, 0.03, 0.04, 0.05)
  result <- votep(p)
  expect_true(is.list(result))
  expect_true("p" %in% names(result))
  expect_true("pos" %in% names(result))
  expect_true("neg" %in% names(result))
})

test_that("try_get succeeds on working expression", {
  result <- suppressMessages(try_get(expr = 42, max_tries = 2))
  expect_equal(result, 42)
})

test_that("download errors on missing arguments", {
  expect_error(download())
})
