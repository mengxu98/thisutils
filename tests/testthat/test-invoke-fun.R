test_that("invoke_fun forwards values, argument names, and dots", {
  f <- function(x, y = 2, ...) list(x = x, y = y, dots = list(...))
  expect_identical(invoke_fun(f, list(1, y = 3), z = NULL), f(1, 3, z = NULL))
  expect_identical(invoke_fun(sum, list(1, 2, NA), na.rm = TRUE), 3)
  expect_identical(invoke_fun(function(...) list(...), list(x = 1), x = 2), list(x = 1, x = 2))
  expect_error(invoke_fun(function(x) x, list(x = 1), x = 2), "matched by multiple")
  expect_error(invoke_fun(function(x) x, list(x = 1, unused = 2)), "unused argument")
  expect_identical(invoke_fun(identity, list(x = NULL)), NULL)
  expect_identical(invoke_fun(identity, pairlist(x = 4)), 4)
  expect_identical(invoke_fun(function() 7, NULL), 7)
})

test_that("invoke_fun validates argument containers and function targets", {
  for (args in list(1:2, new.env(), function() 1)) {
    expect_error(invoke_fun(identity, args), "[.]args.*list")
  }
  for (fn in list(NULL, 1, "", NA_character_, c("mean", "sum"), quote(mean))) {
    expect_error(invoke_fun(fn, list(1)), "[.]fn.*function")
  }
})

test_that("string lookup follows R function lookup with or without arguments", {
  parent <- new.env(parent = baseenv())
  parent$f <- function(x = 7) x
  child <- new.env(parent = parent)
  child$f <- 100
  expect_identical(invoke_fun("f", .env = child), 7)
  expect_identical(invoke_fun("f", list(8), .env = child), 8)
  expect_error(invoke_fun("absent", .env = child), "could not find function")
  expect_identical(invoke_fun("sum", list(1, 2), .env = asNamespace("base")), 3)
})

test_that("temporary arguments do not shadow a string function target", {
  env <- new.env(parent = baseenv())
  env[["1"]] <- function(x, y = 0) "expected"
  env[["2"]] <- function(x, y = 0) "second"
  payload <- function(...) "hijacked"
  expect_identical(invoke_fun("1", list(payload), .env = env), "expected")
  expect_identical(invoke_fun("2", list(0, payload), .env = env), "second")
  expect_identical(env[["1"]](payload), "expected")
  expect_identical(ls(env), c("1", "2"))
})

test_that("string targets keep their names in captured calls", {
  env <- new.env(parent = baseenv())
  env$canonical_method <- function(x = NULL) match.call()
  empty <- invoke_fun("canonical_method", .env = env)
  supplied <- invoke_fun("canonical_method", list(x = 1), .env = env)
  expect_identical(empty[[1]], as.name("canonical_method"))
  expect_identical(supplied[[1]], as.name("canonical_method"))
})

test_that("language and formula arguments remain values", {
  expr <- quote(stop("must not evaluate"))
  formula <- local({
    offset <- 5
    ~ x + offset
  })
  value <- expression(x + 1)
  expect_identical(invoke_fun(identity, list(expr)), expr)
  expect_identical(invoke_fun(identity, list(quote(x))), quote(x))
  expect_identical(invoke_fun(identity, list(formula)), formula)
  expect_identical(invoke_fun(identity, list(value)), value)
})

test_that("explicit missing arguments preserve defaults and missing status", {
  f <- function(x = 10, y = 2) list(x = x, y = y, missing = missing(x))
  args <- list(x = rlang::missing_arg(), y = 3)
  expect_identical(invoke_fun(f, args), do.call(f, args))
  expect_identical(invoke_fun(f, list(rlang::missing_arg())), f())
  expect_identical(invoke_fun(f, x = rlang::missing_arg()), f())
  expect_error(invoke_fun(function(x) x, list(x = rlang::missing_arg())), "missing")
  expect_identical(invoke_fun(function(...) missing(..1), list(rlang::missing_arg())), TRUE)
})

test_that("caller-frame side effects stay in the documented evaluation frame", {
  env <- new.env(parent = baseenv())
  f <- function(x = 1) {
    assign("written", x, envir = parent.frame())
    parent.frame()
  }
  empty_frame <- invoke_fun(f, .env = env)
  expect_identical(empty_frame, env)
  expect_identical(env$written, 1)
  rm("written", envir = env)
  supplied_frame <- invoke_fun(f, list(2), .env = env)
  expect_identical(parent.env(supplied_frame), env)
  expect_identical(supplied_frame$written, 2)
  expect_false(exists("written", envir = env, inherits = FALSE))
})

test_that("visibility and conditions pass through without repeated execution", {
  count <- 0L
  f <- function(x) {
    count <<- count + 1L
    invisible(x)
  }
  expect_identical(withVisible(invoke_fun(f, list(4))), list(value = 4, visible = FALSE))
  expect_identical(count, 1L)
  condition <- structure(list(message = "sentinel", call = NULL), class = c("invoke_test_error", "error", "condition"))
  result <- tryCatch(invoke_fun(function(x) stop(condition), list(1)), error = identity)
  expect_identical(result, condition)
  expect_warning(invoke_fun(function(x) warning("sentinel warning"), list(1)), "sentinel warning")
})

test_that("S3 and S4 methods receive the same argument values", {
  values <- factor(c("b", "a", "b"))
  expect_identical(invoke_fun(as.character, list(values)), as.character(values))
  mat <- Matrix::Matrix(matrix(c(0, 2, 3, 0, 5, 0), nrow = 2), sparse = TRUE)
  expect_identical(invoke_fun(Matrix::t, list(mat)), Matrix::t(mat))
})

test_that("large objects stay outside captured calls", {
  value <- matrix(seq_len(10000), nrow = 100)
  f <- function(x) list(value = x, call = match.call())
  result <- invoke_fun(f, list(value))
  expect_identical(result$value, value)
  expect_lt(nchar(paste(deparse(result$call), collapse = "\n")), 100)
})

test_that("wide calls and partially named calls preserve values", {
  args <- as.list(seq_len(1000))
  expect_identical(invoke_fun(sum, args), do.call(sum, args))
  named <- stats::setNames(list(1, NULL, quote(x), 4), c("", "x", "x", NA_character_))
  f <- function(...) list(...)
  expect_identical(invoke_fun(f, named), do.call(f, named, quote = TRUE))
})
