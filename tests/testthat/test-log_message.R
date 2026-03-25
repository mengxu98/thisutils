test_that("log_message info produces message", {
  expect_message(log_message("test info"))
})

test_that("log_message success produces message", {
  expect_message(log_message("test success", message_type = "success"))
})

test_that("log_message warning produces message", {
  expect_message(log_message("test warning", message_type = "warning"))
})

test_that("log_message error throws error", {
  expect_error(log_message("test error", message_type = "error"))
})

test_that("log_message respects verbose = FALSE", {
  expect_silent(log_message("silent", verbose = FALSE))
})

test_that("log_message respects global verbose option", {
  old <- options(log_message.verbose = FALSE)
  on.exit(options(old))
  expect_silent(log_message("silent"))
})

test_that("log_message error still throws even when verbose = FALSE", {
  expect_error(
    log_message("err", message_type = "error", verbose = FALSE)
  )
})

test_that("log_message with cli_model = FALSE", {
  expect_message(
    log_message("test", cli_model = FALSE)
  )
})

test_that("log_message with timestamp = FALSE", {
  expect_message(log_message("test", timestamp = FALSE))
})

test_that("log_message with text_color", {
  expect_message(log_message("red text", text_color = "red"))
})

test_that("log_message with text_style", {
  expect_message(log_message("bold text", text_style = "bold"))
})

test_that("log_message with level", {
  expect_message(log_message("indented", level = 2))
})

test_that("log_message with multiline", {
  expect_message(log_message("line1\nline2", multiline_indent = TRUE))
})

test_that("log_message errors on invalid level", {
  expect_error(log_message("test", level = -1))
  expect_error(log_message("test", level = 1.5))
})

test_that("log_message errors on same text_color and back_color", {
  expect_error(
    log_message("test", text_color = "red", back_color = "red")
  )
})

test_that("log_message errors on invalid text_style", {
  expect_error(log_message("test", text_style = "nonexistent"))
})

test_that("get_verbose defaults to TRUE", {
  old <- options(log_message.verbose = NULL)
  on.exit(options(old))
  expect_true(get_verbose())
})

test_that("get_verbose respects explicit FALSE", {
  expect_false(get_verbose(verbose = FALSE))
})

test_that("get_verbose respects global option", {
  old <- options(log_message.verbose = FALSE)
  on.exit(options(old))
  expect_false(get_verbose())
})

test_that("parse_inline_expressions evaluates expressions", {
  i <- 1
  result <- parse_inline_expressions("{.val {i}}")
  expect_true(grepl("1", result))
})

test_that("parse_inline_expressions handles arithmetic", {
  x <- 5
  y <- 10
  result <- parse_inline_expressions("{.pkg {x + y}}")
  expect_true(grepl("15", result))
})
