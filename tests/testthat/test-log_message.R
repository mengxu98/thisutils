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

test_that("log_message captures expression output and returns value", {
  expect_message(
    result <- log_message(
      expr = {
        cat("triMean is used\n")
        42
      },
      cli_model = FALSE,
      timestamp = FALSE
    ),
    "triMean is used"
  )

  expect_equal(result, 42)
})

test_that("log_message captures expression messages and warnings", {
  expect_no_warning(
    expect_message(
      result <- log_message(
        expr = {
          message("Create a CellChat object")
          warning("Potential issue")
          "ok"
        },
        cli_model = FALSE,
        timestamp = FALSE
      ),
      "Create a CellChat object"
    )
  )

  expect_equal(result, "ok")
})

test_that("log_message captures cat output without trailing newline", {
  expect_no_warning(
    expect_message(
      result <- log_message(
        expr = {
          cat("This is standard output")
          message("This is a message")
          warning("This is a warning")
          2
        },
        cli_model = FALSE,
        timestamp = FALSE,
        message_type = "running"
      ),
      "This is standard output"
    )
  )

  expect_equal(result, 2)
})

test_that("log_message preserves output order for cat without trailing newline", {
  captured <- capture.output(
    result <- log_message(
      expr = {
        cat("This is standard output")
        message("This is a message")
        warning("This is a warning")
        2
      },
      cli_model = FALSE,
      timestamp = FALSE,
      message_type = "running"
    ),
    type = "message"
  )

  expect_equal(
    captured,
    c(
      "RUNNING: This is standard output",
      "RUNNING: This is a message",
      "WARNING: This is a warning"
    )
  )
  expect_equal(result, 2)
})

test_that("log_message evaluates expr silently when verbose is FALSE", {
  expect_silent(
    result <- log_message(
      expr = {
        cat("hidden output\n")
        message("hidden message")
        7
      },
      verbose = FALSE,
      cli_model = FALSE,
      timestamp = FALSE
    )
  )

  expect_equal(result, 7)
})

test_that("log_message disallows ask and error message types with expr", {
  expect_error(
    log_message(expr = 1 + 1, message_type = "ask")
  )
  expect_error(
    log_message(expr = 1 + 1, message_type = "error")
  )
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
