#' @title Print diagnostic message
#'
#' @description
#' Integrate the message printing function with the `cli` package,
#' and the [base::message] function.
#' The message could be suppressed by [base::suppressMessages].
#'
#' @md
#' @param ... Text to print.
#' @param verbose Logical value, default is *`TRUE`*.
#' Whether to print the message.
#' @param message_type Type of message, default is *`info`*.
#' Could be choose one of *`info`*, *`success`*, *`warning`*, and *`error`*.
#' @param cli_model Logical value, default is *`TRUE`*.
#' Whether to use the `cli` package to print the message.
#' @param timestamp Logical value, default is *`TRUE`*.
#' Whether to show the current time in the message.
#' @param timestamp_format Character value, default is *`"%Y-%m-%d %H:%M:%S"`*.
#' Format string for timestamp display.
#' @param level Integer value, default is *`1`*.
#' The level of the message, which affects the indentation.
#' Level 1 has no indentation, higher levels add more indentation.
#' @param symbol Character value, default is *`"  "`* (two spaces).
#' The symbol used for indentation. When specified, it ignores the level parameter and uses the symbol directly.
#' @param format Logical value, default is *`FALSE`*.
#' Whether to use sprintf-style formatting for the message.
#' @param text_color Character value, default is *`NULL`*.
#' Color for the message text.
#' Available colors: "red", "green", "blue", "yellow", "magenta", "cyan", "white", "black".
#' @param back_color Character value, default is *`NULL`*.
#' Background color for the message text.
#' Available colors: "red", "green", "blue", "yellow", "magenta", "cyan", "white", "black".
#' @param multiline_indent Logical value, default is *`TRUE`*.
#' Whether to apply consistent formatting (timestamp and indentation) to each line in multiline messages.
#' When TRUE, each line gets the full formatting; when FALSE, only the first line gets the timestamp.
#'
#' @return \code{Formated message} printed to the console.
#'
#' @references
#' \url{https://cli.r-lib.org/articles/index.html}
#'
#' @export
#' @examples
#' # basic usage
#' log_message("Hello, ", "world!")
#' log_message("hello, world!")
#' log_message("Hello, world!", timestamp = FALSE)
#' log_message("Hello, ", "world!", message_type = "success")
#' log_message("Hello, world!", message_type = "warning")
#' log_message("Hello, ", "world!", cli_model = FALSE)
#'
#' # suppress messages
#' suppressMessages(log_message("Hello, world!"))
#' log_message("Hello, world!", verbose = FALSE)
#'
#' # cli formatting
#' log_message("hello, {.arg world}!")
#' message("hello, {.arg world}!")
#' log_message("hello, {.code world}!")
#' message("hello, {.code world}!")
#' log_message("{.emph R}")
#' log_message("Hello, {.file world}!")
#' log_message("press {.kbd ENTER}")
#' log_message("address: {.email example@example.com}")
#' log_message("URL: {.url https://example.com}")
#' log_message("Some {.field field}")
#' log_message("Hello, {.pkg world}!")
#' log_message("Hello, {.val world}!")
#'
#' # set indentation
#' log_message("Hello, world!", level = 2)
#' log_message("Hello, world!", symbol = "->")
#' log_message("Hello, %s", "world!", format = TRUE)
#' log_message("Processing %d items", 42, format = TRUE)
#'
#' # multiline message
#' log_message("Line 1\nLine 2\nLine 3", multiline_indent = TRUE)
#' log_message(
#'   "Multi-line\ncolored\nmessage",
#'   text_color = "blue",
#'   multiline_indent = FALSE
#' )
#' log_message(
#'   "Multi-line\ncolored\nmessage",
#'   text_color = "blue",
#'   multiline_indent = FALSE,
#'   timestamp = FALSE
#' )
#'
#' # color formatting
#' log_message(
#'   "This is a red message",
#'   text_color = "red"
#' )
#' log_message(
#'   "This is a message with background",
#'   back_color = "cyan"
#' )
#' log_message(
#'   "This is a message with both text and background",
#'   text_color = "red",
#'   back_color = "cyan"
#' )
#' # color formatting also works with `cli_model = FALSE`
#' log_message(
#'   "This is a red message",
#'   text_color = "red",
#'   cli_model = FALSE
#' )
#' log_message(
#'   "This is a message with background",
#'   back_color = "cyan",
#'   cli_model = FALSE
#' )
#' log_message(
#'   "This is a message with both text and background",
#'   text_color = "red",
#'   back_color = "cyan",
#'   cli_model = FALSE
#' )
log_message <- function(
    ...,
    verbose = TRUE,
    message_type = c("info", "success", "warning", "error"),
    cli_model = TRUE,
    timestamp = TRUE,
    timestamp_format = "%Y-%m-%d %H:%M:%S",
    level = 1,
    symbol = "  ",
    format = FALSE,
    text_color = NULL,
    back_color = NULL,
    multiline_indent = TRUE) {
  message_type <- match.arg(message_type)
  msg <- .build_message(..., format = format)

  if (message_type == "error") {
    caller_call <- rlang::caller_call()
    cli::cli_abort(msg, call = caller_call)
  }

  if (!verbose) {
    return(invisible(NULL))
  }

  .validate_params(
    level = level,
    symbol = symbol,
    timestamp_format = timestamp_format,
    text_color = text_color,
    back_color = back_color
  )

  msg <- .message_formatting(
    msg = msg,
    timestamp = timestamp,
    timestamp_format = timestamp_format,
    level = level,
    symbol = symbol,
    multiline_indent = multiline_indent
  )

  .output_message(
    msg = msg,
    message_type = message_type,
    cli_model = cli_model,
    text_color = text_color,
    back_color = back_color,
    timestamp = timestamp,
    timestamp_format = timestamp_format,
    level = level,
    symbol = symbol,
    multiline_indent = multiline_indent
  )

  invisible(NULL)
}

.validate_params <- function(
    level,
    symbol,
    timestamp_format,
    text_color,
    back_color) {
  caller_call <- rlang::caller_call(n = 2)

  if (!is.numeric(level) || length(level) != 1 || level < 1 || level != round(level)) {
    cli::cli_abort(
      "{.arg level} must be a positive integer",
      call = caller_call
    )
  }

  if (!is.character(symbol) || length(symbol) != 1) {
    cli::cli_abort(
      "{.arg symbol} must be a single character string",
      call = caller_call
    )
  }

  if (!is.character(timestamp_format) || length(timestamp_format) != 1) {
    cli::cli_abort(
      "{.arg timestamp_format} must be a single character string",
      call = caller_call
    )
  }

  valid_colors <- c(
    "red", "green", "blue", "yellow",
    "magenta", "cyan", "white", "black"
  )

  if (!is.null(text_color)) {
    if (!is.character(text_color) || length(text_color) != 1 || !text_color %in% valid_colors) {
      cli::cli_abort(
        "{.arg text_color} must be one of: {.val {valid_colors}}",
        call = caller_call
      )
    }
  }

  if (!is.null(back_color)) {
    if (!is.character(back_color) || length(back_color) != 1 || !back_color %in% valid_colors) {
      cli::cli_abort(
        "{.arg back_color} must be one of: {.val {valid_colors}}",
        call = caller_call
      )
    }
  }

  if (!is.null(text_color) && !is.null(back_color) && text_color == back_color) {
    cli::cli_abort(
      "{.arg text_color} and {.arg back_color} cannot be the same color",
      call = caller_call
    )
  }
}

.build_message <- function(..., format = FALSE) {
  args <- list(...)

  if (length(args) == 0) {
    return("")
  }

  msg <- if (format && length(args) > 1) {
    tryCatch(
      {
        invoke_fun(sprintf, args)
      },
      error = function(e) {
        warning(
          "sprintf formatting failed, falling back to paste0",
          call. = FALSE,
          immediate. = TRUE
        )
        paste0(...)
      }
    )
  } else {
    paste0(...)
  }

  capitalize(msg)
}

.message_formatting <- function(
    msg,
    timestamp = TRUE,
    timestamp_format = "%Y-%m-%d %H:%M:%S",
    level = 1,
    symbol = "  ",
    multiline_indent = TRUE) {
  if (multiline_indent && grepl("\n", msg)) {
    return(
      .format_message(
        msg = msg,
        timestamp = timestamp,
        timestamp_format = timestamp_format,
        level = level,
        symbol = symbol
      )
    )
  }

  .format_line(
    msg = msg,
    timestamp = timestamp,
    timestamp_format = timestamp_format,
    level = level,
    symbol = symbol
  )
}

.format_line <- function(
    msg,
    timestamp = TRUE,
    timestamp_format = "%Y-%m-%d %H:%M:%S",
    level = 1,
    symbol = "  ") {
  parts <- character(0)

  if (timestamp) {
    time_str <- format(Sys.time(), timestamp_format)
    parts <- c(parts, paste0("[", time_str, "] "))
  }

  if (symbol != "  ") {
    parts <- c(parts, paste0(symbol, " "))
  } else if (level > 1) {
    indentation <- paste(
      rep(symbol, level - 1),
      collapse = ""
    )
    parts <- c(parts, indentation)
  }

  parts <- c(parts, msg)

  paste0(parts, collapse = "")
}

.format_message <- function(
    msg,
    timestamp = TRUE,
    timestamp_format = "%Y-%m-%d %H:%M:%S",
    level = 1,
    symbol = "  ") {
  lines <- strsplit(msg, "\n", fixed = TRUE)[[1]]

  timestamp_str <- if (timestamp) {
    paste0("[", format(Sys.time(), timestamp_format), "] ")
  } else {
    ""
  }

  indentation <- if (symbol != "  ") {
    paste0(symbol, " ")
  } else if (level > 1) {
    paste(rep(symbol, level - 1), collapse = "")
  } else {
    ""
  }

  formatted_lines <- paste0(timestamp_str, indentation, lines)

  paste(formatted_lines, collapse = "\n")
}

.output_message <- function(
    msg,
    message_type = c("info", "success", "warning"),
    cli_model = TRUE,
    text_color = NULL,
    back_color = NULL,
    timestamp = TRUE,
    timestamp_format = "%Y-%m-%d %H:%M:%S",
    level = 1,
    symbol = "  ",
    multiline_indent = TRUE) {
  message_type <- match.arg(message_type)

  if (cli_model) {
    if (grepl("\n", msg)) {
      .output_cli_message(
        msg = msg,
        message_type = message_type,
        text_color = text_color,
        back_color = back_color,
        cli_model = cli_model,
        timestamp = timestamp,
        timestamp_format = timestamp_format,
        level = level,
        symbol = symbol,
        multiline_indent = multiline_indent
      )
      return(invisible(NULL))
    }

    if (!is.null(text_color) || !is.null(back_color)) {
      msg <- .color_formatting(
        msg = msg,
        text_color = text_color,
        back_color = back_color,
        cli_model = cli_model
      )
    }

    switch(
      EXPR = message_type,
      "info" = cli::cli_alert_info(msg),
      "success" = cli::cli_alert_success(msg),
      "warning" = cli::cli_alert_warning(msg)
    )
  } else {
    if (!is.null(text_color) || !is.null(back_color)) {
      msg <- .color_formatting(
        msg = msg,
        text_color = text_color,
        back_color = back_color,
        cli_model = cli_model
      )
    }

    prefix <- switch(
      EXPR = message_type,
      "info" = "",
      "success" = "SUCCESS: ",
      "warning" = "WARNING: "
    )
    message(paste0(prefix, msg))
  }
}

.output_cli_message <- function(
    msg,
    message_type = c("info", "success", "warning"),
    text_color = NULL,
    back_color = NULL,
    cli_model = TRUE,
    timestamp = TRUE,
    timestamp_format = "%Y-%m-%d %H:%M:%S",
    level = 1,
    symbol = "  ",
    multiline_indent = TRUE) {
  message_type <- match.arg(message_type)

  lines <- strsplit(msg, "\n", fixed = TRUE)[[1]]

  if (length(lines) <= 1) {
    if (!is.null(text_color) || !is.null(back_color)) {
      msg <- .color_formatting(
        msg = msg,
        text_color = text_color,
        back_color = back_color,
        cli_model = cli_model
      )
    }

    switch(
      EXPR = message_type,
      "info" = cli::cli_alert_info(msg),
      "success" = cli::cli_alert_success(msg),
      "warning" = cli::cli_alert_warning(msg)
    )
    return(invisible(NULL))
  }

  if (!is.null(text_color) || !is.null(back_color)) {
    lines <- sapply(lines, function(line) {
      .color_formatting(
        msg = line,
        text_color = text_color,
        back_color = back_color,
        cli_model = cli_model
      )
    }, USE.NAMES = FALSE)
  }

  first_line <- lines[1]
  switch(
    EXPR = message_type,
    "info" = cli::cli_alert_info(first_line),
    "success" = cli::cli_alert_success(first_line),
    "warning" = cli::cli_alert_warning(first_line)
  )

  if (length(lines) > 1) {
    remaining_lines <- lines[-1]

    if (multiline_indent) {
      icon_space <- "  "
    } else {
      icon_width <- 2

      timestamp_width <- if (timestamp) {
        sample_time <- format(Sys.time(), timestamp_format)
        nchar(paste0("[", sample_time, "] "))
      } else {
        0
      }

      level_indent_width <- if (symbol != "  ") {
        nchar(paste0(symbol, " "))
      } else if (level > 1) {
        nchar(paste(rep(symbol, level - 1), collapse = ""))
      } else {
        0
      }

      total_prefix_width <- icon_width + timestamp_width + level_indent_width
      icon_space <- paste(rep(" ", total_prefix_width), collapse = "")
    }

    for (line in remaining_lines) {
      aligned_line <- paste0(icon_space, line)
      cli::cli_verbatim(aligned_line)
    }
  }
}

.color_formatting <- function(
    msg,
    text_color = NULL,
    back_color = NULL,
    cli_model) {
  if (cli_model) {
    result <- msg

    if (!is.null(text_color)) {
      text_color_func <- switch(
        EXPR = text_color,
        "red" = cli::col_red,
        "green" = cli::col_green,
        "blue" = cli::col_blue,
        "yellow" = cli::col_yellow,
        "magenta" = cli::col_magenta,
        "cyan" = cli::col_cyan,
        "white" = cli::col_white,
        "black" = cli::col_black,
        function(x) x
      )
      result <- text_color_func(result)
    }

    if (!is.null(back_color)) {
      back_color_func <- switch(
        EXPR = back_color,
        "red" = cli::bg_red,
        "green" = cli::bg_green,
        "blue" = cli::bg_blue,
        "yellow" = cli::bg_yellow,
        "magenta" = cli::bg_magenta,
        "cyan" = cli::bg_cyan,
        "white" = cli::bg_white,
        "black" = cli::bg_black,
        function(x) x
      )
      result <- back_color_func(result)
    }

    result
  } else {
    text_ansi_codes <- list(
      "red" = "\033[31m",
      "green" = "\033[32m",
      "blue" = "\033[34m",
      "yellow" = "\033[33m",
      "magenta" = "\033[35m",
      "cyan" = "\033[36m",
      "white" = "\033[37m",
      "black" = "\033[30m"
    )

    back_ansi_codes <- list(
      "red" = "\033[41m",
      "green" = "\033[42m",
      "blue" = "\033[44m",
      "yellow" = "\033[43m",
      "magenta" = "\033[45m",
      "cyan" = "\033[46m",
      "white" = "\033[47m",
      "black" = "\033[40m"
    )

    reset_code <- "\033[0m"
    codes <- character(0)

    if (!is.null(text_color)) {
      text_code <- text_ansi_codes[[text_color]]
      if (!is.null(text_code)) {
        codes <- c(codes, text_code)
      }
    }

    if (!is.null(back_color)) {
      back_code <- back_ansi_codes[[back_color]]
      if (!is.null(back_code)) {
        codes <- c(codes, back_code)
      }
    }

    if (length(codes) > 0) {
      paste0(paste(codes, collapse = ""), msg, reset_code)
    } else {
      msg
    }
  }
}
