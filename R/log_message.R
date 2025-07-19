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
#' The symbol used for indentation.
#' When specified, it ignores the level parameter and uses the symbol directly.
#' @param text_color Character value, default is *`NULL`*.
#' Color for the message text.
#' Available colors: "red", "green", "blue", "yellow", "magenta", "cyan", "white", "black".
#' @param back_color Character value, default is *`NULL`*.
#' Background color for the message text.
#' Available colors: "red", "green", "blue", "yellow", "magenta", "cyan", "white", "black".
#' @param multiline_indent Logical value, default is *`TRUE`*.
#' Whether to apply consistent formatting (timestamp and indentation) to each line in multiline messages.
#' When TRUE, each line gets the full formatting; when FALSE, only the first line gets the timestamp.
#' @param .envir The environment to evaluate calls in. Default is *`parent.frame()`*.
#' @param .frame The frame to use for error reporting. Default is *`.envir`*.
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
#' options(log_message.verbose = FALSE)
#' log_message("Hello, world!")
#' options(log_message.verbose = TRUE)
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
#'
#' # cli variables
#' fun <- function(x) {
#'   log_message("{.val x}")
#'   log_message("{.val {x}}")
#'   log_message("{.val {x + 1}}")
#' }
#' fun(1)
log_message <- function(
    ...,
    verbose = TRUE,
    message_type = c("info", "success", "warning", "error"),
    cli_model = TRUE,
    timestamp = TRUE,
    timestamp_format = "%Y-%m-%d %H:%M:%S",
    level = 1,
    symbol = "  ",
    text_color = NULL,
    back_color = NULL,
    multiline_indent = TRUE,
    .envir = parent.frame(),
    .frame = .envir) {
  verbose <- .get_verbose(verbose)
  message_type <- match.arg(message_type)
  msg <- .build_message(...)

  .envir <- .get_caller_env(.envir, .frame)
  caller_call <- .get_caller_call(.frame)

  if (message_type == "error") {
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
    back_color = back_color,
    .frame = .frame
  )

  if (cli_model && grepl("\n", msg)) {
    .output_multiline_message(
      msg = msg,
      message_type = message_type,
      text_color = text_color,
      back_color = back_color,
      timestamp = timestamp,
      timestamp_format = timestamp_format,
      level = level,
      symbol = symbol,
      multiline_indent = multiline_indent,
      .envir = .envir
    )
  } else {
    msg <- .message_formatting(
      msg = msg,
      timestamp = timestamp,
      timestamp_format = timestamp_format,
      level = level,
      symbol = symbol,
      multiline_indent = multiline_indent,
      .envir = .envir
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
      multiline_indent = multiline_indent,
      .envir = .envir
    )
  }

  invisible(NULL)
}

.get_verbose <- function(verbose) {
  verbose_option <- getOption("log_message.verbose", NULL)

  if (is.null(verbose_option)) {
    return(verbose)
  }

  if (!is.logical(verbose_option) || length(verbose_option) != 1) {
    cli::cli_alert_warning(
      "{.arg log_message.verbose} must be a logical value, set to {.val {verbose}}"
    )
    options(log_message.verbose = verbose)
    verbose <- FALSE
  }

  if (isTRUE(verbose_option) && isTRUE(verbose)) {
    verbose <- TRUE
  }

  if (isFALSE(verbose_option)) {
    verbose <- FALSE
  }

  verbose
}

.build_message <- function(...) {
  args <- list(...)

  if (length(args) == 0) {
    return("")
  }

  msg <- paste0(...)

  capitalize(msg)
}

.validate_params <- function(
    level,
    symbol,
    timestamp_format,
    text_color,
    back_color,
    .frame) {
  caller_call <- .get_caller_call(.frame)

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

.message_formatting <- function(
    msg,
    timestamp = TRUE,
    timestamp_format = "%Y-%m-%d %H:%M:%S",
    level = 1,
    symbol = "  ",
    multiline_indent = TRUE,
    .envir = parent.frame()) {
  if (multiline_indent && grepl("\n", msg)) {
    return(
      .format_message(
        msg = msg,
        timestamp = timestamp,
        timestamp_format = timestamp_format,
        level = level,
        symbol = symbol,
        .envir = .envir
      )
    )
  }

  .format_line(
    msg = msg,
    timestamp = timestamp,
    timestamp_format = timestamp_format,
    level = level,
    symbol = symbol,
    .envir = .envir
  )
}

.format_line <- function(
    msg,
    timestamp = TRUE,
    timestamp_format = "%Y-%m-%d %H:%M:%S",
    level = 1,
    symbol = "  ",
    .envir = parent.frame()) {
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
    symbol = "  ",
    .envir = parent.frame()) {
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
    multiline_indent = TRUE,
    .envir = parent.frame()) {
  message_type <- match.arg(message_type)

  if (cli_model) {
    msg <- .preprocess_cli_variables(msg, .envir)

    if (!is.null(text_color) || !is.null(back_color)) {
      msg <- .color_formatting(
        msg = msg,
        text_color = text_color,
        back_color = back_color,
        cli_model = cli_model,
        .envir = .envir
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
        cli_model = cli_model,
        .envir = .envir
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

.output_multiline_message <- function(
    msg,
    message_type = c("info", "success", "warning"),
    text_color = NULL,
    back_color = NULL,
    timestamp = TRUE,
    timestamp_format = "%Y-%m-%d %H:%M:%S",
    level = 1,
    symbol = "  ",
    multiline_indent = TRUE,
    .envir = parent.frame()) {
  message_type <- match.arg(message_type)

  lines <- strsplit(msg, "\n", fixed = TRUE)[[1]]

  for (i in seq_along(lines)) {
    line <- lines[i]

    line <- .preprocess_cli_variables(line, .envir)

    if (!is.null(text_color) || !is.null(back_color)) {
      line <- .color_formatting(
        msg = line,
        text_color = text_color,
        back_color = back_color,
        cli_model = TRUE,
        .envir = .envir
      )
    }

    if (i == 1 || multiline_indent) {
      if (timestamp) {
        time_str <- format(Sys.time(), timestamp_format)
        timestamp_part <- paste0("[", time_str, "] ")
      } else {
        timestamp_part <- ""
      }

      if (symbol != "  ") {
        indent_part <- paste0(symbol, " ")
      } else if (level > 1) {
        indent_part <- paste(rep("  ", level - 1), collapse = "")
      } else {
        indent_part <- ""
      }

      formatted_line <- paste0(timestamp_part, indent_part, line)
      formatted_line <- .preprocess_cli_variables(formatted_line, .envir)

      switch(
        EXPR = message_type,
        "info" = cli::cli_alert_info(formatted_line),
        "success" = cli::cli_alert_success(formatted_line),
        "warning" = cli::cli_alert_warning(formatted_line)
      )
    } else {
      icon_width <- 2

      if (timestamp) {
        time_str <- format(Sys.time(), timestamp_format)
        timestamp_width <- nchar(paste0("[", time_str, "] "))
      } else {
        timestamp_width <- 0
      }

      level_indent_width <- if (symbol != "  ") {
        nchar(paste0(symbol, " "))
      } else if (level > 1) {
        nchar(paste(rep("  ", level - 1), collapse = ""))
      } else {
        0
      }

      total_prefix_width <- icon_width + timestamp_width + level_indent_width
      alignment_space <- paste(rep(" ", total_prefix_width), collapse = "")

      processed_line <- .preprocess_cli_variables(line, .envir)

      cli::cli_verbatim(paste0(alignment_space, processed_line))
    }
  }
}

.color_formatting <- function(
    msg,
    text_color = NULL,
    back_color = NULL,
    cli_model,
    .envir = parent.frame()) {
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

.get_caller_call <- function(frame) {
  if (is.null(frame)) {
    return(NULL)
  }

  tryCatch(
    {
      if (is.environment(frame)) {
        call <- sys.call(sys.parent())
        if (is.call(call)) {
          return(call)
        }
      }

      calls <- sys.calls()
      frames <- sys.frames()
      parents <- sys.parents()

      for (i in seq_along(calls)) {
        call <- calls[[i]]
        if (is.call(call) && !is.null(call[[1]])) {
          fun_name <- as.character(call[[1]])
          if (fun_name %in% c("log_message", ".validate_params")) {
            if (i > 1) {
              caller_call <- calls[[i - 1]]
              if (is.call(caller_call)) {
                return(caller_call)
              }
            }
          }
        }
      }

      return(NULL)
    },
    error = function(e) {
      NULL
    }
  )
}

.get_caller_env <- function(.envir, .frame) {
  if (is.null(.frame)) {
    .frame <- .envir
  }

  if (is.null(.envir)) {
    .envir <- parent.frame(2)
  }

  if (is.environment(.envir)) {
    return(.envir)
  }

  parent.frame(2)
}

.search_var_in_frames <- function(var_name) {
  tryCatch(
    {
      frames <- sys.frames()
      for (i in rev(seq_along(frames))) {
        frame <- frames[[i]]
        if (is.environment(frame) && exists(var_name, envir = frame, inherits = FALSE)) {
          return(get(var_name, envir = frame))
        }
      }

      if (exists(var_name, envir = .GlobalEnv, inherits = TRUE)) {
        return(get(var_name, envir = .GlobalEnv, inherits = TRUE))
      }

      return(NULL)
    },
    error = function(e) {
      NULL
    }
  )
}

.preprocess_cli_variables <- function(msg, .envir) {
  pattern <- "\\{\\.(val|code|emph|pkg|field|file|email|url|kbd|arg)\\s+\\{([^}]+)\\}\\}"

  while (grepl(pattern, msg)) {
    matches <- regmatches(msg, gregexpr(pattern, msg))[[1]]

    for (match in matches) {
      type <- gsub(pattern, "\\1", match)
      var_expr <- gsub(pattern, "\\2", match)

      var_value <- tryCatch(
        {
          result <- eval(parse(text = var_expr), envir = .envir)
          if (is.null(result)) {
            result <- .search_var_in_frames(var_expr)
          }
          if (is.null(result)) {
            var_expr
          } else {
            as.character(result)
          }
        },
        error = function(e) {
          tryCatch(
            {
              result <- .search_var_in_frames(var_expr)
              if (is.null(result)) {
                var_expr
              } else {
                as.character(result)
              }
            },
            error = function(e2) var_expr
          )
        }
      )

      replacement <- paste0("{.", type, " ", var_value, "}")
      msg <- gsub(match, replacement, msg, fixed = TRUE)
    }
  }

  msg
}
