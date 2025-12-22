#' @title Print formatted message
#'
#' @description
#' Integrate the message printing function with the \href{https://cli.r-lib.org}{cli} package,
#' and the [base::message] function.
#' The message could be suppressed by [base::suppressMessages].
#'
#' @md
#' @param ... The message to print.
#' @param verbose Whether to print the message.
#' Default is `TRUE`.
#' @param message_type Type of message.
#' Could be choose one of `"info"`, `"success"`, `"warning"`, `"error"`, `"running"`, and `"ask"`.
#' When `"ask"` is used, the function will prompt the user for a Yes/No/Cancel response using [utils::askYesNo],
#' and returns `TRUE` for Yes, `FALSE` for No, and `NA` for Cancel.
#' Default is `"info"`.
#' @param cli_model Whether to use the `cli` package to print the message.
#' Default is `TRUE`.
#' @param level The level of the message, which affects the indentation.
#' Level `1` has no indentation, higher levels add more indentation.
#' Default is `1`.
#' @param symbol The symbol used for indentation.
#' When specified, it ignores the level parameter and uses the symbol directly.
#' Default is `"  "` (two spaces).
#' @param text_color Color for the message text.
#' Supports R color names (e.g., `"orange"`),
#' hexadecimal colors (e.g., `"#000000"`),
#' basic colors: `"red"`, `"green"`, `"blue"`, `"yellow"`, `"magenta"`,
#' `"cyan"`, `"white"`, `"black"`, `"grey"`, `"silver"`, `"none"`,
#' and bright colors: `"br_red"`, `"br_green"`, `"br_blue"`,
#' `"br_yellow"`, `"br_magenta"`, `"br_cyan"`, `"br_white"`, `"br_black"`.
#' Default is `NULL`.
#' @param back_color Background color for the message text.
#' Details see parameter `text_color`.
#' Default is `NULL`.
#' @param text_style Text styles to apply.
#' Can be one or more of:
#' `"bold"`, `"italic"`, `"underline"`, `"strikethrough"`, `"dim"`, `"inverse"`.
#' Multiple styles can be combined (e.g., `c("bold", "underline")`).
#' Default is `NULL`.
#' @param timestamp Whether to show the current time in the message.
#' Default is `TRUE`.
#' @param timestamp_format Format string for timestamp display.
#' Default is `"%Y-%m-%d %H:%M:%S"`.
#' @param multiline_indent Whether to apply consistent formatting (timestamp and indentation) to each line in multiline messages.
#' When `TRUE`, each line gets the full formatting; when `FALSE`, only the first line gets the timestamp.
#' Default is `FALSE`.
#' @param timestamp_style Whether to apply the same text styling to the timestamp as the message text.
#' When `TRUE`, timestamp formatting matches the message;
#' when `FALSE`, timestamp keeps its default appearance.
#' Default is `FALSE`.
#' @param plain_text Whether to print only the text content.
#' When `TRUE`, level, symbol, timestamp, and message type formatting are suppressed,
#' but color and multiline settings still apply.
#' @param .envir The environment to evaluate calls in.
#' Default is [parent.frame].
#' @param .frame The frame to use for error reporting.
#' Default is `.envir`.
#'
#' @return
#' Formated message, or a logical value (`TRUE`/`FALSE`/`NA`) if `message_type = "ask"`.
#'
#' @references
#' \url{https://cli.r-lib.org/articles/index.html}
#'
#' @export
#' @examples
#' # basic usage
#' log_message("Hello, ", "world!")
#'
#' log_message("hello, world!")
#'
#' log_message("Hello, world!", timestamp = FALSE)
#'
#' log_message(
#'   "Hello, ", "world!",
#'   message_type = "success"
#' )
#'
#' log_message(
#'   "Hello, world!",
#'   message_type = "warning"
#' )
#'
#' log_message(
#'   "Processing data...",
#'   message_type = "running"
#' )
#'
#' log_message(
#'   "Hello, ", "world!",
#'   cli_model = FALSE
#' )
#'
#'
#' # suppress messages
#' suppressMessages(log_message("Hello, world!"))
#' log_message("Hello, world!", verbose = FALSE)
#' options(log_message.verbose = FALSE)
#' log_message("Hello, world!")
#'
#' # for global verbose option
#' options(log_message.verbose = TRUE)
#' log_message("Hello, world!", verbose = FALSE)
#' options(log_message.verbose = NULL)
#'
#'
#' # cli inline markup
#' log_message("{.arg abc} is a argument")
#'
#' ## 'message' can not deal with cli inline markup
#' message("hello, {.code world}!")
#'
#' log_message("{.val list('abc')} is a {.cls {class(list('abc'))}}")
#'
#' log_message("{.code lm(y ~ x)} is a code example")
#'
#' log_message("{.dt List}list('abc')")
#'
#' log_message("address: {.email example@example.com}")
#'
#' log_message("{.emph R} is a programming language")
#'
#' log_message("{.envvar R_HOME}")
#'
#' log_message("{.file log_message.R} is a file")
#'
#' log_message("{.fn lm} is a function")
#'
#' log_message("{.fun lm} is a function")
#'
#' log_message("{.help lm} to get help")
#'
#' log_message("... see {.help [{.fun lm}](stats::lm)} to learn more")
#'
#' log_message(
#'   "See the {.href [cli homepage](https://cli.r-lib.org)} for details"
#' )
#'
#' log_message("press {.kbd ENTER}")
#'
#' log_message("press {.key ENTER}")
#'
#' log_message("URL: {.url https://cli.r-lib.org}")
#'
#' log_message("Some {.field field}")
#'
#' log_message("{.path /usr/bin/R} is a path")
#'
#' log_message("{.pkg cli} is a package")
#'
#' log_message("{.val object} is a variable")
#'
#' log_message("{.run Rscript log_message.R} is a runnable file")
#'
#' log_message("{.str object} is a string")
#'
#' log_message("{.strong abc} is a strong string")
#'
#' log_message("{.topic stats::lm} is a topic")
#'
#' log_message("{.vignette cli} is a vignette")
#'
#'
#' # set indentation
#' log_message("Hello, world!", level = 2)
#'
#' log_message("Hello, world!", symbol = "->")
#'
#' log_message(
#'   "Hello, world!",
#'   symbol = "#####",
#'   level = 3
#' )
#'
#' # color formatting
#' log_message(
#'   "This is a red message",
#'   text_color = "#ff9900"
#' )
#'
#' log_message(
#'   "This is a message with background",
#'   back_color = "#EE4000"
#' )
#'
#' log_message(
#'   "This is a message with both text and background",
#'   text_color = "white",
#'   back_color = "cyan"
#' )
#'
#' log_message(
#'   "This is a message with background",
#'   back_color = "#EE4000",
#'   cli_model = FALSE
#' )
#'
#' log_message(
#'   "This is a message with both text and background",
#'   text_color = "red",
#'   back_color = "cyan",
#'   cli_model = FALSE
#' )
#'
#' log_message(
#'   "Hex color with {.arg cli_model = FALSE}",
#'   text_color = "#FF5733",
#'   cli_model = FALSE
#' )
#'
#' log_message(
#'   "Bright red text",
#'   text_color = "br_red"
#' )
#'
#' log_message(
#'   "Bright background",
#'   back_color = "br_yellow"
#' )
#'
#' log_message(
#'   "Combined grey and style",
#'   text_color = "grey",
#'   text_style = "bold"
#' )
#'
#' # text style formatting
#' log_message(
#'   "Bold message",
#'   text_style = "bold"
#' )
#'
#' log_message(
#'   "Italic message",
#'   text_style = "italic"
#' )
#'
#' log_message(
#'   "Underlined message",
#'   text_style = "underline"
#' )
#'
#' log_message(
#'   "Combined styles",
#'   text_style = c("bold", "underline")
#' )
#'
#' log_message(
#'   "Color and style",
#'   text_color = "blue",
#'   text_style = c("bold", "italic")
#' )
#'
#' log_message(
#'   "Hex color and style",
#'   text_color = "#FF5733",
#'   text_style = c("bold", "underline")
#' )
#'
#'
#' # multiline message
#' log_message(
#'   "Line 1\nLine 2\nLine 3",
#'   multiline_indent = TRUE,
#'   text_style = "italic"
#' )
#'
#' log_message(
#'   "Multi-line\ncolored\nmessage",
#'   text_color = "blue",
#'   text_style = "italic"
#' )
#'
#' log_message(
#'   "Multi-line\ncolored\nmessage",
#'   text_color = "blue",
#'   timestamp = FALSE
#' )
#'
#' # timestamp styling
#' log_message(
#'   "Multi-line message\nwith timestamp styling",
#'   text_color = "red",
#'   text_style = "bold",
#'   timestamp_style = TRUE
#' )
#'
#' log_message(
#'   "Multi-line message\nwithout timestamp styling",
#'   text_color = "#669999",
#'   text_style = c("bold", "italic")
#' )
#'
#'
#' # combine cli package and log_message
#' log_message(
#'   cli::col_green(
#'     "I am a green line ",
#'     cli::col_blue(
#'       cli::style_underline(
#'         cli::style_bold("with a blue substring")
#'       )
#'     ),
#'     " that becomes green again!"
#'   )
#' )
#'
#' # cli variables
#' fun <- function(x = 1) {
#'   log_message("{.val x}")
#'   log_message("{.val {x}}")
#'   log_message("{.val {x + 1}}")
#' }
#' fun()
#'
#'
#' # print objects directly
#' df <- data.frame(
#'   x = 1:3,
#'   y = letters[1:3],
#'   z = c(" a", "b  ", "c")
#' )
#' log_message("Content:\n", df)
#'
#' # interactive prompt
#' if (interactive()) {
#'   log_message(
#'     "Do you want to continue?",
#'     message_type = "ask"
#'   )
#' }
log_message <- function(
    ...,
    verbose = TRUE,
    message_type = c(
      "info", "success", "warning", "error", "running", "ask"
    ),
    cli_model = TRUE,
    level = 1,
    symbol = "  ",
    text_color = NULL,
    back_color = NULL,
    text_style = NULL,
    multiline_indent = FALSE,
    timestamp = TRUE,
    timestamp_format = paste0(
      "[", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "] "
    ),
    timestamp_style = FALSE,
    plain_text = FALSE,
    .envir = parent.frame(),
    .frame = .envir) {
  verbose <- get_verbose(verbose)
  message_type <- match.arg(message_type)
  msg <- build_message(...)

  if (is.null(.envir) || !is.environment(.envir)) {
    .envir <- parent.frame()
  }
  caller_call <- get_caller_call(.frame)

  if (message_type == "error") {
    cli::cli_abort(msg, call = caller_call, .envir = .envir)
  }

  if (!verbose) {
    return(invisible(NULL))
  }

  validate_params(
    level = level,
    symbol = symbol,
    text_color = text_color,
    back_color = back_color,
    text_style = text_style,
    timestamp_style = timestamp_style,
    .frame = .frame
  )

  if (message_type == "ask") {
    output_message(
      msg = msg,
      message_type = message_type,
      cli_model = cli_model,
      text_color = text_color,
      back_color = back_color,
      text_style = text_style,
      timestamp = timestamp,
      timestamp_format = timestamp_format,
      level = level,
      symbol = symbol,
      multiline_indent = multiline_indent,
      timestamp_style = timestamp_style,
      plain_text = plain_text,
      .envir = .envir
    )
    return(utils::askYesNo(msg = ""))
  }

  output_message(
    msg = msg,
    message_type = message_type,
    cli_model = cli_model,
    text_color = text_color,
    back_color = back_color,
    text_style = text_style,
    timestamp = timestamp,
    timestamp_format = timestamp_format,
    level = level,
    symbol = symbol,
    multiline_indent = multiline_indent,
    timestamp_style = timestamp_style,
    plain_text = plain_text,
    .envir = .envir
  )

  invisible(NULL)
}

#' @title Get the verbose option
#'
#' @description
#' Get the verbose option from the global options or the local argument.
#'
#' @param verbose The verbose option.
#' Default is `NULL`, which means to get the verbose option from the global options.
#' @return The verbose option.
#' @export
#'
#' @examples
#' get_verbose()
#' get_verbose(verbose = FALSE)
#' get_verbose(verbose = TRUE)
#'
#' options(log_message.verbose = FALSE)
#' get_verbose()
#' get_verbose(verbose = TRUE)
#'
#' options(log_message.verbose = TRUE)
#' get_verbose()
#'
#' options(log_message.verbose = NULL)
get_verbose <- function(verbose = NULL) {
  verbose_global <- getOption("log_message.verbose", NULL)

  if (is.null(verbose_global)) {
    if (is.null(verbose)) {
      verbose <- TRUE
    } else {
      if (!is.logical(verbose) || length(verbose) != 1) {
        cli::cli_alert_warning(
          "{.arg verbose} is not a logical value, set to {.pkg TRUE}"
        )
        verbose <- TRUE
      }
    }
  } else {
    if (!is.logical(verbose_global) || length(verbose_global) != 1) {
      cli::cli_alert_warning(
        "{.arg log_message.verbose} is not a logical value, set to {.pkg NULL}"
      )
      cli::cli_alert_warning(
        "or using {.code options(log_message.verbose = TRUE/FALSE)}"
      )
      options(log_message.verbose = NULL)
      verbose <- FALSE
    } else {
      verbose <- verbose_global
    }
  }

  verbose
}

build_message <- function(...) {
  args <- list(...)

  if (length(args) == 0) {
    return("")
  }

  processed_args <- lapply(
    args, function(arg) {
      if (is.character(arg) && length(arg) == 1) {
        return(arg)
      } else if (is.character(arg)) {
        return(paste(arg, collapse = ""))
      } else {
        tryCatch(
          {
            output <- utils::capture.output(print(arg))
            if (length(output) > 0) {
              return(paste(output, collapse = "\n"))
            } else {
              return(as.character(arg))
            }
          },
          error = function(e) {
            return(as.character(arg))
          }
        )
      }
    }
  )

  msg <- paste0(processed_args, collapse = "")

  if (length(msg) != 1) {
    msg <- paste(msg, collapse = "")
  }

  capitalize(msg)
}

validate_params <- function(
    level,
    symbol,
    text_color,
    back_color,
    text_style,
    timestamp_style,
    .frame) {
  caller_call <- get_caller_call(.frame)

  if (!is.numeric(level) || length(level) != 1 || level < 1 || level != round(level)) {
    cli::cli_abort(
      "{.arg level} must be a positive integer",
      call = caller_call,
      .envir = parent.frame()
    )
  }

  if (!is.character(symbol) || length(symbol) != 1) {
    cli::cli_abort(
      "{.arg symbol} must be a single character string",
      call = caller_call,
      .envir = parent.frame()
    )
  }

  validate_color_param <- function(color_value,
                                   param_name,
                                   caller_call) {
    if (!is.null(color_value) && !check_color(color_value)) {
      error_msg <- paste0(
        "{.arg {param_name}} must be a valid color name, ",
        "hexadecimal color code (e.g., '#000000'), or R color name"
      )
      cli::cli_abort(
        error_msg,
        call = caller_call,
        .envir = parent.frame(2)
      )
    }
  }

  validate_color_param(text_color, "text_color", caller_call)
  validate_color_param(back_color, "back_color", caller_call)

  if (!is.null(text_color) && !is.null(back_color) && text_color == back_color) {
    cli::cli_abort(
      "{.arg text_color} and {.arg back_color} cannot be the same color",
      call = caller_call,
      .envir = parent.frame()
    )
  }

  if (!is.null(text_style)) {
    valid_styles <- c(
      "bold", "italic", "underline", "strikethrough", "dim", "inverse"
    )
    if (!all(text_style %in% valid_styles)) {
      cli::cli_abort(
        "{.arg text_style} must be one or more of: {.val {valid_styles}}",
        call = caller_call,
        .envir = parent.frame()
      )
    }
  }

  if (!is.logical(timestamp_style) || length(timestamp_style) != 1) {
    cli::cli_abort(
      "{.arg timestamp_style} must be a single logical value",
      call = caller_call,
      .envir = parent.frame()
    )
  }
}

get_indent_part <- function(symbol, level) {
  if (symbol != "  ") {
    paste0(paste(rep(symbol, level), collapse = ""), " ")
  } else if (level > 1) {
    paste(rep("  ", level - 1), collapse = "")
  } else {
    ""
  }
}

format_line_with_style <- function(
    line,
    prefix,
    text_color,
    back_color,
    text_style,
    timestamp_style,
    .envir) {
  if (is.null(text_color) && is.null(back_color) && is.null(text_style)) {
    return(paste0(prefix, line))
  }

  if (timestamp_style) {
    full_line <- paste0(prefix, line)
    style_formatting(
      msg = full_line,
      text_color = text_color,
      back_color = back_color,
      text_style = text_style,
      cli_model = TRUE,
      .envir = .envir
    )
  } else {
    styled_line <- style_formatting(
      msg = line,
      text_color = text_color,
      back_color = back_color,
      text_style = text_style,
      cli_model = TRUE,
      .envir = .envir
    )
    paste0(prefix, styled_line)
  }
}

output_cli_message <- function(
    message,
    message_type,
    .envir = parent.frame()) {
  switch(
    EXPR = message_type,
    "info" = cli::cli_alert_info(message, .envir = .envir),
    "success" = cli::cli_alert_success(message, .envir = .envir),
    "warning" = cli::cli_alert_warning(message, .envir = .envir),
    "running" = cli::cli_text(
      paste0(
        cli::make_ansi_style("orange")(cli::symbol$circle_dotted), " ", message
      ),
      .envir = .envir
    ),
    "ask" = {
      magenta_style <- cli::make_ansi_style("magenta")
      cli::cli_text(
        paste0(magenta_style("?"), " ", message),
        .envir = .envir
      )
    }
  )
}

output_message <- function(
    msg,
    message_type,
    cli_model,
    text_color,
    back_color,
    text_style,
    timestamp,
    timestamp_format,
    level,
    symbol,
    multiline_indent,
    timestamp_style,
    plain_text,
    .envir = parent.frame()) {
  if (plain_text) {
    plain_msg <- if (length(msg) == 1) {
      msg
    } else {
      paste(msg, collapse = "")
    }

    if (length(plain_msg) == 1 && grepl("\n", plain_msg)) {
      lines <- strsplit(plain_msg, "\n", fixed = TRUE)[[1]]
      for (line in lines) {
        plain_text_output(
          text = line,
          cli_model = cli_model,
          text_color = text_color,
          back_color = back_color,
          text_style = text_style,
          .envir = .envir
        )
      }
    } else {
      plain_text_output(
        text = plain_msg,
        cli_model = cli_model,
        text_color = text_color,
        back_color = back_color,
        text_style = text_style,
        .envir = .envir
      )
    }

    return(invisible(NULL))
  }

  if (cli_model && length(msg) == 1 && grepl("\n", msg)) {
    lines <- strsplit(msg, "\n", fixed = TRUE)[[1]]

    for (i in seq_along(lines)) {
      line <- lines[i]

      if (i == 1 || multiline_indent) {
        timestamp_part <- if (timestamp) {
          timestamp_format
        } else {
          ""
        }
        indent_part <- get_indent_part(symbol, level)
        prefix <- paste0(timestamp_part, indent_part)
      } else {
        indent_part <- get_indent_part(symbol, level)
        alignment_spaces <- if (timestamp) {
          timestamp_width <- nchar(
            timestamp_format
          )
          paste(rep(" ", timestamp_width), collapse = "")
        } else {
          ""
        }
        prefix <- paste0(alignment_spaces, indent_part)
      }

      formatted_line <- format_line_with_style(
        line = line,
        prefix = prefix,
        text_color = text_color,
        back_color = back_color,
        text_style = text_style,
        timestamp_style = timestamp_style,
        .envir = .envir
      )
      output_cli_message(
        message = formatted_line,
        message_type = message_type,
        .envir = .envir
      )
    }
    return(invisible(NULL))
  }

  if (cli_model) {
    timestamp_part <- if (timestamp) {
      timestamp_format
    } else {
      ""
    }
    indent_part <- get_indent_part(symbol, level)

    if (symbol != "  ") {
      final_msg <- paste0(
        timestamp_part,
        paste(rep(symbol, level), collapse = ""), " ", msg
      )
    } else {
      final_msg <- paste0(
        timestamp_part, indent_part, msg
      )
    }

    if (!is.null(text_color) || !is.null(back_color) || !is.null(text_style)) {
      if (timestamp_style) {
        final_msg <- style_formatting(
          msg = final_msg,
          text_color = text_color,
          back_color = back_color,
          text_style = text_style,
          cli_model = cli_model,
          .envir = .envir
        )
      } else {
        styled_msg <- style_formatting(
          msg = msg,
          text_color = text_color,
          back_color = back_color,
          text_style = text_style,
          cli_model = cli_model,
          .envir = .envir
        )
        final_msg <- paste0(
          timestamp_part,
          if (symbol != "  ") {
            paste(rep(symbol, level), collapse = "")
          } else {
            indent_part
          },
          if (symbol != "  ") {
            " "
          } else {
            ""
          },
          styled_msg
        )
      }
    }

    output_cli_message(
      message = final_msg,
      message_type = message_type,
      .envir = .envir
    )
  } else {
    formatted_msg <- tryCatch(
      cli::format_inline(msg, .envir = .envir),
      error = function(e) {
        msg
      }
    )

    if (!is.null(text_color) || !is.null(back_color) || !is.null(text_style)) {
      formatted_msg <- style_formatting(
        msg = formatted_msg,
        text_color = text_color,
        back_color = back_color,
        text_style = text_style,
        cli_model = cli_model,
        .envir = .envir
      )
    }

    prefix <- switch(
      EXPR = message_type,
      "info" = "",
      "success" = "SUCCESS: ",
      "warning" = "WARNING: ",
      "running" = "RUNNING: ",
      "ask" = "? "
    )
    message(paste0(prefix, formatted_msg))
  }
}

style_formatting <- function(
    msg,
    text_color,
    back_color,
    text_style,
    cli_model,
    .envir = parent.frame()) {
  if (is.null(text_color) && is.null(back_color) && is.null(text_style)) {
    return(msg)
  }

  if (!is.null(text_color)) {
    text_fun <- make_color_style(text_color)
    msg <- text_fun(msg)
  }

  if (!is.null(back_color)) {
    back_fun <- make_color_style(back_color, bg = TRUE)
    msg <- back_fun(msg)
  }

  if (!is.null(text_style)) {
    for (style in text_style) {
      style_fun <- switch(
        EXPR = style,
        "bold" = cli::style_bold,
        "italic" = cli::style_italic,
        "underline" = cli::style_underline,
        "strikethrough" = cli::style_strikethrough,
        "dim" = cli::style_dim,
        "inverse" = cli::style_inverse,
        function(x) x
      )

      msg <- style_fun(msg)
    }
  }

  msg
}

plain_text_output <- function(
    text,
    cli_model,
    text_color,
    back_color,
    text_style,
    .envir = parent.frame()) {
  if (cli_model) {
    if (!is.null(text_color) || !is.null(back_color) || !is.null(text_style)) {
      text <- style_formatting(
        msg = text,
        text_color = text_color,
        back_color = back_color,
        text_style = text_style,
        cli_model = cli_model,
        .envir = .envir
      )
    }
    cli::cli_verbatim(text, .envir = .envir)
  } else {
    formatted_text <- tryCatch(
      cli::format_inline(text, .envir = .envir),
      error = function(e) {
        text
      }
    )

    if (!is.null(text_color) || !is.null(back_color) || !is.null(text_style)) {
      formatted_text <- style_formatting(
        msg = formatted_text,
        text_color = text_color,
        back_color = back_color,
        text_style = text_style,
        cli_model = cli_model,
        .envir = .envir
      )
    }

    message(formatted_text)
  }
}

get_caller_call <- function(
    frame = parent.frame(),
    max_depth = 10) {
  if (is.null(frame)) {
    return(NULL)
  }

  tryCatch(
    {
      internal_functions <- c(
        "log_message",
        "validate_params",
        "output_message",
        "style_formatting",
        "check_color",
        "make_color_style",
        "extract_function_name"
      )

      calls <- sys.calls()

      if (length(calls) <= 1) {
        return(NULL)
      }

      current_frame <- sys.nframe()

      for (depth in seq_len(min(max_depth, length(calls)))) {
        target_frame <- current_frame - depth

        if (target_frame < 1 || target_frame > length(calls)) {
          next
        }

        call <- calls[[target_frame]]

        if (!is.call(call) || is.null(call[[1]])) {
          next
        }

        fun_name <- extract_function_name(call)

        if (!is.null(fun_name) && !fun_name %in% internal_functions) {
          if (!identical(call[[1]], quote(get_caller_call))) {
            return(call)
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

extract_function_name <- function(call) {
  tryCatch(
    {
      fun_part <- call[[1]]

      if (is.symbol(fun_part)) {
        return(as.character(fun_part))
      } else if (is.call(fun_part) && length(fun_part) >= 3) {
        if (as.character(fun_part[[1]]) %in% c("::", ":::")) {
          return(as.character(fun_part[[3]]))
        }
      } else if (is.character(fun_part)) {
        return(fun_part[1])
      }

      return(NULL)
    },
    error = function(e) {
      NULL
    }
  )
}

make_color_style <- function(color, bg = FALSE) {
  if (!bg) {
    text_color_map <- list(
      "black" = cli::col_black,
      "red" = cli::col_red,
      "green" = cli::col_green,
      "yellow" = cli::col_yellow,
      "blue" = cli::col_blue,
      "magenta" = cli::col_magenta,
      "cyan" = cli::col_cyan,
      "white" = cli::col_white,
      "grey" = cli::col_grey,
      "silver" = cli::col_silver,
      "none" = cli::col_none,
      "br_black" = cli::col_br_black,
      "br_red" = cli::col_br_red,
      "br_green" = cli::col_br_green,
      "br_yellow" = cli::col_br_yellow,
      "br_blue" = cli::col_br_blue,
      "br_magenta" = cli::col_br_magenta,
      "br_cyan" = cli::col_br_cyan,
      "br_white" = cli::col_br_white
    )

    if (color %in% names(text_color_map)) {
      return(text_color_map[[color]])
    }
  } else {
    bg_color_map <- list(
      "black" = cli::bg_black,
      "red" = cli::bg_red,
      "green" = cli::bg_green,
      "yellow" = cli::bg_yellow,
      "blue" = cli::bg_blue,
      "magenta" = cli::bg_magenta,
      "cyan" = cli::bg_cyan,
      "white" = cli::bg_white,
      "none" = cli::bg_none,
      "br_black" = cli::bg_br_black,
      "br_red" = cli::bg_br_red,
      "br_green" = cli::bg_br_green,
      "br_yellow" = cli::bg_br_yellow,
      "br_blue" = cli::bg_br_blue,
      "br_magenta" = cli::bg_br_magenta,
      "br_cyan" = cli::bg_br_cyan,
      "br_white" = cli::bg_br_white
    )

    if (color %in% names(bg_color_map)) {
      return(bg_color_map[[color]])
    }
  }

  return(cli::make_ansi_style(color, bg = bg))
}

check_color <- function(color) {
  if (!is.character(color) || length(color) != 1) {
    return(FALSE)
  }

  all_cli_colors <- c(
    "black", "red", "green", "yellow",
    "blue", "magenta", "cyan", "white",
    "grey", "silver", "none",
    "br_black", "br_red", "br_green", "br_yellow",
    "br_blue", "br_magenta", "br_cyan", "br_white"
  )

  if (color %in% all_cli_colors) {
    return(TRUE)
  }

  tryCatch(
    {
      invisible(cli::make_ansi_style(color))
      return(TRUE)
    },
    error = function(e) {
      FALSE
    }
  )
}

#' @title Parse inline expressions
#'
#' @description
#' Parse `{}` inline expressions and evaluate them in the current environment,
#' while preserving outer formatting markers like `\{.val ...\}`.
#'
#' @param text A character string containing inline expressions to parse.
#' @param env Environment in which to evaluate expressions.
#' Defaults to the calling environment.
#'
#' @return
#' A character string with expressions evaluated but formatting preserved.
#'
#' @export
#'
#' @examples
#' i <- 1
#' parse_inline_expressions(
#'   "{.val {i}}"
#' )
#'
#' x <- 5
#' y <- 10
#' parse_inline_expressions(
#'   "{.pkg {x + y}}"
#' )
#'
#' name <- "testing"
#' name <- parse_inline_expressions(
#'   "{.pkg {name}}"
#' )
#' name
#'
#' log_message(name)
parse_inline_expressions <- function(
    text,
    env = parent.frame()) {
  vapply(
    text,
    .replace_expressions,
    character(1),
    env = env,
    USE.NAMES = FALSE
  )
}

.replace_expressions <- function(text, env) {
  pattern <- "\\{([^{}]+)\\}"

  max_iterations <- 10
  iteration <- 0

  while (grepl(pattern, text) && iteration < max_iterations) {
    iteration <- iteration + 1

    matches <- gregexpr(pattern, text, perl = TRUE)[[1]]
    if (matches[1] == -1) break

    match_starts <- as.numeric(matches)
    match_lengths <- attr(matches, "match.length")

    order_idx <- order(match_starts, decreasing = TRUE)

    for (i in order_idx) {
      start_pos <- match_starts[i]
      match_length <- match_lengths[i]

      match <- substr(text, start_pos, start_pos + match_length - 1)
      replacement <- .process_match(match, env)

      text <- paste0(
        substr(text, 1, start_pos - 1),
        replacement,
        substr(text, start_pos + match_length, nchar(text))
      )
    }
  }

  text
}

.process_match <- function(match, env) {
  inner_content <- substr(match, 2, nchar(match) - 1)

  if (grepl("^\\.[a-zA-Z_]+\\s+", inner_content)) {
    .process_format_expression(inner_content, env)
  } else {
    .evaluate_expression(inner_content, env)
  }
}

.process_format_expression <- function(content, env) {
  parts <- strsplit(content, "\\s+", 2)[[1]]

  if (length(parts) != 2) {
    return(paste0("{", content, "}"))
  }

  format_tag <- parts[1]
  format_content <- parts[2]

  if (grepl("\\{", format_content)) {
    return(paste0("{", content, "}"))
  }

  evaluated <- .evaluate_expression(format_content, env)
  paste0("{", format_tag, " ", evaluated, "}")
}

.evaluate_expression <- function(expr, env) {
  tryCatch(
    {
      result <- eval(parse(text = expr), envir = env)
      as.character(result)
    },
    error = function(e) {
      expr
    }
  )
}
