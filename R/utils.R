#' @title Value selection operator
#'
#' @description
#' This operator returns the left side if it's not NULL,
#' otherwise it returns the right side.
#'
#' @param a The left side value to check
#' @param b The right side value to use if a is NULL
#'
#' @return a if it is not NULL, otherwise b
#' @export
#'
#' @examples
#' NULL %ss% 10
#' 5 %ss% 10
`%ss%` <- function(a, b) {
  if (is.null(a)) {
    return(b)
  } else {
    return(a)
  }
}

#' Invoke a function with a list of arguments
#' @param .fn A function, or function name as a string.
#' @param .args A list of arguments.
#' @param ... Other arguments passed to the function.
#' @param .env Environment in which to evaluate the call.
#' This will be most useful if .fn is a string, or the function has side-effects.
#'
#' @export
#'
#' @examples
#' f <- function(x, y) {
#'   x + y
#' }
#' invoke_fun(f, list(x = 1, y = 2))
#' invoke_fun("f", list(x = 1, y = 2))
#' invoke_fun("f", x = 1, y = 2)
invoke_fun <- function(
    .fn,
    .args = list(),
    ...,
    .env = rlang::caller_env()) {
  args <- c(.args, list(...))
  .bury <- c(".fn", "")
  if (rlang::is_null(.bury) || !length(args)) {
    if (rlang::is_scalar_character(.fn)) {
      .fn <- rlang::env_get(.env, .fn, inherit = TRUE)
    }
    call <- rlang::call2(.fn, !!!args)
    return(rlang::eval_bare(call, .env))
  }
  if (!rlang::is_character(.bury, 2L)) {
    log_message(
      "{.arg .bury} must be a {.field character vector} of length 2",
      message_type = "error"
    )
  }
  arg_prefix <- .bury[[2]]
  fn_nm <- .bury[[1]]
  buried_nms <- paste0(arg_prefix, seq_along(args))
  buried_args <- rlang::set_names(args, buried_nms)
  .env <- rlang::env(.env, !!!buried_args)
  args <- rlang::set_names(buried_nms, names(args))
  args <- rlang::syms(args)
  if (rlang::is_function(.fn)) {
    `:=` <- rlang::`:=`
    rlang::env_bind(.env, `:=`(!!fn_nm, .fn))
    .fn <- fn_nm
  }
  call <- rlang::call2(.fn, !!!args)
  rlang::eval_bare(call, .env)
}

#' Capitalizes the characters
#' Making the first letter uppercase
#'
#' @param x A vector of character strings to be capitalized.
#' @param force_tolower Whether to force the remaining letters to be lowercase.
#' @export
#'
#' @examples
#' x <- c(
#'   "hello world",
#'   "Hello world",
#'   "hello World"
#' )
#' capitalize(x)
capitalize <- function(x, force_tolower = FALSE) {
  if (is.null(x)) {
    return(NULL)
  }
  if (inherits(x, "factor")) {
    x <- as.character(x)
  }
  if (!inherits(x, "character")) {
    log_message(
      "x must be the type of character.",
      message_type = "error"
    )
  }
  if (isTRUE(force_tolower)) {
    x <- paste(
      toupper(substr(x, 1, 1)),
      tolower(substr(x, 2, nchar(x))),
      sep = ""
    )
  } else {
    first_word <- sapply(strsplit(x, "\\s|-"), function(s) s[1])
    index <- which(first_word == tolower(first_word))
    x[index] <- paste(
      toupper(substr(x[index], 1, 1)),
      substr(x[index], 2, nchar(x[index])),
      sep = ""
    )
  }
  return(x)
}

#' @title Wrap text
#'
#' @param x A character vector to wrap.
#' @param width The maximum width of the lines.
#'
#' @return A character vector with wrapped text.
#' @export
#' @examples
#' str_wrap(rep("Hello, world!", 10))
#' str_wrap(rep("Hello, world!", 10), width = 10)
str_wrap <- function(x, width = 80) {
  if (is.null(x)) {
    return(NULL)
  }
  if (inherits(x, "factor")) {
    x <- as.character(x)
  }
  x_wrap <- unlist(
    lapply(
      x,
      function(i) {
        paste0(strwrap(i, width = width), collapse = "\n")
      }
    )
  )
  return(x_wrap)
}

#' @title Unnest a list-column
#'
#' @description
#' Implement similar functions to the [tidyr::unnest] function.
#'
#' @md
#' @param data A data frame.
#' @param cols Columns to unnest.
#' @param keep_empty By default, you get one row of output for each element of the list your unchopping/unnesting.
#' This means that if there's a size-0 element (like `NULL` or an empty data frame),
#' that entire row will be dropped from the output.
#' If you want to preserve all rows,
#' use `keep_empty = TRUE` to replace size-0 elements with a single row of missing values.
#' @export
#' @examples
#' data <- data.frame(
#'   id = 1:3,
#'   x = c("a", "b", "c"),
#'   stringsAsFactors = FALSE
#' )
#' data$data <- list(
#'   c(1, 2),
#'   c(3, 4, 5),
#'   c(6)
#' )
#' unnest_fun(data, cols = "data")
#'
#' data2 <- data.frame(
#'   id = 1:3,
#'   x = c("a", "b", "c"),
#'   stringsAsFactors = FALSE
#' )
#' data2$data <- list(
#'   c(1, 2),
#'   numeric(0),
#'   c(6)
#' )
#' unnest_fun(data2, cols = "data")
#' unnest_fun(data2, cols = "data", keep_empty = TRUE)
unnest_fun <- function(
    data,
    cols,
    keep_empty = FALSE) {
  if (nrow(data) == 0 || length(cols) == 0) {
    return(data)
  }
  for (col in cols) {
    col_expand <- unlist(data[[col]])
    expand_times <- sapply(data[[col]], length)
    if (isTRUE(keep_empty)) {
      data[[col]][expand_times == 0] <- NA
      col_expand <- unlist(data[[col]])
      expand_times[expand_times == 0] <- 1
    }
    data <- data[rep(seq_len(nrow(data)), times = expand_times), ]
    data[, col] <- col_expand
  }
  rownames(data) <- NULL
  return(data)
}

#' @title Remove and normalize spaces
#'
#' @description
#' Remove leading/trailing spaces and normalize multiple spaces between words in character strings.
#'
#' @param x A vector of character strings.
#' @param trim_start Logical value, default is `TRUE`.
#' Whether to remove leading spaces before the first word.
#' @param trim_end Logical value, default is `FALSE`.
#' Whether to remove trailing spaces after the last word.
#' @param collapse_multiple Logical value, default is `TRUE`.
#' Whether to collapse multiple consecutive spaces between words into a single space.
#' @param preserve_newlines Logical value, default is `TRUE`.
#' Whether to preserve newline characters when collapsing spaces.
#'
#' @return A character vector with spaces normalized according to the specified parameters.
#' @export
#'
#' @examples
#' x <- c(
#'   " hello  world ",
#'   "  test   case  ",
#'   "no space",
#'   "   multiple   spaces   "
#' )
#' remove_space(x)
#' remove_space(x, trim_start = FALSE)
#' remove_space(x, trim_end = TRUE)
#' remove_space(x, collapse_multiple = FALSE)
#' remove_space(
#'   x,
#'   trim_start = FALSE,
#'   trim_end = FALSE,
#'   collapse_multiple = FALSE
#' )
#'
#' # with newlines
#' multiline <- c(
#'   "hello\n\n  world  ",
#'   "  first  \n  second  "
#' )
#' remove_space(multiline)
#' remove_space(multiline, preserve_newlines = FALSE)
remove_space <- function(
    x,
    trim_start = TRUE,
    trim_end = FALSE,
    collapse_multiple = TRUE,
    preserve_newlines = TRUE) {
  if (is.null(x)) {
    return(NULL)
  }
  if (inherits(x, "factor")) {
    x <- as.character(x)
  }
  if (!inherits(x, "character")) {
    log_message(
      "x must be the type of character.",
      message_type = "error"
    )
  }

  if (!is.logical(trim_start) || length(trim_start) != 1) {
    log_message(
      "trim_start must be a single logical value.",
      message_type = "error"
    )
  }
  if (!is.logical(trim_end) || length(trim_end) != 1) {
    log_message(
      "trim_end must be a single logical value.",
      message_type = "error"
    )
  }
  if (!is.logical(collapse_multiple) || length(collapse_multiple) != 1) {
    log_message(
      "collapse_multiple must be a single logical value.",
      message_type = "error"
    )
  }
  if (!is.logical(preserve_newlines) || length(preserve_newlines) != 1) {
    log_message(
      "preserve_newlines must be a single logical value.",
      message_type = "error"
    )
  }

  result <- x

  if (trim_start) {
    result <- gsub("^\\s+", "", result)
  }

  if (trim_end) {
    result <- gsub("\\s+$", "", result)
  }

  if (collapse_multiple) {
    if (preserve_newlines) {
      result <- gsub("[ \t\r\f\v]+", " ", result)
      result <- gsub("\\s*\n\\s*", "\n", result)
    } else {
      result <- gsub("\\s+", " ", result)
    }
  }

  return(result)
}

#' Try to evaluate an expression a set number of times before failing
#'
#' The function is used as a fail-safe if your R code sometimes works and sometimes
#' doesn't, usually because it depends on a resource that may be temporarily
#' unavailable. It tries to evaluate the expression `max_tries` times. If all the
#' attempts fail, it throws an error; if not, the evaluated expression is returned.
#'
#' @param expr The expression to be evaluated.
#' @param max_tries The maximum number of attempts to evaluate the expression before giving up. Default is set to 5.
#' @param error_message a string, additional custom error message you would like to be displayed when an error occurs.
#' @param retry_message a string, a message displayed when a new try to evaluate the expression would be attempted.
#'
#' @return This function returns the evaluated expression if successful,
#' otherwise it throws an error if all attempts are unsuccessful.
#' @export
#'
#' @examples
#' f <- function() {
#'   value <- runif(1, min = 0, max = 1)
#'   if (value > 0.5) {
#'     log_message("value is larger than 0.5")
#'     return(value)
#'   } else {
#'     log_message(
#'       "value is smaller than 0.5",
#'       message_type = "error"
#'     )
#'   }
#' }
#' f_evaluated <- try_get(expr = f())
#' print(f_evaluated)
try_get <- function(
    expr,
    max_tries = 5,
    error_message = "",
    retry_message = "Retrying...") {
  out <- simpleError("start")
  ntry <- 0
  while (inherits(out, "error")) {
    ntry <- ntry + 1
    out <- tryCatch(
      expr = eval.parent(substitute(expr)),
      error = function(error) {
        log_message(error)
        log_message("")
        log_message(error_message)
        Sys.sleep(1)
        return(error)
      }
    )
    if (inherits(out, "error") && ntry >= max_tries) {
      log_message(
        out,
        message_type = "error"
      )
    } else {
      if (!inherits(out, "error")) {
        break
      } else {
        log_message(retry_message)
      }
    }
  }
  return(out)
}

#' Download File from the Internet
#'
#' @md
#' @inheritParams utils::download.file
#' @param methods Methods to be used for downloading files.
#' The default is to try different download methods in turn until the download is successfully completed.
#' @param max_tries Number of tries for each download method.
#' @param verbose Logical value, default is `TRUE`.
#' Whether to print progress messages.
#' @param use_httr Logical value, default is `FALSE`.
#' Whether to use [httr2::request] to download the file.
#' @param ... Other arguments passed to [utils::download.file]
#'
#' @export
download <- function(
    url,
    destfile,
    methods = c("auto", "wget", "libcurl", "curl", "wininet", "internal"),
    max_tries = 2,
    use_httr = FALSE,
    verbose = TRUE,
    ...) {
  if (missing(url) || missing(destfile)) {
    log_message(
      "'url' and 'destfile' must be both provided.",
      message_type = "error"
    )
  }
  ntry <- 0
  status <- NULL

  if (isTRUE(use_httr)) {
    tryCatch(
      {
        log_message(
          "Attempting download with {.val httr2}...",
          message_type = "info"
        )

        req <- httr2::request(url) |>
          httr2::req_headers(
            "Accept" = "text/plain,application/json;q=0.9,*/*;q=0.8",
            "Accept-Language" = "zh-CN,zh;q=0.9,en;q=0.8",
            "Cache-Control" = "no-cache",
            "Connection" = "keep-alive",
            "Host" = "guolab.wchscu.cn",
            "Pragma" = "no-cache",
            "Referer" = dirname(url),
            "User-Agent" = "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36"
          ) |>
          httr2::req_retry(max_tries = max_tries) |>
          httr2::req_timeout(seconds = 30)

        resp <- req |> httr2::req_perform(path = destfile)

        if (httr2::resp_status(resp) == 200) {
          if (file.exists(destfile)) {
            content <- readLines(destfile, n = 1, warn = FALSE)
            if (!grepl("^<!doctype|^<html", tolower(content))) {
              log_message(
                "Download completed successfully using {.val httr2}",
                message_type = "success"
              )
              return(invisible(NULL))
            }
          }
          log_message(
            "Downloaded file appears to be HTML instead of expected data",
            message_type = "warning"
          )
        } else {
          log_message(
            paste0("HTTP error: ", httr2::resp_status(resp)),
            message_type = "warning"
          )
        }
      },
      error = function(e) {
        log_message(
          paste0("httr2 request failed: ", conditionMessage(e)),
          message_type = "warning"
        )
      }
    )
  }

  while (is.null(status)) {
    for (method in methods) {
      status <- tryCatch(
        expr = {
          suppressWarnings(
            utils::download.file(
              url = url,
              destfile = destfile,
              method = method,
              quiet = !verbose,
              ...
            )
          )
          status <- 1
        },
        error = function(error) {
          log_message(
            error,
            message_type = "warning"
          )
          log_message(
            "Cannot download from the url: ", url,
            message_type = "warning"
          )
          log_message(
            "Failed to download using {.val method}. Retry...",
            message_type = "warning"
          )
          Sys.sleep(1)
          return(NULL)
        }
      )
      if (!is.null(status)) {
        break
      }
    }
    ntry <- ntry + 1
    if (is.null(status) && ntry >= max_tries) {
      log_message(
        "Download failed.",
        message_type = "error"
      )
    }
  }
  return(invisible(NULL))
}
