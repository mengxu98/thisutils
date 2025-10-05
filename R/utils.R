#' @title Value selection operator
#'
#' @description
#' This operator returns the left side if it's not `NULL`,
#' otherwise it returns the right side.
#'
#' @md
#' @param a The left side value to check.
#' @param b The right side value to use if `a` is `NULL`.
#'
#' @return `a` if it is not `NULL`, otherwise `b`.
#'
#' @export
#'
#' @examples
#' NULL %ss% 10
#' 5 %ss% 10
`%ss%` <- function(a, b) {
  if (is.null(a)) {
    b
  } else {
    a
  }
}

#' @title Invoke a function with a list of arguments
#'
#' @md
#' @param .fn A function, or function name as a string.
#' @param .args A list of arguments.
#' @param ... Other arguments passed to the function.
#' @param .env Environment in which to evaluate the call.
#' This will be most useful if `.fn` is a string,
#' or the function has side-effects.
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

#' @title Capitalize the first letter of each word
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
#' @md
#' @param x A vector of character strings.
#' @param trim_start Whether to remove leading spaces before the first word.
#' Default is `TRUE`.
#' @param trim_end Whether to remove trailing spaces after the last word.
#' Default is `FALSE`.
#' @param collapse_multiple Whether to collapse multiple consecutive spaces between words into a single space.
#' Default is `TRUE`.
#' @param preserve_newlines Whether to preserve newline characters when collapsing spaces.
#' Default is `TRUE`.
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
      "{.arg x} must be the type of character",
      message_type = "error"
    )
  }

  if (!is.logical(trim_start) || length(trim_start) != 1) {
    log_message(
      "{.arg trim_start} must be a single logical value",
      message_type = "error"
    )
  }
  if (!is.logical(trim_end) || length(trim_end) != 1) {
    log_message(
      "{.arg trim_end} must be a single logical value",
      message_type = "error"
    )
  }
  if (!is.logical(collapse_multiple) || length(collapse_multiple) != 1) {
    log_message(
      "{.arg collapse_multiple} must be a single logical value",
      message_type = "error"
    )
  }
  if (!is.logical(preserve_newlines) || length(preserve_newlines) != 1) {
    log_message(
      "{.arg preserve_newlines} must be a single logical value",
      message_type = "error"
    )
  }

  if (trim_start) {
    x <- gsub("^\\s+", "", x)
  }
  if (trim_end) {
    x <- gsub("\\s+$", "", x)
  }

  if (collapse_multiple) {
    if (preserve_newlines) {
      x <- gsub("[ \t\r\f\v]+", " ", x)
      x <- gsub("\\s*\n\\s*", "\n", x)
    } else {
      x <- gsub("\\s+", " ", x)
    }
  }

  return(x)
}

#' @title Try to evaluate an expression a set number of times before failing
#'
#' @description
#' The function is used as a fail-safe if code sometimes works and sometimes doesn't,
#' usually because it depends on a resource that may be temporarily unavailable.
#' It tries to evaluate the expression `max_tries` times.
#' If all the attempts fail, it throws an error;
#' if not, the evaluated expression is returned.
#'
#' @md
#' @param expr The expression to be evaluated.
#' @param max_tries The maximum number of attempts to evaluate the expression before giving up.
#' Default is `5`.
#' @param error_message Additional custom error message to be displayed when an error occurs.
#' @param retry_message Message displayed when a new try to evaluate the expression would be attempted.
#'
#' @return
#' The evaluated expression if successful,
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
        log_message(error, message_type = "warning")
        log_message(error_message, message_type = "warning")
        Sys.sleep(1)
        error
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
        log_message(retry_message, message_type = "warning")
      }
    }
  }
  return(out)
}

#' @title Download file from the Internet
#'
#' @md
#' @inheritParams utils::download.file
#' @param methods Methods to be used for downloading files.
#' Can be `"auto"`, `"wget"`, `"libcurl"`, `"curl"`, `"wininet"`, `"internal"`.
#' Default is `"auto"`, which means to try different download methods.
#' @param max_tries Number of tries for each download method.
#' Default is `2`.
#' @param ... Other arguments passed to [utils::download.file].
#'
#' @export
download <- function(
    url,
    destfile,
    methods = c(
      "auto", "wget", "libcurl", "curl", "wininet", "internal"
    ),
    quiet = FALSE,
    ...,
    max_tries = 2) {
  if (missing(url) || missing(destfile)) {
    log_message(
      "{.arg url} and {.arg destfile} must be both provided",
      message_type = "error"
    )
  }
  ntry <- 0
  status <- NULL
  while (is.null(status)) {
    for (method in methods) {
      status <- tryCatch(
        expr = {
          suppressWarnings(
            utils::download.file(
              url = url,
              destfile = destfile,
              method = method,
              quiet = quiet,
              ...
            )
          )
          status <- 1
        }, error = function(error) {
          log_message(error)
          log_message(
            "Failed to download using {.pkg {method}}, from {.url {url}}",
            message_type = "warning"
          )
          Sys.sleep(1)
          NULL
        }
      )
      if (!is.null(status)) {
        break
      }
    }
    ntry <- ntry + 1
    if (is.null(status) && ntry >= max_tries) {
      log_message(
        "Download failed after {.val {max_tries}} tries from {.url {url}}",
        message_type = "error"
      )
    }
  }
  return(invisible(NULL))
}

#' @title Wilkinson's P-value
#'
#' @param p A vector of P-values.
#' @param r The number of studies to include in the P-value calculation.
#' @param alpha The significance level.
#' @param log.p Whether to return the log of the P-value.
#' @export
#'
#' @examples
#' p <- c(0.01, 0.02, 0.03, 0.04, 0.05)
#' wilkinsonp(p)
#' wilkinsonp(p, r = 2)
#' wilkinsonp(p, alpha = 0.01)
#' wilkinsonp(p, log.p = TRUE)
wilkinsonp <- function(p, r = 1, alpha = 0.05, log.p = FALSE) {
  alpha <- ifelse(alpha > 1, alpha / 100, alpha)
  stopifnot(alpha > 0, alpha < 1)
  alpha <- ifelse(alpha > 0.5, 1 - alpha, alpha)
  keep <- (p >= 0) & (p <= 1)
  invalid <- sum(1L * keep) < 2
  if (invalid) {
    log_message(
      "Must have at least two valid P-values",
      message_type = "warning"
    )
    res <- list(
      p = NA_real_,
      pr = NA_real_,
      r = r,
      critp = NA_real_,
      alpha = alpha,
      validp = p[keep]
    )
  } else {
    pi <- p[keep]
    k <- length(pi)
    if (k != length(p)) {
      log_message(
        "Some studies omitted",
        message_type = "warning"
      )
    }
    if ((r < 1) || (r > k)) {
      r <- 1
      log_message(
        "Illegal r set to 1",
        message_type = "warning"
      )
    }
    pi <- sort(pi)
    pr <- pi[r]
    res <- list(
      p = stats::pbeta(pr, r, k + 1 - r, log.p = log.p),
      pr = pr,
      r = r,
      critp = stats::qbeta(alpha, r, k + 1 - r),
      alpha = alpha,
      validp = pi
    )
  }
  res
}

#' @title Maximum P-value
#'
#' @inheritParams wilkinsonp
#' @export
#'
#' @examples
#' p <- c(0.01, 0.02, 0.03, 0.04, 0.05)
#' maximump(p)
#' maximump(p, alpha = 0.01)
#' maximump(p, log.p = TRUE)
maximump <- function(p, alpha = 0.05, log.p = FALSE) {
  keep <- (p >= 0) & (p <= 1)
  validp <- p[keep]
  k <- length(validp)
  res <- wilkinsonp(p, r = k, alpha, log.p)
  res
}

#' @title Minimum P-value
#'
#' @inheritParams wilkinsonp
#' @export
#'
#' @examples
#' p <- c(0.01, 0.02, 0.03, 0.04, 0.05)
#' minimump(p)
#' minimump(p, alpha = 0.01)
#' minimump(p, log.p = TRUE)
minimump <- function(p, alpha = 0.05, log.p = FALSE) {
  res <- wilkinsonp(p, r = 1, alpha, log.p)
  res
}

#' @title Mean P-value
#'
#' @inheritParams wilkinsonp
#' @export
#'
#' @examples
#' p <- c(0.01, 0.02, 0.03, 0.04, 0.05)
#' meanp(p)
meanp <- function(p) {
  keep <- (p >= 0) & (p <= 1)
  invalid <- sum(1L * keep) < 4
  if (invalid) {
    log_message(
      "Must have at least four valid P-values",
      message_type = "warning"
    )
    res <- list(
      z = NA_real_,
      p = NA_real_,
      validp = p[keep]
    )
  } else {
    pi <- mean(p[keep])
    k <- length(p[keep])
    z <- (0.5 - pi) * sqrt(12 * k)
    if (k != length(p)) {
      log_message(
        "Some studies omitted",
        message_type = "warning"
      )
    }
    res <- list(
      z = z,
      p = stats::pnorm(z, lower.tail = FALSE),
      validp = p[keep]
    )
  }
  res
}

#' @title Sum P-value
#'
#' @inheritParams wilkinsonp
#' @export
#'
#' @examples
#' p <- c(0.01, 0.02, 0.03, 0.04, 0.05)
#' sump(p)
sump <- function(p) {
  keep <- (p >= 0) & (p <= 1)
  invalid <- sum(1L * keep) < 2
  if (invalid) {
    log_message(
      "Must have at least two valid P-values",
      message_type = "warning"
    )
    res <- list(
      p = NA_real_,
      conservativep = NA_real_,
      validp = p[keep]
    )
  } else {
    sigmap <- sum(p[keep])
    k <- length(p[keep])
    conservativep <- exp(k * log(sigmap) - lgamma(k + 1))
    nterm <- floor(sigmap) + 1
    denom <- lfactorial(k)
    psum <- 0
    terms <- vector("numeric", nterm)
    for (i in 1:nterm) {
      terms[i] <- lchoose(k, i - 1) +
        k *
          log(
            sigmap -
              i +
              1
          ) -
        denom
      pm <- 2 * (i %% 2) - 1
      psum <- psum + pm * exp(terms[i])
    }
    if (k != length(p)) {
      log_message(
        "Some studies omitted",
        message_type = "warning"
      )
    }
    if (sigmap > 20) {
      log_message(
        "Likely to be unreliable, check with another method",
        message_type = "warning"
      )
    }
    res <- list(
      p = psum,
      conservativep = conservativep,
      validp = p[keep]
    )
  }
  res
}

#' @title Vote P-value
#'
#' @inheritParams wilkinsonp
#' @export
#'
#' @examples
#' p <- c(0.01, 0.02, 0.03, 0.04, 0.05)
#' votep(p)
#' votep(p, alpha = 0.01)
votep <- function(p, alpha = 0.5) {
  alpha <- ifelse(alpha > 1, alpha / 100, alpha)
  stopifnot(alpha > 0, alpha < 1)
  keep <- (p >= 0) & (p <= 1)
  alp <- vector("numeric", 2)
  if (alpha <= 0.5) {
    alp[1] <- alpha
    alp[2] <- 1 - alpha
  } else {
    alp[2] <- alpha
    alp[1] <- 1 - alpha
  }
  invalid <- sum(1L * keep) < 2
  if (invalid) {
    log_message(
      "Must have at least two valid P-values",
      message_type = "warning"
    )
    res <- list(
      p = NA_real_,
      pos = NA_integer_,
      neg = NA_integer_,
      alpha = alpha,
      validp = p[keep]
    )
  } else {
    pi <- p[keep]
    k <- length(pi)
    pos <- sum(1L * (pi < alp[1]))
    neg <- sum(1L * (pi > alp[2]))
    if (k != length(p)) {
      log_message(
        "Some studies omitted",
        message_type = "warning"
      )
    }
    if ((pos + neg) <= 0) {
      log_message(
        "All P-values are within specified limits of alpha",
        message_type = "warning"
      )
      p <- 1
    } else {
      p <- stats::binom.test(
        pos, pos + neg, 0.5,
        alternative = "greater"
      )$p.value
    }
    res <- list(
      p = p,
      pos = pos,
      neg = neg,
      alpha = alpha,
      validp = pi
    )
  }
  res
}

#' @title Maximum depth of a list
#'
#' @param x A list.
#' @param depth The depth of the list.
#' @export
#'
#' @examples
#' x <- list(
#'   a = list(b = list(c = 1)),
#'   d = list(e = list(f = 2))
#' )
#' max_depth(x)
max_depth <- function(x, depth = 0) {
  if (is.list(x)) {
    return(max(unlist(lapply(x, max_depth, depth + 1))))
  } else {
    return(depth)
  }
}
