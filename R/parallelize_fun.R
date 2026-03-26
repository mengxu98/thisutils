#' @title Parallelize a function
#'
#' @md
#' @inheritParams log_message
#' @param x A vector or list to apply over.
#' @param fun The function to be applied to each element.
#' @param cores The number of cores to use for parallelization with [foreach::foreach].
#' Default is `1`.
#' @param export_fun The functions to export the function to workers.
#' @param clean_result Whether to remove failed results from output.
#' If `FALSE`, failed results are kept as error objects.
#' Default is `FALSE`.
#' @param throw_error Whether to print detailed error information for failed results.
#' Default is `TRUE`.
#' @param progress_bar_width Width of the verbose progress bar in characters.
#' Default is `10L`.
#'
#' @return
#' A list of computed results.
#' If `clean_result = FALSE`, failed results are included as error objects.
#' If `clean_result = TRUE`, only successful results are returned.
#'
#' @export
#'
#' @examples
#' parallelize_fun(1:3, function(x) {
#'   Sys.sleep(0.2)
#'   x^2
#' })
#'
#' parallelize_fun(list(1, 2, 3), function(x) {
#'   Sys.sleep(0.2)
#'   x^2
#' }, cores = 2)
#'
#' # Examples with error handling
#' parallelize_fun(1:5, function(x) {
#'   if (x == 3) stop("Error on element 3")
#'   x^2
#' }, clean_result = FALSE)
#'
#' parallelize_fun(1:5, function(x) {
#'   if (x == 3) stop("Error on element 3")
#'   x^2
#' }, clean_result = TRUE)
#'
#' # Control error printing
#' parallelize_fun(1:5, function(x) {
#'   if (x == 2) stop("Error on element 3")
#'   if (x == 4) stop("Error on element 4")
#'   x^2
#' })
#'
#' parallelize_fun(1:5, function(x) {
#'   if (x == 3) stop("Error on element 3")
#'   x^2
#' }, throw_error = FALSE)
parallelize_fun <- function(
  x,
  fun,
  cores = 1,
  export_fun = NULL,
  clean_result = FALSE,
  throw_error = TRUE,
  progress_bar_width = 10L,
  timestamp_format = paste0(
    "[",
    format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
    "] "
  ),
  verbose = TRUE
) {
  total <- length(x)
  has_names <- !is.null(names(x)) && any(names(x) != "")
  is_vector <- is.vector(x) && !is.list(x)
  show_values <- !has_names && is_vector

  if (verbose) {
    old_cli_opts <- options(
      cli.progress_show_after = 0,
      cli.progress_clear = FALSE
    )
    on.exit(options(old_cli_opts), add = TRUE)

    pb <- cli::cli_progress_bar(
      format = paste0(
        "{cli::make_ansi_style('orange')(cli::pb_spin)} {timestamp_format}",
        "Running for {.pkg {cli::pb_status}}[{.pkg {cli::pb_current}}/{.pkg {cli::pb_total}}] ",
        "{(parallel_progress_bar(cli::pb_current, cli::pb_total, progress_bar_width))} ",
        "{cli::pb_percent} | ETA: {.pkg {cli::pb_eta}}"
      ),
      format_done = paste0(
        "{cli::col_green(cli::symbol$tick)} {timestamp_format}",
        "Completed {.pkg {cli::pb_total}} tasks ",
        "in {.pkg {cli::pb_elapsed}}"
      ),
      total = total,
      clear = FALSE
    )
  }

  safe_call <- function(fun, ...) {
    msg_con <- file(nullfile(), open = "w")
    sink(msg_con, type = "message")
    on.exit({
      sink(type = "message")
      close(msg_con)
    })
    suppressWarnings(fun(...))
  }

  if (cores == 1) {
    log_message(
      "Using {.pkg 1} core",
      timestamp_format = timestamp_format,
      verbose = verbose
    )

    if (verbose) {
      output_list <- vector("list", total)

      for (i in seq_along(x)) {
        output_list[i] <- list(tryCatch(
          safe_call(fun, x[[i]]),
          error = function(e) {
            structure(
              list(
                error = e$message,
                index = i,
                input = x[[i]]
              ),
              class = "parallelize_error"
            )
          }
        ))

        if (has_names) {
          cli::cli_progress_update(id = pb, status = names(x)[i])
        } else if (show_values) {
          cli::cli_progress_update(id = pb, status = as.character(x[[i]]))
        } else {
          cli::cli_progress_update(id = pb)
        }
      }

      cli::cli_progress_done(id = pb)
    } else {
      output_list <- base::lapply(
        X = x,
        FUN = function(xi) {
          tryCatch(
            safe_call(fun, xi),
            error = function(e) {
              structure(
                list(
                  error = e$message,
                  input = xi
                ),
                class = "parallelize_error"
              )
            }
          )
        }
      )
    }
  }

  if (cores > 1) {
    cores <- cores_detect(cores, total)
    doParallel::registerDoParallel(cores = cores)

    log_message(
      "Using {.pkg {foreach::getDoParWorkers()}} cores",
      timestamp_format = timestamp_format,
      verbose = verbose
    )

    if (verbose) {
      chunks <- split(
        seq_along(x),
        rep(1:cores, length.out = total)
      )

      output_list <- vector("list", total)
      for (chunk_idx in seq_along(chunks)) {
        chunk <- chunks[[chunk_idx]]

        i <- NULL
        "%dopar%" <- foreach::"%dopar%"
        chunk_results <- foreach::foreach(
          i = chunk,
          .combine = "c",
          .export = export_fun
        ) %dopar%
          {
            list(
              tryCatch(
                safe_call(fun, x[[i]]),
                error = function(e) {
                  structure(
                    list(
                      error = e$message,
                      index = i,
                      input = x[[i]]
                    ),
                    class = "parallelize_error"
                  )
                }
              )
            )
          }

        output_list[chunk] <- chunk_results

        if (has_names) {
          chunk_names <- names(x)[chunk]
          status_text <- if (length(chunk_names) > 0) {
            paste0(chunk_names[1], "...")
          } else {
            ""
          }
          cli::cli_progress_update(
            id = pb,
            inc = length(chunk),
            status = status_text
          )
        } else if (show_values) {
          chunk_values <- as.character(x[chunk])
          status_text <- if (length(chunk_values) > 0) {
            paste0(chunk_values[1], "...")
          } else {
            ""
          }
          cli::cli_progress_update(
            id = pb,
            inc = length(chunk),
            status = status_text
          )
        } else {
          cli::cli_progress_update(id = pb, inc = length(chunk))
        }
      }

      cli::cli_progress_done(id = pb)
    } else {
      i <- NULL
      "%dopar%" <- foreach::"%dopar%"
      output_list <- foreach::foreach(
        i = seq_along(x),
        .export = export_fun
      ) %dopar%
        {
          tryCatch(
            safe_call(fun, x[[i]]),
            error = function(e) {
              structure(
                list(
                  error = e$message,
                  index = i,
                  input = x[[i]]
                ),
                class = "parallelize_error"
              )
            }
          )
        }
    }

    doParallel::stopImplicitCluster()
  }

  log_message(
    "Building results",
    timestamp_format = timestamp_format,
    verbose = verbose
  )

  error_indices <- vapply(
    output_list,
    function(x) inherits(x, "parallelize_error"),
    logical(1)
  )
  if (any(error_indices)) {
    log_message(
      "Found {.pkg {sum(error_indices)}} failed result{?s}",
      timestamp_format = timestamp_format,
      message_type = "warning",
      verbose = verbose
    )

    if (throw_error && verbose) {
      error_objects <- output_list[error_indices]
      error_inputs <- x[error_indices]

      error_msgs <- vapply(
        error_objects,
        function(e) e$error,
        character(1)
      )
      error_groups <- split(
        seq_along(error_msgs),
        error_msgs
      )

      group_lines <- vapply(
        names(error_groups),
        function(msg) {
          idx <- error_groups[[msg]]
          inputs <- error_inputs[idx]
          n <- length(idx)
          max_show <- 3
          shown <- vapply(
            utils::head(inputs, max_show),
            function(v) {
              parse_inline_expressions("{.val {v}}")
            },
            character(1)
          )
          task_str <- paste(shown, collapse = ", ")
          if (n > max_show) {
            task_str <- paste0(
              task_str,
              sprintf(" and %d more", n - max_show)
            )
          }
          parse_inline_expressions(
            paste0("{.emph ", msg, "} (", n, "): ", task_str)
          )
        },
        character(1)
      )

      error_message <- paste0(
        "Error details:\n",
        paste(group_lines, collapse = "\n")
      )
      log_message(
        error_message,
        symbol = cli::symbol$cross,
        text_color = "red",
        verbose = verbose
      )
    }

    if (clean_result) {
      output_list <- output_list[!error_indices]
      x <- x[!error_indices]
      log_message(
        "Removed {.pkg {sum(error_indices)}} failed result{?s}",
        timestamp_format = timestamp_format,
        verbose = verbose
      )
    }
  }

  if (has_names) {
    names(output_list) <- names(x)
  } else if (is_vector) {
    names(output_list) <- as.character(x)
  }

  return(output_list)
}

parallel_progress_bar <- function(
  current,
  total,
  width = 10L
) {
  width <- suppressWarnings(as.integer(width[[1]]))
  if (is.na(width) || width < 1L) {
    width <- 10L
  }

  current <- suppressWarnings(as.numeric(current))
  total <- suppressWarnings(as.numeric(total))

  if (!is.finite(total) || total <= 0) {
    return("")
  }

  if (!is.finite(current)) {
    current <- 0
  }

  current <- max(0, min(current, total))
  filled <- floor(width * current / total)
  if (current >= total) {
    filled <- width
  }

  empty <- max(width - filled, 0L)

  paste0(
    cli::col_green(strrep("\u25A0", filled)),
    strrep(" ", empty)
  )
}

cores_detect <- function(
  cores = 1,
  num_session = NULL
) {
  if (is.null(num_session)) {
    return(1)
  }
  detected_cores <- suppressWarnings(
    parallel::detectCores(logical = FALSE)
  )
  if (!is.finite(detected_cores) || detected_cores < 2) {
    detected_cores <- 2L
  }

  max_cores <- max(1L, as.integer(detected_cores) - 1L)
  requested_cores <- suppressWarnings(as.integer(cores[[1]]))
  if (is.na(requested_cores) || requested_cores < 1L) {
    requested_cores <- 1L
  }

  num_session <- suppressWarnings(as.integer(num_session[[1]]))
  if (is.na(num_session) || num_session < 1L) {
    num_session <- 1L
  }

  min(max_cores, requested_cores, num_session)
}
