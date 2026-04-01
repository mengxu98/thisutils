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
    progress_env <- environment()
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
      clear = FALSE,
      .envir = progress_env
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
    log_message(
      "Using {.pkg {cores}} cores",
      timestamp_format = timestamp_format,
      verbose = verbose
    )

    if (verbose) {
      output_list <- parallel_collect_results(
        x = x,
        fun = fun,
        total = total,
        cores = cores,
        export_fun = export_fun,
        safe_call = safe_call,
        progress_id = pb,
        progress_env = progress_env,
        has_names = has_names,
        show_values = show_values
      )
      cli::cli_progress_done(id = pb)
    } else {
      doParallel::registerDoParallel(cores = cores)
      on.exit(doParallel::stopImplicitCluster(), add = TRUE)

      i <- NULL
      "%dopar%" <- foreach::"%dopar%"
      output_list <- foreach::foreach(
        i = seq_along(x),
        .export = export_fun,
        .options.multicore = parallel_foreach_options(cores),
        .options.snow = list(
          preschedule = FALSE
        )
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

parallel_collect_results <- function(
  x,
  fun,
  total,
  cores,
  export_fun,
  safe_call,
  progress_id,
  progress_env,
  has_names,
  show_values
) {
  if (.Platform$OS.type != "windows") {
    return(parallel_collect_results_mc(
      x = x,
      fun = fun,
      total = total,
      cores = cores,
      safe_call = safe_call,
      progress_id = progress_id,
      progress_env = progress_env,
      has_names = has_names,
      show_values = show_values
    ))
  }

  parallel_collect_results_cluster(
    x = x,
    fun = fun,
    total = total,
    cores = cores,
    export_fun = export_fun,
    safe_call = safe_call,
    progress_id = progress_id,
    progress_env = progress_env,
    has_names = has_names,
    show_values = show_values
  )
}

parallel_collect_results_mc <- function(
  x,
  fun,
  total,
  cores,
  safe_call,
  progress_id,
  progress_env,
  has_names,
  show_values
) {
  parallel_worker_task <- function(i) {
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

  output_list <- vector("list", total)
  jobs <- list()
  job_indices <- list()
  next_index <- 1L
  completed <- 0L

  launch_job <- function(task_index) {
    job <- parallel::mcparallel(
      parallel_worker_task(task_index),
      silent = TRUE
    )
    job_pid <- as.character(job$pid)
    jobs[[job_pid]] <<- job
    job_indices[[job_pid]] <<- task_index
  }

  while (next_index <= min(cores, total)) {
    launch_job(next_index)
    next_index <- next_index + 1L
  }

  while (completed < total) {
    result <- parallel::mccollect(jobs, wait = FALSE)
    if (is.null(result)) {
      Sys.sleep(0.01)
      next
    }

    for (pid in names(result)) {
      task_index <- job_indices[[pid]]
      output_list[task_index] <- list(result[[pid]])
      jobs[[pid]] <- NULL
      job_indices[[pid]] <- NULL
      completed <- completed + 1L

      cli::cli_progress_update(
        id = progress_id,
        inc = 1L,
        status = progress_status(x, task_index, has_names, show_values),
        .envir = progress_env
      )

      if (next_index <= total) {
        launch_job(next_index)
        next_index <- next_index + 1L
      }
    }
  }

  output_list
}

parallel_collect_results_cluster <- function(
  x,
  fun,
  total,
  cores,
  export_fun,
  safe_call,
  progress_id,
  progress_env,
  has_names,
  show_values
) {
  cl <- make_parallel_cluster(cores)
  on.exit(parallel::stopCluster(cl), add = TRUE)

  parallel_worker_task <- function(i) {
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

  parallel::clusterExport(
    cl = cl,
    varlist = "parallel_worker_task",
    envir = environment()
  )

  if (!is.null(export_fun) && length(export_fun) > 0) {
    export_env <- environment(fun)
    if (is.null(export_env)) {
      export_env <- parent.frame()
    }
    parallel::clusterExport(
      cl = cl,
      varlist = export_fun,
      envir = export_env
    )
  }

  output_list <- vector("list", total)
  next_index <- 1L
  active_workers <- min(length(cl), total)

  sendCall <- get_namespace_fun("parallel", "sendCall")
  for (worker_idx in seq_len(active_workers)) {
    sendCall(
      con = cl[[worker_idx]],
      fun = parallel_worker_task,
      args = list(next_index),
      tag = next_index
    )
    next_index <- next_index + 1L
  }

  completed <- 0L
  while (completed < total) {
    result <- get_namespace_fun("parallel", "recvOneResult")(cl)
    task_index <- as.integer(result$tag)
    output_list[task_index] <- list(result$value)
    completed <- completed + 1L

    cli::cli_progress_update(
      id = progress_id,
      inc = 1L,
      status = progress_status(x, task_index, has_names, show_values),
      .envir = progress_env
    )

    if (next_index <= total) {
      sendCall(
        con = cl[[result$node]],
        fun = parallel_worker_task,
        args = list(next_index),
        tag = next_index
      )
      next_index <- next_index + 1L
    }
  }

  output_list
}

parallel_foreach_options <- function(cores) {
  list(
    preschedule = FALSE,
    cores = cores
  )
}

progress_status <- function(x, index, has_names, show_values) {
  if (has_names) {
    return(names(x)[index])
  }

  if (show_values) {
    return(as.character(x[[index]]))
  }

  NULL
}

make_parallel_cluster <- function(cores) {
  if (.Platform$OS.type == "windows") {
    return(parallel::makePSOCKcluster(cores))
  }

  parallel::makeForkCluster(cores)
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
