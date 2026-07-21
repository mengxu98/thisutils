#' @title Parallelize a function
#'
#' @md
#' @inheritParams log_message
#' @param x A vector or list to apply over.
#' @param fun The function to be applied to each element.
#' @param cores The number of worker processes to use for parallelization.
#' Default is `1`.
#' @param export_fun The functions to export the function to workers.
#' @param clean_result Whether to remove failed results from output.
#' If `FALSE`, failed results are kept as error objects.
#' Default is `FALSE`.
#' @param throw_error Whether to print detailed error information for failed results.
#' Default is `TRUE`.
#' @param progress_bar_width Width of the verbose progress bar in characters.
#' Default is `10L`.
#' @param backend Parallel backend. `"auto"` uses PSOCK on Windows, while
#' collecting coverage, and when a non-sequential future plan is active;
#' otherwise it uses fork. `"fork"` is unavailable on Windows.
#' @param timeout Maximum number of seconds that a parallel worker task may run.
#' `Inf` disables task timeouts. This is ignored when execution uses one core.
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
  verbose = TRUE,
  backend = c("auto", "fork", "psock"),
  timeout = Inf
) {
  fun <- match.fun(fun)
  backend <- match.arg(backend)
  timeout <- suppressWarnings(as.numeric(timeout)[1L])
  if (!length(timeout) || is.na(timeout) || timeout <= 0) {
    stop("timeout must be a positive number or Inf.", call. = FALSE)
  }
  total <- length(x)
  if (identical(Sys.getenv("thisutils_workers"), "true")) {
    cores <- 1L
  }
  cores <- cores_detect(cores, total)
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
        X = seq_along(x),
        FUN = function(i) {
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
      )
    }
  }

  if (cores > 1) {
    log_message(
      "Using {.pkg {cores}} cores",
      timestamp_format = timestamp_format,
      verbose = verbose
    )

    output_list <- parallel_collect_results(
      x = x,
      fun = fun,
      total = total,
      cores = cores,
      backend = backend,
      timeout = timeout,
      export_fun = export_fun,
      safe_call = safe_call,
      progress_id = if (verbose) pb else NULL,
      progress_env = if (verbose) progress_env else NULL,
      has_names = has_names,
      show_values = show_values
    )
    if (verbose) {
      cli::cli_progress_done(id = pb)
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
  backend,
  timeout,
  export_fun,
  safe_call,
  progress_id,
  progress_env,
  has_names,
  show_values
) {
  parallel_collect_results_cluster(
    x = x,
    fun = fun,
    total = total,
    cores = cores,
    backend = backend,
    timeout = timeout,
    export_fun = export_fun,
    safe_call = safe_call,
    progress_id = progress_id,
    progress_env = progress_env,
    has_names = has_names,
    show_values = show_values
  )
}

parallel_collect_results_cluster <- function(
  x,
  fun,
  total,
  cores,
  backend,
  timeout,
  export_fun,
  safe_call,
  progress_id,
  progress_env,
  has_names,
  show_values
) {
  backend <- parallel_backend(backend)
  context_id <- NULL
  if (backend == "fork") {
    context_id <- paste0(
      "context_",
      length(ls(envir = .parallel_fork_contexts, all.names = TRUE)) + 1L
    )
    context <- new.env(parent = emptyenv())
    context$x <- x
    context$fun <- fun
    context$safe_call <- safe_call
    .parallel_fork_contexts[[context_id]] <- context
    on.exit({
      if (exists(context_id, envir = .parallel_fork_contexts, inherits = FALSE)) {
        rm(list = context_id, envir = .parallel_fork_contexts)
      }
    }, add = TRUE)
  }

  cl <- make_parallel_cluster(cores, backend)
  worker_pids <- integer()
  force_cleanup <- TRUE
  on.exit(
    terminate_parallel_cluster(cl, worker_pids, force = force_cleanup),
    add = TRUE
  )
  worker_pids <- unlist(parallel::clusterCall(cl, Sys.getpid), use.names = FALSE)

  if (backend == "fork") {
    dispatch_task <- parallel_fork_worker_task
    worker_args <- function(i) list(context_id, i)
  } else {
    worker_task <- function(i) {
      Sys.setenv(thisutils_workers = "true")
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
    worker_args <- function(i) list(i)

    parallel::clusterExport(
      cl = cl,
      varlist = "worker_task",
      envir = environment()
    )
    parallel::clusterExport(
      cl = cl,
      varlist = "parallelize_fun",
      envir = environment(parallelize_fun)
    )
    dispatch_task <- parallel_psock_worker_task

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
  }

  output_list <- vector("list", total)
  task_started <- rep(NA_real_, total)
  next_index <- 1L
  active_workers <- min(length(cl), total)

  sendCall <- get_namespace_fun("parallel", "sendCall")
  for (worker_idx in seq_len(active_workers)) {
    sendCall(
      con = cl[[worker_idx]],
      fun = dispatch_task,
      args = worker_args(next_index),
      tag = next_index
    )
    task_started[[next_index]] <- parallel_elapsed()
    next_index <- next_index + 1L
  }

  completed <- 0L
  while (completed < total) {
    active_tasks <- which(!is.na(task_started))
    deadlines <- stats::setNames(
      task_started[active_tasks] + timeout,
      active_tasks
    )
    result <- receive_parallel_result(
      cl,
      deadlines = deadlines,
      timeout = timeout,
      worker_pids = worker_pids
    )
    task_index <- as.integer(result$tag)
    output_list[task_index] <- list(result$value)
    task_started[[task_index]] <- NA_real_
    completed <- completed + 1L

    if (!is.null(progress_id)) {
      cli::cli_progress_update(
        id = progress_id,
        inc = 1L,
        status = progress_status(x, task_index, has_names, show_values),
        .envir = progress_env
      )
    }

    if (next_index <= total) {
      sendCall(
        con = cl[[result$node]],
        fun = dispatch_task,
        args = worker_args(next_index),
        tag = next_index
      )
      task_started[[next_index]] <- parallel_elapsed()
      next_index <- next_index + 1L
    }
  }

  force_cleanup <- FALSE
  output_list
}

receive_parallel_result <- function(
  cl,
  deadlines = numeric(),
  timeout = Inf,
  worker_pids = integer(),
  poll_interval = 0.1
) {
  connections <- lapply(cl, `[[`, "con")
  repeat {
    now <- parallel_elapsed()
    next_deadline <- if (length(deadlines)) min(deadlines) else Inf
    wait <- min(poll_interval, max(0, next_deadline - now))

    if (any(base::socketSelect(connections, timeout = wait))) {
      return(tryCatch(
        get_namespace_fun("parallel", "recvOneResult")(cl),
        error = function(e) {
          dead_workers <- worker_pids[!vapply(
            worker_pids,
            parallel_process_alive,
            logical(1)
          )]
          stop(parallel_worker_error(dead_workers, conditionMessage(e)))
        }
      ))
    }

    dead_workers <- worker_pids[!vapply(
      worker_pids,
      parallel_process_alive,
      logical(1)
    )]
    if (length(dead_workers)) {
      stop(parallel_worker_error(dead_workers))
    }

    now <- parallel_elapsed()
    timed_out <- names(deadlines)[deadlines <= now]
    if (length(timed_out)) {
      stop(parallel_timeout_error(as.integer(timed_out), timeout))
    }
    Sys.sleep(0.001)
  }
}

parallel_elapsed <- function() {
  unname(proc.time()[["elapsed"]])
}

parallel_process_alive <- function(pid) {
  handle <- tryCatch(
    ps::ps_handle(as.integer(pid)),
    error = function(e) NULL
  )
  !is.null(handle) && isTRUE(tryCatch(
    ps::ps_is_running(handle),
    error = function(e) FALSE
  ))
}

parallel_timeout_error <- function(indices, timeout) {
  structure(
    list(
      message = sprintf(
        "Parallel task%s %s timed out after %s seconds.",
        if (length(indices) == 1L) "" else "s",
        paste(indices, collapse = ", "),
        format(timeout, trim = TRUE)
      ),
      call = NULL,
      indices = indices,
      timeout = timeout
    ),
    class = c("parallelize_timeout", "error", "condition")
  )
}

parallel_worker_error <- function(pids = integer(), cause = NULL) {
  worker_text <- if (length(pids)) {
    sprintf(
      "Parallel worker%s %s exited before returning a result.",
      if (length(pids) == 1L) "" else "s",
      paste(pids, collapse = ", ")
    )
  } else {
    "A parallel worker failed before returning a result."
  }
  if (!is.null(cause) && nzchar(cause)) {
    worker_text <- paste(worker_text, cause)
  }

  structure(
    list(
      message = worker_text,
      call = NULL,
      pids = pids,
      cause = cause
    ),
    class = c("parallelize_worker_error", "error", "condition")
  )
}

terminate_parallel_cluster <- function(cl, worker_pids, force = FALSE) {
  if (isTRUE(force)) {
    parallel_signal_workers(worker_pids, tools::SIGTERM)
    deadline <- parallel_elapsed() + 0.2
    while (
      any(vapply(worker_pids, parallel_process_alive, logical(1))) &&
        parallel_elapsed() < deadline
    ) {
      Sys.sleep(0.01)
    }
    remaining <- worker_pids[vapply(
      worker_pids,
      parallel_process_alive,
      logical(1)
    )]
    final_signal <- if (.Platform$OS.type == "windows") {
      tools::SIGTERM
    } else {
      tools::SIGKILL
    }
    parallel_signal_workers(remaining, final_signal)
  }

  try(parallel::stopCluster(cl), silent = TRUE)
  invisible(NULL)
}

parallel_signal_workers <- function(pids, signal) {
  for (pid in unique(as.integer(pids))) {
    try(tools::pskill(pid, signal), silent = TRUE)
  }
  invisible(NULL)
}

.parallel_fork_contexts <- new.env(parent = emptyenv())

parallel_fork_worker_task <- function(context_id, i) {
  Sys.setenv(thisutils_workers = "true")
  context <- .parallel_fork_contexts[[context_id]]
  tryCatch(
    context$safe_call(context$fun, context$x[[i]]),
    error = function(e) {
      structure(
        list(
          error = e$message,
          index = i,
          input = context$x[[i]]
        ),
        class = "parallelize_error"
      )
    }
  )
}

parallel_psock_worker_task <- function(i) {
  get("worker_task", envir = globalenv(), inherits = FALSE)(i)
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

parallel_backend <- function(backend = c("auto", "fork", "psock")) {
  backend <- match.arg(backend)
  if (backend != "auto") {
    if (backend == "fork" && future_workers_active()) {
      warning(
        "Using fork with an active non-sequential future plan can hang or corrupt future worker connections; use backend = \"psock\" or future::plan(future::sequential).",
        call. = FALSE
      )
    }
    return(backend)
  }

  if (
    .Platform$OS.type == "windows" ||
      identical(Sys.getenv("R_COVR"), "true") ||
      future_workers_active()
  ) {
    return("psock")
  }

  "fork"
}

future_workers_active <- function() {
  if (!("future" %in% loadedNamespaces())) {
    return(FALSE)
  }

  future_ns <- asNamespace("future")
  plan <- tryCatch(
    get("plan", envir = future_ns, inherits = FALSE)(),
    error = function(e) NULL
  )
  workers <- tryCatch(
    get("nbrOfWorkers", envir = future_ns, inherits = FALSE)(),
    error = function(e) 1L
  )

  !is.null(plan) && !inherits(plan, "sequential") && workers > 1L
}

make_parallel_cluster <- function(cores, backend = parallel_backend()) {
  if (backend == "psock") {
    return(parallel::makePSOCKcluster(cores, outfile = nullfile()))
  }

  parallel::makeForkCluster(cores, outfile = nullfile())
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
  requested_cores <- suppressWarnings(as.integer(cores)[1L])
  if (!length(requested_cores) || is.na(requested_cores) || requested_cores < 1L) {
    requested_cores <- 1L
  }

  num_session <- suppressWarnings(as.integer(num_session)[1L])
  if (!length(num_session) || is.na(num_session) || num_session < 1L) {
    num_session <- 1L
  }

  min(max_cores, requested_cores, num_session)
}
