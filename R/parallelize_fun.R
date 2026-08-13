#' @title Parallelize a function
#'
#' @md
#' @inheritParams log_message
#' @param x A vector or list to apply over.
#' @param fun The function to be applied to each element.
#' @param cores The number of worker processes to use for parallelization.
#' Default is `1`.
#' @param export_fun Character vector naming functions or other objects from
#'   the environment of `fun` that PSOCK workers need. Objects referenced from
#'   a global environment are not discovered automatically; list them here or
#'   include them in each element of `x`.
#' @param clean_result Whether to remove failed results from output.
#' If `FALSE`, failed results are kept as error objects.
#' Default is `FALSE`.
#' @param throw_error Whether to print detailed error information for failed results.
#' Default is `TRUE`.
#' @param progress_bar_width Width of the verbose progress bar in characters.
#' Default is `10L`.
#' @param backend Parallel backend. `"auto"` uses PSOCK on every platform.
#' Use `"fork"` to opt in to forked workers on supported systems.
#' `"fork"` is unavailable on Windows. Prefer PSOCK in long-lived sessions
#' that also use child-process managers such as callr or processx, because
#' they can compete with R's fork-worker signal handler.
#' @param timeout Maximum number of seconds that a parallel worker task may run.
#' `Inf` disables task timeouts. This is ignored when execution uses one core.
#' @param total_timeout Maximum number of seconds allowed for the complete call.
#' `Inf` disables the overall deadline. In single-core mode the deadline is
#' checked between inputs but cannot interrupt a function that is already
#' running.
#' @param seed Optional integer seed. When supplied, every input receives a
#' deterministic independent L'Ecuyer-CMRG random-number stream, making results
#' reproducible across worker counts and scheduling order. The caller's random
#' number state is restored when the call finishes.
#' @param progress Whether to draw a dynamic terminal progress bar. The default
#' follows `verbose`. Set this to `FALSE` while keeping `verbose = TRUE` to emit
#' concise lifecycle messages through [log_message()] without terminal timing
#' output, for example in reports and persistent logs.
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
  timeout = Inf,
  total_timeout = Inf,
  seed = NULL,
  progress = verbose
) {
  call_started <- parallel_elapsed()
  fun <- match.fun(fun)
  backend <- match.arg(backend)
  timeout <- parallel_validate_timeout(timeout, "timeout")
  total_timeout <- parallel_validate_timeout(
    total_timeout,
    "total_timeout"
  )
  total_deadline <- call_started + total_timeout
  seed <- parallel_validate_seed(seed)
  total <- length(x)
  rng_streams <- NULL
  if (!is.null(seed)) {
    rng_state <- parallel_capture_rng_state()
    on.exit(parallel_restore_rng_state(rng_state), add = TRUE)
    rng_streams <- parallel_rng_streams(total, seed)
  }
  if (parallel_worker_depth() > 0L) {
    cores <- 1L
  }
  cores <- cores_detect(cores, total)
  has_names <- !is.null(names(x)) && any(names(x) != "")
  is_vector <- is.vector(x) && !is.list(x)
  show_values <- !has_names && is_vector
  show_progress <- isTRUE(verbose) && isTRUE(progress)

  if (show_progress) {
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

    if (show_progress) {
      output_list <- vector("list", total)

      for (i in seq_along(x)) {
        parallel_assert_total_time(total_deadline, total_timeout)
        parallel_set_rng_stream(rng_streams, i)
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
        parallel_assert_total_time(total_deadline, total_timeout)

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
          parallel_assert_total_time(total_deadline, total_timeout)
          parallel_set_rng_stream(rng_streams, i)
          result <- tryCatch(
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
          parallel_assert_total_time(total_deadline, total_timeout)
          result
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
      total_timeout = total_timeout,
      total_deadline = total_deadline,
      rng_streams = rng_streams,
      export_fun = export_fun,
      safe_call = safe_call,
      progress_id = if (show_progress) pb else NULL,
      progress_env = if (show_progress) progress_env else NULL,
      has_names = has_names,
      show_values = show_values
    )
    if (show_progress) {
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
          shown_inputs <- utils::head(inputs, max_show)
          shown <- mapply(
            parallel_task_label,
            input = unname(shown_inputs),
            name = names(shown_inputs) %ss% rep("", length(shown_inputs)),
            USE.NAMES = FALSE
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
  } else if (is_vector && length(x) > 0L) {
    names(output_list) <- as.character(x)
  }

  return(output_list)
}

parallel_task_label <- function(input, name = "") {
  if (length(name) == 1L && !is.na(name) && nzchar(name)) {
    label <- name
  } else if (is.atomic(input) && length(input) == 1L) {
    label <- as.character(input)
  } else if (!is.null(dim(input))) {
    label <- sprintf(
      "<%s %s>",
      class(input)[[1L]],
      paste(dim(input), collapse = " x ")
    )
  } else {
    label <- sprintf(
      "<%s length %d>",
      class(input)[[1L]],
      length(input)
    )
  }

  parse_inline_expressions("{.val {label}}", env = environment())
}

parallel_collect_results <- function(
  x,
  fun,
  total,
  cores,
  backend,
  timeout,
  total_timeout,
  total_deadline,
  rng_streams,
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
    total_timeout = total_timeout,
    total_deadline = total_deadline,
    rng_streams = rng_streams,
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
  total_timeout,
  total_deadline,
  rng_streams,
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
    context$rng_streams <- rng_streams
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
    worker_args <- function(indices) list(context_id, indices)
  } else {
    worker_context <- parallel_with_worker_context
    worker_task <- function(indices) {
      lapply(
        indices,
        function(i) {
          worker_context({
            if (!is.null(rng_streams)) {
              assign(".Random.seed", rng_streams[[i]], envir = globalenv())
            }
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
          })
        }
      )
    }
    worker_args <- function(indices) list(indices)

    parallel::clusterExport(
      cl = cl,
      varlist = "worker_task",
      envir = environment()
    )
    parallel::clusterExport(
      cl = cl,
      varlist = c(
        "parallelize_fun",
        "parallel_worker_depth",
        "parallel_with_worker_context",
        "parallel_validate_timeout",
        "parallel_validate_seed",
        "parallel_capture_rng_state",
        "parallel_restore_rng_state",
        "parallel_rng_streams",
        "parallel_set_rng_stream",
        "parallel_assert_total_time",
        "parallel_total_timeout_error"
      ),
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
  task_chunks <- parallel_task_chunks(total, cores, timeout)
  total_jobs <- length(task_chunks)
  job_started <- rep(NA_real_, total_jobs)
  next_job <- 1L
  active_workers <- min(length(cl), total_jobs)

  send_call <- parallel_scheduler_fun("sendCall")
  receive_result <- parallel_scheduler_fun("recvOneResult")
  parallel_assert_total_time(total_deadline, total_timeout)
  for (worker_idx in seq_len(active_workers)) {
    send_call(
      con = cl[[worker_idx]],
      fun = dispatch_task,
      args = worker_args(task_chunks[[next_job]]),
      tag = next_job
    )
    job_started[[next_job]] <- parallel_elapsed()
    next_job <- next_job + 1L
  }

  completed_jobs <- 0L
  while (completed_jobs < total_jobs) {
    active_jobs <- which(!is.na(job_started))
    deadlines <- stats::setNames(
      job_started[active_jobs] + timeout,
      active_jobs
    )
    result <- receive_parallel_result(
      cl,
      deadlines = deadlines,
      timeout = timeout,
      total_deadline = total_deadline,
      total_timeout = total_timeout,
      worker_pids = worker_pids,
      receive_result = receive_result
    )
    job_index <- as.integer(result$tag)
    task_indices <- task_chunks[[job_index]]
    task_values <- result$value
    if (!is.list(task_values) || length(task_values) != length(task_indices)) {
      stop(parallel_worker_error(cause = "Worker returned an invalid task batch."))
    }
    output_list[task_indices] <- task_values
    job_started[[job_index]] <- NA_real_
    completed_jobs <- completed_jobs + 1L

    if (!is.null(progress_id)) {
      for (task_index in task_indices) {
        cli::cli_progress_update(
          id = progress_id,
          inc = 1L,
          status = progress_status(x, task_index, has_names, show_values),
          .envir = progress_env
        )
      }
    }

    if (next_job <= total_jobs) {
      send_call(
        con = cl[[result$node]],
        fun = dispatch_task,
        args = worker_args(task_chunks[[next_job]]),
        tag = next_job
      )
      job_started[[next_job]] <- parallel_elapsed()
      next_job <- next_job + 1L
    }
  }

  force_cleanup <- FALSE
  output_list
}

parallel_task_chunks <- function(total, cores, timeout) {
  if (total < 1L) {
    return(list())
  }
  if (is.finite(timeout)) {
    return(as.list(seq_len(total)))
  }

  jobs <- min(total, max(1L, as.integer(cores) * 4L))
  parallel::splitIndices(total, jobs)
}

receive_parallel_result <- function(
  cl,
  deadlines = numeric(),
  timeout = Inf,
  total_deadline = Inf,
  total_timeout = Inf,
  worker_pids = integer(),
  receive_result = parallel_scheduler_fun("recvOneResult"),
  poll_interval = 0.1
) {
  connections <- lapply(cl, `[[`, "con")
  repeat {
    now <- parallel_elapsed()
    next_task_deadline <- if (length(deadlines)) min(deadlines) else Inf
    next_deadline <- min(next_task_deadline, total_deadline)
    wait <- min(poll_interval, max(0, next_deadline - now))

    if (any(base::socketSelect(connections, timeout = wait))) {
      return(tryCatch(
        receive_result(cl),
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
    if (now >= total_deadline) {
      stop(parallel_total_timeout_error(total_timeout))
    }
    timed_out <- names(deadlines)[deadlines <= now]
    if (length(timed_out)) {
      stop(parallel_timeout_error(as.integer(timed_out), timeout))
    }
  }
}

parallel_scheduler_fun <- function(name) {
  fun <- get_namespace_fun("parallel", name)
  if (!is.function(fun)) {
    stop(
      sprintf(
        "This R version does not provide the parallel scheduler function %s.",
        name
      ),
      call. = FALSE
    )
  }
  fun
}

parallel_validate_timeout <- function(value, name) {
  value <- suppressWarnings(as.numeric(value)[1L])
  if (!length(value) || is.na(value) || value <= 0) {
    stop(
      sprintf("%s must be a positive number or Inf.", name),
      call. = FALSE
    )
  }
  value
}

parallel_validate_seed <- function(seed) {
  if (is.null(seed)) {
    return(NULL)
  }
  if (length(seed) != 1L || !is.numeric(seed)) {
    stop("seed must be NULL or a single integer.", call. = FALSE)
  }
  value <- suppressWarnings(as.integer(seed)[1L])
  numeric_value <- suppressWarnings(as.numeric(seed)[1L])
  if (
    !length(value) ||
      is.na(value) ||
      !is.finite(numeric_value) ||
      numeric_value != value
  ) {
    stop("seed must be NULL or a single integer.", call. = FALSE)
  }
  value
}

parallel_capture_rng_state <- function() {
  has_seed <- exists(".Random.seed", envir = globalenv(), inherits = FALSE)
  list(
    kind = RNGkind(),
    has_seed = has_seed,
    seed = if (has_seed) {
      get(".Random.seed", envir = globalenv(), inherits = FALSE)
    } else {
      NULL
    }
  )
}

parallel_restore_rng_state <- function(state) {
  do.call(RNGkind, as.list(state$kind))
  if (isTRUE(state$has_seed)) {
    assign(".Random.seed", state$seed, envir = globalenv())
  } else if (exists(".Random.seed", envir = globalenv(), inherits = FALSE)) {
    rm(".Random.seed", envir = globalenv())
  }
  invisible(NULL)
}

parallel_rng_streams <- function(total, seed) {
  state <- parallel_capture_rng_state()
  on.exit(parallel_restore_rng_state(state), add = TRUE)

  RNGkind("L'Ecuyer-CMRG")
  set.seed(seed)
  streams <- vector("list", total)
  current <- get(".Random.seed", envir = globalenv(), inherits = FALSE)
  for (i in seq_len(total)) {
    streams[[i]] <- current
    current <- parallel::nextRNGStream(current)
  }
  streams
}

parallel_set_rng_stream <- function(streams, index) {
  if (!is.null(streams)) {
    assign(".Random.seed", streams[[index]], envir = globalenv())
  }
  invisible(NULL)
}

parallel_assert_total_time <- function(deadline, timeout) {
  if (parallel_elapsed() >= deadline) {
    stop(parallel_total_timeout_error(timeout))
  }
  invisible(NULL)
}

parallel_elapsed <- function() {
  unname(proc.time()[["elapsed"]])
}

parallel_worker_depth <- function() {
  depth <- suppressWarnings(
    as.integer(getOption("thisutils.parallel.depth", 0L))[1L]
  )
  if (!length(depth) || is.na(depth) || depth < 0L) {
    return(0L)
  }
  depth
}

parallel_with_worker_context <- function(code) {
  old_options <- options(
    thisutils.parallel.depth = parallel_worker_depth() + 1L
  )
  on.exit(options(old_options), add = TRUE)
  force(code)
}

parallel_process_alive <- function(pid) {
  handle <- tryCatch(
    ps::ps_handle(as.integer(pid)),
    error = function(e) NULL
  )
  if (is.null(handle) || !isTRUE(tryCatch(
    ps::ps_is_running(handle),
    error = function(e) FALSE
  ))) {
    return(FALSE)
  }

  status <- tryCatch(ps::ps_status(handle), error = function(e) NA_character_)
  !status %in% c("dead", "zombie")
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

parallel_total_timeout_error <- function(timeout) {
  structure(
    list(
      message = sprintf(
        "Parallel call exceeded the total timeout of %s seconds.",
        format(timeout, trim = TRUE)
      ),
      call = NULL,
      timeout = timeout
    ),
    class = c("parallelize_total_timeout", "error", "condition")
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
  on.exit({
    close_parallel_cluster_connections(cl)
    remove_parallel_cluster_tempdir(cl)
  }, add = TRUE)

  if (isTRUE(force)) {
    parallel_signal_workers(worker_pids, tools::SIGTERM)
    remaining <- parallel_wait_for_workers(worker_pids, timeout = 0.2)
    final_signal <- if (.Platform$OS.type == "windows") {
      tools::SIGTERM
    } else {
      tools::SIGKILL
    }
    parallel_signal_workers(remaining, final_signal)
    parallel_wait_for_workers(remaining, timeout = 1)
  }

  try(parallel::stopCluster(cl), silent = TRUE)
  invisible(NULL)
}

parallel_wait_for_workers <- function(worker_pids, timeout) {
  worker_pids <- unique(as.integer(worker_pids))
  worker_pids <- worker_pids[!is.na(worker_pids)]
  deadline <- parallel_elapsed() + timeout

  repeat {
    remaining <- worker_pids[vapply(
      worker_pids,
      parallel_process_alive,
      logical(1)
    )]
    if (!length(remaining) || parallel_elapsed() >= deadline) {
      return(remaining)
    }
    Sys.sleep(0.01)
  }
}

close_parallel_cluster_connections <- function(cl) {
  for (worker in cl) {
    connection <- tryCatch(
      worker[["con"]],
      error = function(e) NULL
    )
    if (!is.null(connection)) {
      try(close(connection), silent = TRUE)
    }
  }
  invisible(NULL)
}

parallel_signal_workers <- function(pids, signal) {
  for (pid in unique(as.integer(pids))) {
    try(tools::pskill(pid, signal), silent = TRUE)
  }
  invisible(NULL)
}

.parallel_fork_contexts <- new.env(parent = emptyenv())

parallel_fork_worker_task <- function(context_id, indices) {
  context <- .parallel_fork_contexts[[context_id]]
  lapply(
    indices,
    function(i) {
      parallel_with_worker_context({
        parallel_set_rng_stream(context$rng_streams, i)
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
      })
    }
  )
}

parallel_psock_worker_task <- function(indices) {
  get("worker_task", envir = globalenv(), inherits = FALSE)(indices)
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
  if (backend == "fork" && .Platform$OS.type == "windows") {
    stop(
      "The fork backend is unavailable on Windows; use backend = \"psock\".",
      call. = FALSE
    )
  }
  if (backend == "auto") {
    return("psock")
  }

  backend
}

make_parallel_cluster <- function(cores, backend = parallel_backend()) {
  if (backend == "psock") {
    return(make_parallel_psock_cluster(cores))
  }

  parallel::makeForkCluster(cores, outfile = nullfile())
}

make_parallel_psock_cluster <- function(cores) {
  if (.Platform$OS.type != "windows") {
    return(parallel::makePSOCKcluster(cores, outfile = nullfile()))
  }

  launch_dir <- tempfile("thisutils-psock-", tmpdir = tempdir())
  if (!dir.create(launch_dir)) {
    stop("Unable to create a temporary directory for PSOCK workers.", call. = FALSE)
  }
  complete <- FALSE
  on.exit({
    if (!complete) {
      unlink(launch_dir, recursive = TRUE, force = TRUE)
    }
  }, add = TRUE)

  old_tmpdir <- Sys.getenv("TMPDIR", unset = NA_character_)
  Sys.setenv(TMPDIR = launch_dir)
  on.exit({
    if (is.na(old_tmpdir)) {
      Sys.unsetenv("TMPDIR")
    } else {
      Sys.setenv(TMPDIR = old_tmpdir)
    }
  }, add = TRUE)

  cl <- parallel::makePSOCKcluster(cores, outfile = nullfile())
  attr(cl, "thisutils.psock.tempdir") <- launch_dir
  complete <- TRUE
  cl
}

remove_parallel_cluster_tempdir <- function(cl) {
  launch_dir <- attr(cl, "thisutils.psock.tempdir", exact = TRUE)
  if (is.null(launch_dir) || length(launch_dir) != 1L || is.na(launch_dir)) {
    return(invisible(NULL))
  }

  launch_dir <- normalizePath(launch_dir, winslash = "/", mustWork = FALSE)
  session_dir <- normalizePath(tempdir(), winslash = "/", mustWork = FALSE)
  if (
    !identical(dirname(launch_dir), session_dir) ||
      !startsWith(basename(launch_dir), "thisutils-psock-")
  ) {
    return(invisible(NULL))
  }

  for (attempt in seq_len(10L)) {
    unlink(launch_dir, recursive = TRUE, force = TRUE)
    if (!dir.exists(launch_dir)) {
      break
    }
    Sys.sleep(0.02)
  }
  invisible(NULL)
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
