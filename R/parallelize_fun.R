#' @title Parallelize a function
#'
#' @md
#' @param x A vector or list to apply over.
#' @param fun The function to be applied to each element.
#' @param cores The number of cores to use for parallelization with [foreach::foreach].
#' Default is `1`.
#' @param export_fun The functions to export the function to workers.
#' @param verbose Whether to print progress messages.
#' Default is `TRUE`.
#'
#' @return
#' A list of computed results.
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
parallelize_fun <- function(
    x,
    fun,
    cores = 1,
    export_fun = NULL,
    verbose = TRUE) {
  total <- length(x)
  if (verbose) {
    time_str <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
    options(cli.progress_show_after = 0)
    options(cli.progress_clear = FALSE)
    pb <- cli::cli_progress_bar(
      format = paste0(
        "{cli::pb_spin} [{time_str}] ",
        "Running [{.pkg {cli::pb_current}}/{.pkg {cli::pb_total}}] ",
        "ETA: {cli::pb_eta}"
      ),
      format_done = paste0(
        "{cli::col_green(cli::symbol$tick)} [{time_str}] ",
        "Completed {.pkg {cli::pb_total}} tasks ",
        "in {cli::pb_elapsed}"
      ),
      total = total,
      clear = FALSE
    )
  }

  if (cores == 1) {
    log_message(
      "Using {.pkg {1}} core",
      verbose = verbose
    )

    if (verbose) {
      output_list <- vector("list", total)


      for (i in seq_along(x)) {
        output_list[[i]] <- fun(x[[i]])
        cli::cli_progress_update(id = pb)
      }

      cli::cli_progress_done(id = pb)
    } else {
      output_list <- base::lapply(X = x, FUN = fun)
    }
  }

  if (cores > 1) {
    cores <- .cores_detect(cores, total)
    doParallel::registerDoParallel(cores = cores)

    log_message(
      "Using {.pkg {foreach::getDoParWorkers()}} cores",
      verbose = verbose
    )

    if (verbose) {
      chunks <- split(seq_along(x), rep(1:cores, length.out = total))

      output_chunks <- vector("list", total)
      for (chunk_idx in seq_along(chunks)) {
        chunk <- chunks[[chunk_idx]]

        i <- NULL
        "%dopar%" <- foreach::"%dopar%"
        chunk_results <- foreach::foreach(
          i = chunk,
          .combine = "c",
          .export = export_fun
        ) %dopar% {
          list(fun(x[[i]]))
        }

        output_chunks[[chunk_idx]] <- chunk_results
        cli::cli_progress_update(id = pb, inc = length(chunk))
      }

      cli::cli_progress_done(id = pb)

      output_list <- unlist(output_chunks, recursive = FALSE)
    } else {
      i <- NULL
      "%dopar%" <- foreach::"%dopar%"
      output_list <- foreach::foreach(
        i = seq_along(x),
        .export = export_fun
      ) %dopar% {
        fun(x[[i]])
      }
    }

    doParallel::stopImplicitCluster()
  }

  log_message(
    "Building results",
    verbose = verbose
  )
  if (verbose) {
    options(cli.progress_show_after = NULL)
    options(cli.progress_clear = NULL)
  }

  names(output_list) <- x

  return(output_list)
}

.cores_detect <- function(
    cores = 1,
    num_session = NULL) {
  if (is.null(num_session)) {
    return(1)
  }
  cores <- min(
    (parallel::detectCores(logical = FALSE) - 1),
    cores,
    num_session
  )
  return(cores)
}
