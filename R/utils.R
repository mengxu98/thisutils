#' @title Print diagnostic message
#'
#' @description
#' Integrate the message printing function with the `cli` package,
#' and the \code{\link[base]{message}} function.
#' The message could be suppressed by \code{\link[base]{suppressMessages}}.
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
#' @param level Integer value, default is *`1`*.
#' The level of the message, which affects the indentation.
#' Level 1 has no indentation, higher levels add more indentation.
#' @param level_symbol Character value, default is *`"  "`* (two spaces).
#' The symbol used for indentation at each level.
#'
#' @return \code{Formated message} printed to the console.
#'
#' @export
#' @examples
#' log_message("Hello, ", "world!")
#' log_message("Hello, world!", timestamp = FALSE)
#' log_message("Hello, ", "world!", message_type = "success")
#' log_message("Hello, world!", message_type = "warning")
#' suppressMessages(log_message("Hello, ", "world!"))
#' log_message("Hello, world!", verbose = FALSE)
#' log_message("Hello, world!", level = 2)
#' log_message("Hello, world!", level = 3, level_symbol = "->")
log_message <- function(
    ...,
    verbose = TRUE,
    message_type = c("info", "success", "warning", "error"),
    cli_model = TRUE,
    timestamp = TRUE,
    level = 1,
    level_symbol = "  ") {
  message_type <- match.arg(message_type)
  msg <- paste0(...)

  if (message_type == "error") {
    stop(msg, call. = FALSE)
  }

  if (!verbose) {
    return(invisible(NULL))
  }

  if (level > 1) {
    indentation <- paste(rep(level_symbol, level - 1), collapse = "")
    msg <- paste0(indentation, msg)
  }

  if (timestamp) {
    time_str <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
    msg <- paste0("[", time_str, "] ", msg)
  }

  if (cli_model) {
    switch(
      EXPR = message_type,
      "info" = cli::cli_alert_info(msg),
      "success" = cli::cli_alert_success(msg),
      "warning" = cli::cli_alert_warning(msg)
    )
  } else {
    prefix <- switch(
      EXPR = message_type,
      "info" = "",
      "success" = "",
      "warning" = "WARNING: "
    )
    message(prefix, msg)
  }

  invisible(NULL)
}

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

#' @title Parallelize a function
#'
#' @md
#' @param x A vector or list to apply over.
#' @param fun The function to be applied to each element.
#' @param cores The number of cores to use for parallelization with \code{\link[foreach]{foreach}}.
#' Default is *`1`*.
#' @param export_fun The functions to export the function to workers.
#' @param verbose Logical value, default is *`TRUE`*.
#' Whether to print progress messages.
#'
#' @return A list of computed results
#'
#' @export
#'
#' @examples
#' parallelize_fun(1:3, function(x) x^2)
#' parallelize_fun(list(1, 2, 3), function(x) x^2)
parallelize_fun <- function(
    x,
    fun,
    cores = 1,
    export_fun = NULL,
    verbose = TRUE) {
  time_start <- Sys.time()
  if (cores == 1) {
    log_message(
      "Using 1 core",
      verbose = verbose
    )
    time_str <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
    if (verbose) {
      pb_id <- cli::cli_progress_bar(
        total = length(x),
        format = paste0(
          "{cli::pb_spin} [{time_str}] Running [{cli::pb_current}/{cli::pb_total}] ETA: {cli::pb_eta}"
        )
      )
      fun_progress <- function(...) {
        on.exit(cli::cli_progress_update(id = pb_id), add = TRUE)
        fun(...)
      }
      output_list <- purrr::map(x, fun_progress)
    }
    if (!verbose) {
      output_list <- base::lapply(X = x, FUN = fun)
    }
  }

  if (cores > 1) {
    cores <- .cores_detect(cores, length(x))

    doParallel::registerDoParallel(cores = cores)
    log_message(
      "Using ", foreach::getDoParWorkers(), " cores",
      verbose = verbose
    )

    i <- NULL
    "%dopar%" <- foreach::"%dopar%"
    output_list <- foreach::foreach(
      i = seq_along(x),
      .export = export_fun
    ) %dopar% {
      fun(x[[i]])
    }
    doParallel::stopImplicitCluster()
  }

  names(output_list) <- x

  time_end <- Sys.time()
  elapsed <- as.numeric(time_end - time_start)

  log_message(
    .elapsed_str(elapsed)
  )
  return(output_list)
}

.elapsed_str <- function(x) {
  if (x < 60) {
    elapsed_str <- sprintf("Elapsed %.2f sec", x)
  } else if (x < 3600) {
    min <- floor(x / 60)
    sec <- x %% 60
    elapsed_str <- sprintf("Elapsed %d min %.2f sec", min, sec)
  } else {
    hour <- floor(x / 3600)
    min <- floor((x %% 3600) / 60)
    sec <- x %% 60
    elapsed_str <- sprintf("Elapsed %d h %d min %.2f sec", hour, min, sec)
  }

  return(elapsed_str)
}

.cores_detect <- function(
    cores = 1,
    num_session = NULL) {
  if (is.null(num_session)) {
    return(1)
  } else {
    cores <- min(
      (parallel::detectCores(logical = FALSE) - 1), cores, num_session
    )

    return(cores)
  }
}

#' @title Generate a simulated sparse matrix
#'
#' @description This function generates a sparse matrix with a specified number of rows and columns,
#' a given sparsity level, and a distribution function for the non-zero values.
#'
#' @md
#' @param nrow Number of rows in the matrix.
#' @param ncol Number of columns in the matrix.
#' @param sparsity Proportion of zero elements (sparsity level).
#' Default is 0.95, meaning 95% of elements are zero (5% are non-zero).
#' @param distribution_fun Function to generate non-zero values.
#' @param decimal Numeric value, default is *`0`*.
#' Controls the number of decimal places in the generated values.
#' If set to *`0`*, values will be integers.
#' When decimal > 0, random decimal parts are uniformly distributed across the full range.
#' @param seed Random seed for reproducibility.
#'
#' @return A sparse matrix of class "dgCMatrix"
#' @export
#'
#' @examples
#' simulate_sparse_matrix(1000, 500) |>
#'   check_sparsity()
#'
#' simulate_sparse_matrix(10, 10, decimal = 1)
#' simulate_sparse_matrix(10, 10, decimal = 5)
simulate_sparse_matrix <- function(
    nrow,
    ncol,
    sparsity = 0.95,
    distribution_fun = function(n) stats::rpois(n, lambda = 0.5) + 1,
    decimal = 0,
    seed = 1) {
  set.seed(seed)

  nnz <- round(nrow * ncol * (1 - sparsity))

  total_positions <- nrow * ncol
  if (nnz > total_positions) {
    nnz <- total_positions
  }

  positions <- sample(total_positions, nnz, replace = FALSE)

  i <- ((positions - 1) %% nrow) + 1
  j <- ((positions - 1) %/% nrow) + 1

  x <- distribution_fun(nnz)

  if (decimal > 0) {
    decimal_part <- stats::runif(nnz, min = 0, max = 1)
    x <- x + decimal_part
  }

  x <- round(x, decimal)

  Matrix::sparseMatrix(
    i = i,
    j = j,
    x = x,
    dims = c(nrow, ncol),
    dimnames = list(
      paste0("row_", 1:nrow),
      paste0("col_", 1:ncol)
    )
  )
}

#' @title Check sparsity of matrix
#'
#' @param x A matrix.
#'
#' @return Sparsity of matrix
#' @export
check_sparsity <- function(x) {
  if (methods::is(x, "sparseMatrix")) {
    non_zero_count <- Matrix::nnzero(x)
    total_counts <- prod(dim(x))
  } else {
    non_zero_count <- sum(x != 0)
    total_counts <- length(x)
  }

  sparsity_ratio <- non_zero_count / total_counts

  sparsity <- 1 - sparsity_ratio

  return(sparsity)
}

#' @title Normalize numeric vector
#'
#' @param x Input numeric vector.
#' @param method Method used for normalization.
#' @param na_rm Whether to remove `NA` values,
#' and if setting TRUE, using `0` instead.
#' @param ... Parameters for other methods.
#'
#' @md
#' @return Normalized numeric vector
#' @export
#'
#' @examples
#' nums <- c(runif(2), NA, -runif(2))
#' nums
#' normalization(nums, method = "max_min")
#' normalization(nums, method = "maximum")
#' normalization(nums, method = "sum")
#' normalization(nums, method = "softmax")
#' normalization(nums, method = "z_score")
#' normalization(nums, method = "mad")
#' normalization(nums, method = "unit_vector")
#' normalization(nums, method = "unit_vector", na_rm = FALSE)
normalization <- function(
    x,
    method = "max_min",
    na_rm = TRUE,
    ...) {
  method <- match.arg(
    method,
    c(
      "max_min",
      "maximum",
      "sum",
      "softmax",
      "z_score",
      "mad",
      "unit_vector",
      "robust_scale"
    )
  )
  na_index <- which(is.na(x))
  x[na_index] <- 0
  x <- switch(
    EXPR = method,
    "max_min" = {
      (x - min(x)) / (max(x) - min(x))
    },
    "maximum" = {
      x / max(abs(x))
    },
    "sum" = {
      x / sum(abs(x))
    },
    "softmax" = {
      # exp(x - max(x)) / sum(exp(x - max(x)))
      temp <- (x - mean(x)) / stats::sd(x)
      exp(temp) / sum(exp(temp))
    },
    "z_score" = {
      (x - mean(x)) / stats::sd(x)
    },
    "mad" = {
      (x - stats::median(x)) / stats::mad(x)
    },
    "unit_vector" = {
      x / sqrt(sum(x^2))
    }
  )

  if (!na_rm) {
    x[na_index] <- NA
  }

  return(x)
}

.rmse <- function(true, pred) {
  return(
    sqrt(mean((true - pred)^2))
  )
}

.sse <- function(y_true, y_pred) {
  return(
    sum((y_true - y_pred)**2)
  )
}

.rse <- function(y_true, y_pred) {
  return(
    .sse(y_true, y_pred) / .sse(y_true, mean(y_true))
  )
}

#' @title coefficient of determination (\eqn{R^2})
#'
#' @md
#' @param y_true A numeric vector with ground truth values.
#' @param y_pred A numeric vector with predicted values.
#'
#' @return \eqn{R^2} value
#'
#' @export
#'
#' @examples
#' y <- rnorm(100)
#' y_pred <- y + rnorm(100, sd = 0.5)
#' r_square(y, y_pred)
r_square <- function(y_true, y_pred) {
  return(
    1 - .rse(y_true, y_pred)
  )
}
