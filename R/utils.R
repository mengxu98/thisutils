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

#' @title Correlation and covariance calculation for sparse matrix
#'
#' @inheritParams sparse_cor
pearson_correlation <- function(x, y = NULL) {
  if (!methods::is(x, "sparseMatrix")) {
    stop("x should be a sparse matrix.")
  }
  if (!is.null(y) && !methods::is(y, "sparseMatrix")) {
    stop("y should be a sparse matrix.")
  }

  result <- sparse_cov_cor(x, y)

  return(
    list(
      cov = result$cov,
      cor = result$cor
    )
  )
}


#' @title Fast correlation and covariance calcualtion for sparse matrices
#'
#' @inheritParams sparse_cor
sparse_cov_cor <- function(x, y = NULL) {
  if (!methods::is(x, "sparseMatrix")) {
    log_message(
      "x should be a sparse matrix",
      message_type = "error"
    )
  }
  n <- nrow(x)
  mu_x <- Matrix::colMeans(x)
  if (is.null(y)) {
    covmat <- (
      (as.matrix(Matrix::crossprod(x)) - n * Matrix::tcrossprod(mu_x)) / (n - 1)
    )
    sdvec <- sqrt(diag(covmat))
    cormat <- covmat / tcrossprod(sdvec)
  } else {
    if (!methods::is(y, "sparseMatrix")) {
      log_message(
        "y should be a sparse matrix",
        message_type = "error"
      )
    }
    if (nrow(x) != nrow(y)) {
      log_message(
        "x and y should have the same number of rows",
        message_type = "error"
      )
    }

    mu_y <- Matrix::colMeans(y)
    covmat <- (
      (as.matrix(Matrix::crossprod(x, y)) - n * Matrix::tcrossprod(mu_x, mu_y)) / (n - 1)
    )
    sdvecX <- sqrt((Matrix::colSums(x^2) - n * mu_x^2) / (n - 1))
    sdvecY <- sqrt((Matrix::colSums(y^2) - n * mu_y^2) / (n - 1))
    cormat <- covmat / Matrix::tcrossprod(sdvecX, sdvecY)
  }
  return(
    list(
      cov = covmat,
      cor = cormat
    )
  )
}

#' @title Generate a simulated sparse matrix
#'
#' @param nrow Number of rows (genes) in the matrix.
#' @param ncol Number of columns (cells) in the matrix.
#' @param density Density of non-zero elements (default: 0.1, representing 90 sparsity).
#' @param distribution_fun Function to generate non-zero values.
#' @param seed Random seed for reproducibility.
#'
#' @return A sparse matrix of class "dgCMatrix"
#' @export
#'
#' @examples
#' simulate_sparse_matrix(2000, 500) |>
#'   check_sparsity()
simulate_sparse_matrix <- function(
    nrow,
    ncol,
    density = 0.1,
    distribution_fun = function(n) stats::rpois(n, lambda = 0.5) + 1,
    seed = 1) {
  set.seed(seed)

  nnz <- round(nrow * ncol * density)

  i <- sample(1:nrow, nnz, replace = TRUE)
  j <- sample(1:ncol, nnz, replace = TRUE)
  x <- distribution_fun(nnz)

  Matrix::sparseMatrix(
    i = i,
    j = j,
    x = x,
    dims = c(nrow, ncol),
    dimnames = list(
      paste0("cell_", 1:nrow),
      paste0("gene_", 1:ncol)
    )
  )
}

#' @title Safe correlation function which returns a sparse matrix without missing values
#'
#' @param x Sparse matrix or character vector.
#' @param y Sparse matrix or character vector.
#' @param method Method to use for calculating the correlation coefficient.
#' @param allow_neg Logical. Whether to allow negative values or set them to 0.
#' @param remove_na Logical. Whether to replace NA values with 0.
#' @param remove_inf Logical. Whether to replace infinite values with 1.
#' @param ... Other arguments passed to \code{\link[stats]{cor}} function.
#'
#' @return A correlation matrix.
#'
#' @export
#'
#' @examples
#' m1 <- simulate_sparse_matrix(
#'   500, 500,
#'   density = 0.01
#' )
#' m2 <- simulate_sparse_matrix(
#'   500, 100,
#'   density = 0.05
#' )
#' a <- as_matrix(sparse_cor(m1))
#' b <- as_matrix(sparse_cor(m1, m2))
#' all.equal(a, b)
#'
#' all.equal(
#'   b,
#'   as_matrix(cor(as_matrix(m1), as_matrix(m2)))
#' )
#'
#' m1[sample(1:500, 10)] <- NA
#' m2[sample(1:500, 10)] <- NA
#'
#' sparse_cor(m1, m2)[1:5, 1:5]
#'
#' system.time(
#'   sparse_cor(m1)
#' )
#' system.time(
#'   cor(as_matrix(m1))
#' )
#' system.time(
#'   sparse_cor(m1, m2)
#' )
#' system.time(
#'   cor(as_matrix(m1), as_matrix(m2))
#' )
sparse_cor <- function(
    x,
    y = NULL,
    method = "pearson",
    allow_neg = TRUE,
    remove_na = TRUE,
    remove_inf = TRUE,
    ...) {
  if (!methods::is(x, "sparseMatrix")) {
    x <- as_matrix(x, sparse = TRUE)
  }

  if (!is.null(y)) {
    if (!methods::is(y, "sparseMatrix")) {
      y <- as_matrix(y, sparse = TRUE)
    }
    if (nrow(x) != nrow(y)) {
      stop("x and y must have the same number of rows.")
    }
  }

  corr_mat <- switch(
    EXPR = method,
    "pearson" = pearson_correlation(x, y)$cor,
    "spearman" = {
      if (is.null(y)) {
        stats::cor(
          as_matrix(x),
          method = "spearman",
          ...
        )
      } else {
        stats::cor(
          as_matrix(x),
          as_matrix(y),
          method = "spearman",
          ...
        )
      }
    },
    "kendall" = {
      if (is.null(y)) {
        stats::cor(
          as_matrix(x),
          method = "kendall",
          ...
        )
      } else {
        stats::cor(
          as_matrix(x),
          as_matrix(y),
          method = "kendall",
          ...
        )
      }
    }
  )

  if (is.null(y)) {
    colnames(corr_mat) <- colnames(x)
  } else {
    colnames(corr_mat) <- colnames(y)
  }
  rownames(corr_mat) <- colnames(x)

  if (remove_na) {
    corr_mat[is.na(corr_mat)] <- 0
  }
  if (remove_inf) {
    corr_mat[is.infinite(corr_mat)] <- 1
  }

  corr_mat <- as_matrix(corr_mat, sparse = TRUE)

  if (!allow_neg) {
    corr_mat[corr_mat < 0] <- 0
  }

  return(corr_mat)
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

.is_scalar <- function(x) {
  is.atomic(x) && length(x) == 1L && !is.character(x) && Im(x) == 0 && !is.nan(x) && !is.na(x)
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
#' y_true <- rnorm(100)
#' y_pred <- rnorm(100)
#' r_square(y_true, y_pred)
r_square <- function(y_true, y_pred) {
  return(
    1 - .rse(y_true, y_pred)
  )
}
