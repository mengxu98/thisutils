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

  1 - sparsity_ratio
}

.rmse <- function(true, pred) {
  sqrt(mean((true - pred)^2))
}

.sse <- function(y_true, y_pred) {
  sum((y_true - y_pred)**2)
}

.rse <- function(y_true, y_pred) {
  .sse(y_true, y_pred) / .sse(y_true, mean(y_true))
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
  1 - .rse(y_true, y_pred)
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
#' x <- c(runif(2), NA, -runif(2))
#' x
#' normalization(x, method = "max_min")
#' normalization(x, method = "maximum")
#' normalization(x, method = "sum")
#' normalization(x, method = "softmax")
#' normalization(x, method = "z_score")
#' normalization(x, method = "mad")
#' normalization(x, method = "unit_vector")
#' normalization(x, method = "unit_vector", na_rm = FALSE)
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

  x
}

#' @title Rescale numeric vector
#'
#' @param x A numeric vector.
#' @param from The range of the original data.
#' @param to The range of the rescaled data.
#'
#' @return A numeric vector with rescaled values.
#' @export
#'
#' @examples
#' x <- rnorm(10)
#' rescale(x)
#' rescale(x, from = c(0, 1))
#' rescale(x, to = c(0, 2))
rescale <- function(
    x,
    from = range(x, na.rm = TRUE, finite = TRUE),
    to = c(0, 1)) {
  if (zero_range(from) || zero_range(to)) {
    return(ifelse(is.na(x), NA, mean(to)))
  } else {
    return((x - from[1]) / diff(from) * diff(to) + to[1])
  }
}

zero_range <- function(
    x,
    tol = 1000 * .Machine$double.eps) {
  if (length(x) == 1) {
    return(TRUE)
  }
  if (length(x) != 2) {
    log_message(
      "x must be length 1 or 2",
      message_type = "error"
    )
  }
  if (any(is.na(x))) {
    return(NA)
  }
  if (x[1] == x[2]) {
    return(TRUE)
  }
  if (all(is.infinite(x))) {
    return(FALSE)
  }
  m <- min(abs(x))
  if (m == 0) {
    return(FALSE)
  }
  abs((x[1] - x[2]) / m) < tol
}
