#' @title Collapse sparse matrix rows by group
#'
#' @param matrix A sparse matrix.
#' @param group A vector defining the output row groups.
#'
#' @return A sparse matrix with rows collapsed by `group`.
#'
#' @export
#' @examples
#' mat <- Matrix::Matrix(
#'   matrix(c(1, 0, 2, 0, 3, 4), nrow = 3, byrow = TRUE),
#'   sparse = TRUE
#' )
#' collapse_sparse_rows(mat, c("g1", "g1", "g2"))
collapse_sparse_rows <- function(matrix, group) {
  if (length(group) != nrow(matrix)) {
    cli::cli_abort(
      "{.arg group} length must match the number of rows of {.arg matrix}"
    )
  }

  group <- as.character(group)
  keep <- !is.na(group) & nzchar(group)
  matrix <- matrix[keep, , drop = FALSE]
  group <- group[keep]

  levels_use <- unique(group)
  matrix_summary <- Matrix::summary(matrix)
  i_new <- match(group[matrix_summary$i], levels_use)

  Matrix::sparseMatrix(
    i = i_new,
    j = matrix_summary$j,
    x = matrix_summary$x,
    dims = c(length(levels_use), ncol(matrix)),
    dimnames = list(levels_use, colnames(matrix))
  )
}

sparse_matrix_maxima <- function(x, margin = 1L, na.rm = TRUE) {
  x <- methods::as(x, "CsparseMatrix")
  if (identical(margin, 1L)) {
    x <- Matrix::t(x)
  }
  nr <- nrow(x)
  nc <- ncol(x)
  out <- if (nr > 0L) {
    rep(0, nc)
  } else {
    rep(-Inf, nc)
  }
  names(out) <- colnames(x)
  if (nr == 0L || nc == 0L) {
    return(out)
  }
  sm <- Matrix::summary(x)
  if (nrow(sm) == 0L) {
    return(out)
  }
  stored_n <- tabulate(sm$j, nbins = nc)
  maxima <- tapply(sm$x, sm$j, max, na.rm = na.rm)
  idx <- as.integer(names(maxima))
  implicit_zero <- stored_n[idx] < nr
  values <- as.numeric(maxima)
  replace_with_zero <- implicit_zero & (is.na(values) | values < 0)
  values[replace_with_zero] <- 0
  out[idx] <- values
  out
}

#' @title Column maxima
#'
#' @param x A dense or sparse matrix.
#' @param na.rm Whether to ignore `NA` values.
#'
#' @return A numeric vector of column maxima. Implicit sparse zeros are
#' treated as zero.
#'
#' @export
#'
#' @examples
#' col_maxs(matrix(c(-2, 0, 3, 1), nrow = 2))
col_maxs <- function(x, na.rm = TRUE) {
  if (inherits(x, "sparseMatrix")) {
    return(sparse_matrix_maxima(x, margin = 2L, na.rm = na.rm))
  }
  stats::setNames(apply(as.matrix(x), 2L, max, na.rm = na.rm), colnames(x))
}

#' @title Row maxima
#'
#' @inheritParams col_maxs
#'
#' @return A numeric vector of row maxima. Implicit sparse zeros are
#' treated as zero.
#'
#' @export
#'
#' @examples
#' row_maxs(matrix(c(-2, 0, 3, 1), nrow = 2))
row_maxs <- function(x, na.rm = TRUE) {
  if (inherits(x, "sparseMatrix")) {
    return(sparse_matrix_maxima(x, margin = 1L, na.rm = na.rm))
  }
  stats::setNames(apply(as.matrix(x), 1L, max, na.rm = na.rm), rownames(x))
}

aggregate_summary_fun <- list(
  mean = function(x) base::mean(x, na.rm = TRUE),
  sum = base::sum,
  max = function(x) base::max(x, na.rm = TRUE),
  min = function(x) base::min(x, na.rm = TRUE)
)

dMcast <- function(
  data,
  formula,
  fun.aggregate = "sum",
  value.var = NULL,
  as.factors = FALSE,
  factor.nas = TRUE,
  drop.unused.levels = TRUE
) {
  values <- 1
  if (!is.null(value.var)) {
    values <- data[, value.var]
  }
  alltms <- stats::terms(formula, data = data)
  response <- rownames(attr(alltms, "factors"))[attr(alltms, "response")]
  tm <- attr(alltms, "term.labels")
  interactions <- tm[grep(":", tm)]
  simple <- setdiff(tm, interactions)
  i2 <- strsplit(interactions, ":")
  newterms <- unlist(
    lapply(
      i2, function(x) {
        paste("paste(", paste(x, collapse = ","), ",", "sep='_'", ")")
      }
    )
  )
  newterms <- c(simple, newterms)
  newformula <- stats::as.formula(
    paste("~0+", paste(newterms, collapse = "+"))
  )
  allvars <- all.vars(alltms)
  data <- data[, c(allvars), drop = FALSE]
  if (as.factors) {
    data <- data.frame(lapply(data, as.factor))
  }
  characters <- unlist(lapply(data, is.character))
  data[, characters] <- lapply(data[, characters, drop = FALSE], as.factor)
  factors <- unlist(lapply(data, is.factor))
  data[, factors] <- lapply(
    data[, factors, drop = FALSE], function(x) {
      if (factor.nas) {
        if (any(is.na(x))) {
          levels(x) <- c(levels(x), "NA")
          x[is.na(x)] <- "NA"
        }
      }
      if (drop.unused.levels) {
        if (nlevels(x) != length(stats::na.omit(unique(x)))) {
          x <- factor(as.character(x))
        }
      }
      y <- stats::contrasts(x, contrasts = FALSE, sparse = TRUE)
      attr(x, "contrasts") <- y
      x
    }
  )
  attr(data, "na.action") <- stats::na.pass
  result <- Matrix::sparse.model.matrix(
    newformula,
    data,
    drop.unused.levels = FALSE,
    row.names = FALSE
  )
  broken_names <- grep("paste(", colnames(result), fixed = TRUE)
  colnames(result)[broken_names] <- lapply(
    colnames(result)[broken_names], function(x) {
      x <- gsub("paste(", replacement = "", x = x, fixed = TRUE)
      x <- gsub(pattern = ", ", replacement = "_", x = x, fixed = TRUE)
      x <- gsub(
        pattern = '_sep = \"_\")',
        replacement = "",
        x = x,
        fixed = TRUE
      )
      x
    }
  )
  result <- result * values
  if (isTRUE(response > 0)) {
    responses <- all.vars(
      stats::terms(stats::as.formula(paste(response, "~0")))
    )
    result <- fast_aggregate(
      result,
      data[, responses, drop = FALSE],
      fun = fun.aggregate
    )
  }
  result
}

#' @title Fast sparse aggregation
#'
#' @param x A matrix. Dense inputs are coerced to a sparse matrix.
#' @param groupings Group labels, recycled to a one-column data frame.
#' @param form Optional model formula used to build the grouping design.
#' @param fun Aggregation: `"sum"`, `"mean"`, or `"count"`.
#' @param ... Unused.
#'
#' @return A sparse matrix with groups in rows.
#'
#' @export
#'
#' @examples
#' fast_aggregate(
#'   matrix(1:6, nrow = 3),
#'   groupings = c("a", "a", "b"),
#'   fun = "sum"
#' )
fast_aggregate <- function(
  x,
  groupings = NULL,
  form = NULL,
  fun = "sum",
  ...
) {
  if (!inherits(x, "Matrix")) {
    x <- Matrix::Matrix(as.matrix(x), sparse = TRUE)
  }
  if (identical(fun, "count")) {
    x <- x != 0
  }
  groupings2 <- groupings
  if (!is.data.frame(groupings2)) {
    groupings2 <- as.data.frame(groupings2)
  }
  groupings2 <- data.frame(lapply(groupings2, as.factor))
  groupings2 <- data.frame(interaction(groupings2, sep = "_"))
  colnames(groupings2) <- "A"
  if (is.null(form)) {
    form <- stats::as.formula("~0+.")
  }
  form <- stats::as.formula(form)
  mapping <- dMcast(groupings2, form)
  colnames(mapping) <- substring(colnames(mapping), 2)
  result <- Matrix::t(mapping) %*% x
  if (identical(fun, "mean")) {
    result@x <- result@x / (fast_aggregate(x, groupings2, fun = "count"))@x
  }
  result
}

#' @title Aggregate matrix over groups
#'
#' @param x A matrix.
#' @param groups A character vector with the groups to aggregate over.
#' Length must be `nrow(x)` or `1`.
#' @param fun Summary applied to each group: `"mean"`, `"sum"`, `"count"`,
#' `"max"`, `"min"`, or a function.
#'
#' @return A sparse summary matrix with groups in rows.
#'
#' @export
#'
#' @examples
#' aggregate_matrix(
#'   matrix(1:6, nrow = 3),
#'   groups = c("a", "a", "b"),
#'   fun = "mean"
#' )
aggregate_matrix <- function(
  x,
  groups = NULL,
  fun = "mean"
) {
  if (length(groups) == nrow(x) && is.character(fun)) {
    if (fun %in% c("count", "sum")) {
      return(fast_aggregate(x = x, groupings = groups, fun = fun))
    }
    if (identical(fun, "mean")) {
      group_counts <- as.numeric(table(groups))
      agg_mat <- fast_aggregate(x = x, groupings = groups, fun = "sum")
      return(agg_mat / group_counts)
    }
  }
  if (is.character(fun)) {
    fun <- aggregate_summary_fun[[fun]]
  }
  if (!is.function(fun)) {
    log_message(
      "{.arg fun} must be a known summary name or a function",
      message_type = "error"
    )
  }
  if (length(groups) == nrow(x)) {
    agg_mat <- sapply(levels(factor(groups)), function(g) {
      chunk <- x[which(groups == g), ]
      if (is.null(dim(chunk))) {
        chunk
      } else {
        fun(chunk)
      }
    })
    agg_mat <- Matrix::Matrix(agg_mat, sparse = TRUE)
  } else if (length(groups) <= 1) {
    agg_mat <- fun(x)
    agg_mat <- Matrix::Matrix(agg_mat, sparse = TRUE)
    colnames(agg_mat) <- groups
    rownames(agg_mat) <- colnames(x)
  } else {
    log_message(
      "Length of {.arg groups} must be either nrow(x) or 1",
      message_type = "error"
    )
  }
  Matrix::t(agg_mat)
}
