#' @title Fit feature trends
#'
#' @md
#' @param x Numeric matrix with features in rows and observations in columns.
#' @param time Finite numeric coordinate per observation. Named coordinates are
#'   aligned to column names; unnamed coordinates are positional.
#' @param method Fitting method: `"gam"` or `"pretsa"`.
#' @param family GAM distribution, scalar or one named value per feature.
#' @param exposure Positive observation exposures for GAM offsets.
#' @param use_exposure Logical scalar or feature vector selecting which
#'   non-Gaussian fits use observation exposures.
#' @param reference_exposure Positive scalar offset for other fits and reference
#'   scale for predictions. Defaults to the median positive finite exposure.
#' @param knot Nonnegative number of interior knots or `"auto"` for BIC selection.
#' @param max_knot_allowed Maximum interior knots in automatic selection.
#' @param padjust_method Method passed to [stats::p.adjust()].
#' @param return_fitted Include fitted, lower and upper matrices in the result.
#' @param cores Number of GAM workers.
#' @param verbose Report fitting failures and progress.
#'
#' @return A list with `statistics`, `time`, and optional `fitted`, `lower`,
#'   and `upper` matrices, in input order. Statistics contain feature names,
#'   counts above each feature's minimum, fit scores, peak/valley times,
#'   P-values and adjusted P-values. Failed GAM fits return NA.
#'   GAM bounds use +/- 2 standard errors on the link scale;
#'   PreTSA bounds equal fitted values, not confidence intervals.
#'
#' @details Inputs are not transformed. PreTSA requires finite values;
#'   GAM omits unusable observations. Peak/valley times summarize the
#'   upper/lower 1% of fitted values, including ties for PreTSA.
#'
#' @references Zhuang and Ji, PreTSA, https://github.com/haotian-zhuang/PreTSA
#' @export
#'
#' @examples
#' t <- seq(0, 1, length.out = 40)
#' x <- rbind(a = sin(t * 5), b = cos(t * 3))
#' fit_trends(x, t, method = "pretsa")$statistics
fit_trends <- function(
  x, time, method = c("gam", "pretsa"), family = "gaussian",
  exposure = rep(1, ncol(x)), use_exposure = FALSE,
  reference_exposure = NULL, knot = 0, max_knot_allowed = 10,
  padjust_method = "fdr", return_fitted = TRUE, cores = 1, verbose = TRUE
) {
  method <- match.arg(method)
  padjust_method <- match.arg(padjust_method, stats::p.adjust.methods)
  x <- as.matrix(x)
  if (!is.numeric(x) || length(dim(x)) != 2L || any(dim(x) == 0L)) {
    stop("`x` must be a nonempty numeric matrix.", call. = FALSE)
  }
  if (is.null(rownames(x))) rownames(x) <- paste0("feature", seq_len(nrow(x)))
  if (is.null(colnames(x))) colnames(x) <- paste0("observation", seq_len(ncol(x)))
  if (anyNA(dimnames(x)[[1]]) || anyNA(dimnames(x)[[2]]) ||
    anyDuplicated(rownames(x)) || anyDuplicated(colnames(x))) {
    stop("Matrix dimension names must be unique and nonmissing.", call. = FALSE)
  }
  align <- function(v, ids, scalar = FALSE) {
    if (scalar && length(v) == 1L) {
      return(rep(unname(v), length(ids)))
    }
    if (length(v) != length(ids)) stop("Argument length does not match the matrix.", call. = FALSE)
    if (!is.null(names(v))) {
      if (anyDuplicated(names(v)) || !setequal(names(v), ids)) {
        stop("Argument names must match matrix dimension names.", call. = FALSE)
      }
      v <- v[ids]
    }
    unname(v)
  }
  time <- align(time, colnames(x))
  if (!is.numeric(time) || any(!is.finite(time))) {
    stop("`time` must contain finite numeric coordinates.", call. = FALSE)
  }
  exposure <- align(exposure, colnames(x), TRUE)
  if (!is.numeric(exposure)) stop("`exposure` must be numeric.", call. = FALSE)
  family <- align(family, rownames(x), TRUE)
  use_exposure <- align(use_exposure, rownames(x), TRUE)
  if (!is.character(family) || anyNA(family) ||
    !is.logical(use_exposure) || anyNA(use_exposure)) {
    stop("Provide character families and logical exposure flags.", call. = FALSE)
  }
  if (!is.logical(return_fitted) || length(return_fitted) != 1L || is.na(return_fitted)) {
    stop("`return_fitted` must be TRUE or FALSE.", call. = FALSE)
  }
  o <- order(time)
  reordered <- !identical(o, seq_along(time))
  if (reordered) {
    time <- time[o]
    exposure <- exposure[o]
    x <- x[, o, drop = FALSE]
  }
  if (method == "pretsa") {
    ranges <- row_ranges(x)
    if (length(unique(time)) < 2L) stop("Spline fitting needs distinct coordinates.", call. = FALSE)
    valid_count <- function(z) {
      is.numeric(z) && length(z) == 1L &&
        is.finite(z) && z >= 0 && z == floor(z)
    }
    automatic <- identical(knot, "auto")
    if ((!automatic && !valid_count(knot)) || !valid_count(max_knot_allowed)) {
      stop("Knot counts must be nonnegative integers, or knot = 'auto'.", call. = FALSE)
    }
    candidates <- if (automatic) 0:max_knot_allowed else knot
    fitted <- matrix(0, nrow(x), ncol(x), dimnames = dimnames(x))
    best <- rep(Inf, nrow(x))
    df1 <- df2 <- numeric(nrow(x))
    for (k in candidates) {
      B <- splines::bs(time, intercept = FALSE, df = k + 3)
      B <- cbind(1, B[, apply(B, 2L, stats::sd) > 0, drop = FALSE])
      inverse <- tryCatch(chol2inv(chol(crossprod(B))), error = function(e) NULL)
      if (is.null(inverse) || ncol(B) >= ncol(x)) {
        if (automatic && k > 0) break
        stop("Insufficient independent coordinates for spline fitting.", call. = FALSE)
      }
      pred <- x %*% B %*% inverse %*% t(B)
      if (!automatic) {
        fitted <- pred
        df1[] <- ncol(B) - 1
        df2[] <- ncol(x) - ncol(B)
        break
      }
      mse <- rowMeans((x - pred)^2)
      bic <- ncol(x) * (1 + log(2 * pi) + log(mse)) + log(ncol(x)) * (ncol(B) + 1)
      keep <- if (k == candidates[1]) rep(TRUE, nrow(x)) else bic < best
      fitted[keep, ] <- pred[keep, , drop = FALSE]
      best[keep] <- bic[keep]
      df1[keep] <- ncol(B) - 1
      df2[keep] <- ncol(x) - ncol(B)
    }
    dimnames(fitted) <- dimnames(x)
    SSE <- rowSums((x - fitted)^2)
    SST <- rowSums(sweep(x, 1L, rowMeans(x), "-")^2)
    fstat <- ((SST - SSE) / df1) / (SSE / df2)
    fstat[rowSums(x) == 0] <- 0
    pvalue <- stats::pf(fstat, df1, df2, lower.tail = FALSE)
    rsq <- 1 - SSE / SST
    rsq[!is.finite(rsq) | rsq < 0] <- 0
    hi <- ranges$maximum
    lo <- ranges$minimum
    flat <- (hi - lo) <= sqrt(.Machine$double.eps) * pmax(1, abs(hi), abs(lo))
    if (any(flat)) fitted[flat, ] <- rowMeans(x[flat, , drop = FALSE])
    summary <- summarize_curves(fitted, x, time, TRUE)
    statistics <- data.frame(
      features = rownames(x), n_above_min = summary$n_above_min,
      r.sq = rsq, dev.expl = rsq, peaktime = summary$peak,
      valleytime = summary$valley, pvalue = pvalue, row.names = rownames(x)
    )
    lower <- upper <- fitted
  } else {
    if (is.null(reference_exposure)) {
      reference_exposure <- stats::median(exposure[is.finite(exposure) & exposure > 0])
    }
    if (length(reference_exposure) != 1L || !is.finite(reference_exposure) || reference_exposure <= 0) {
      stop("`reference_exposure` must be positive and finite.", call. = FALSE)
    }
    fits <- parallelize_fun(seq_len(nrow(x)), function(i) {
      y <- as.numeric(x[i, ])
      f <- family[i]
      minimum <- suppressWarnings(min(y, na.rm = TRUE))
      if (is.finite(minimum) && minimum < 0 && f %in% c("nb", "poisson", "binomial")) f <- "gaussian"
      e <- if (use_exposure[i] && f != "gaussian") exposure else rep(reference_exposure, ncol(x))
      valid <- is.finite(y) & is.finite(e) & e > 0
      if (sum(valid) < 4L || length(unique(time[valid])) < 3L) stop("Insufficient finite observations.")
      s <- mgcv::s
      mod <- mgcv::gam(y ~ s(t, bs = "cs") + offset(log(e)),
        family = f,
        data = data.frame(y = y[valid], t = time[valid], e = e[valid])
      )
      pre <- stats::predict(mod, newdata = data.frame(t = time, e = e), type = "link", se.fit = TRUE)
      scale <- reference_exposure / e
      fitted <- mod$family$linkinv(pre$fit) * scale
      res <- summary(mod)
      list(
        fitted = fitted, lower = mod$family$linkinv(pre$fit - 2 * pre$se.fit) * scale,
        upper = mod$family$linkinv(pre$fit + 2 * pre$se.fit) * scale,
        pvalue = res$s.table[[4]], rsq = max(0, min(1, res$r.sq)), dev = res$dev.expl
      )
    }, cores = cores, verbose = verbose)
    failed <- vapply(fits, inherits, logical(1), "parallelize_error")
    if (any(failed)) {
      if (verbose) warning("Failed fits retained as NA: ", paste(rownames(x)[failed], collapse = ", "), call. = FALSE)
      fits[failed] <- rep(list(list(
        fitted = rep(NA_real_, ncol(x)),
        lower = rep(NA_real_, ncol(x)), upper = rep(NA_real_, ncol(x)),
        pvalue = NA_real_, rsq = NA_real_, dev = NA_real_
      )), sum(failed))
    }
    collect <- function(key) {
      z <- do.call(rbind, lapply(fits, `[[`, key))
      dimnames(z) <- dimnames(x)
      z
    }
    fitted <- collect("fitted")
    lower <- collect("lower")
    upper <- collect("upper")
    summary <- summarize_curves(fitted, x, time, FALSE)
    statistics <- data.frame(
      features = rownames(x), n_above_min = summary$n_above_min,
      r.sq = vapply(fits, `[[`, numeric(1), "rsq"), dev.expl = vapply(fits, `[[`, numeric(1), "dev"),
      peaktime = summary$peak, valleytime = summary$valley,
      pvalue = vapply(fits, `[[`, numeric(1), "pvalue"), row.names = rownames(x)
    )
  }
  statistics$padjust <- stats::p.adjust(statistics$pvalue, method = padjust_method)
  out <- list(statistics = statistics, time = stats::setNames(time, colnames(x)))
  if (return_fitted) {
    out$fitted <- fitted
    out$lower <- lower
    out$upper <- upper
  }
  if (reordered) {
    restore <- order(o)
    out$time <- out$time[restore]
    if (return_fitted) {
      out$fitted <- fitted[, restore, drop = FALSE]
      out$lower <- lower[, restore, drop = FALSE]
      out$upper <- upper[, restore, drop = FALSE]
    }
  }
  out
}
