test_that("dynamic fitting aligns coordinates and restores observation order", {
  set.seed(42)
  time <- setNames(seq(0, 1, length.out = 60), paste0("o", 1:60))
  x <- rbind(a = sin(time * 5) + rnorm(60, sd = .1), b = rnorm(60), flat = 2, zero = 0)
  for (method in c("gam", "pretsa")) {
    reference <- fit_trends(x, time, method = method, verbose = FALSE)
    shuffled <- sample(60)
    other <- fit_trends(x[, shuffled], time, method = method, verbose = FALSE)
    expect_equal(other$statistics, reference$statistics, tolerance = 1e-10)
    expect_equal(other$fitted, reference$fitted[, shuffled], tolerance = 1e-10)
    expect_equal(other$time, time[shuffled])
  }
  out <- fit_trends(x, time, method = "pretsa", knot = "auto")
  expect_equal(unname(out$fitted["flat", ]), rep(2, 60))
  expect_equal(out$statistics["flat", "peaktime"], median(time))
  expect_identical(out$statistics["zero", "pvalue"], 1)
  expect_equal(out$statistics$padjust, p.adjust(out$statistics$pvalue, "fdr"))
  expect_null(fit_trends(x, time, method = "pretsa", return_fitted = FALSE)$fitted)
  expect_error(fit_trends(x, time[-1]), "length")
  expect_error(fit_trends(x, rep(0, 60), method = "pretsa"))
})

test_that("GAM retains failed feature slots and aligns exposure", {
  time <- setNames(seq(0, 1, length.out = 40), paste0("o", 1:40))
  x <- rbind(good = sin(time * 3), partial = cos(time * 3), bad = NA_real_)
  x["partial", c(3, 9)] <- NA_real_
  a <- fit_trends(x, time, method = "gam", verbose = FALSE)
  expect_true(all(is.na(a$fitted["bad", ])))
  expect_true(is.na(a$statistics["bad", "pvalue"]))
  expect_true(all(is.finite(a$fitted["partial", ])))
  expect_equal(rownames(a$statistics), rownames(x))
  set.seed(1)
  x <- matrix(rpois(80, 5), 2, dimnames = list(c("a", "b"), names(time)))
  exposure <- setNames(seq_len(40) + 10, names(time))
  a <- fit_trends(x, time, family = "poisson", exposure = exposure, use_exposure = TRUE, verbose = FALSE)
  b <- fit_trends(x, time, family = "poisson", exposure = rev(exposure), use_exposure = TRUE, verbose = FALSE)
  expect_equal(a, b)
  parallel <- fit_trends(x, time,
    family = "poisson", exposure = exposure,
    use_exposure = TRUE, cores = 2, verbose = FALSE
  )
  expect_equal(parallel, a, tolerance = 1e-10)
})

test_that("fixed spline statistics agree with the explicit regression", {
  set.seed(19)
  time <- seq(0, 1, length.out = 70)
  x <- rbind(a = sin(6 * time) + rnorm(70, sd = .2), b = rnorm(70))
  B <- cbind(1, splines::bs(time, df = 3, intercept = FALSE))
  fitted <- x %*% B %*% chol2inv(chol(crossprod(B))) %*% t(B)
  sse <- rowSums((x - fitted)^2)
  sst <- rowSums(sweep(x, 1, rowMeans(x), "-")^2)
  p <- pf(((sst - sse) / 3) / (sse / (70 - 4)), 3, 70 - 4, lower.tail = FALSE)
  out <- fit_trends(x, time, method = "pretsa")
  expect_equal(unname(out$fitted), unname(fitted), tolerance = 1e-12)
  expect_equal(out$statistics$pvalue, unname(p), tolerance = 1e-12)
})

test_that("curve quantiles preserve ties and missing-value behavior", {
  set.seed(29)
  for (n in c(1, 2, 3, 99, 100, 101, 1000)) {
    time <- seq_len(n)
    values <- matrix(sample(c(0, 0, 1, 2, 3, NA), 6 * n, replace = TRUE), 6)
    fitted <- matrix(rnorm(6 * n), 6)
    fitted[1, ] <- 1
    fitted[2, ] <- NA_real_
    if (n > 2) fitted[3, 1] <- NA_real_
    for (inclusive in c(TRUE, FALSE)) {
      out <- summarize_curves(fitted, values, time, inclusive)
      peak <- apply(fitted, 1, function(v) {
        q <- quantile(v, .99, na.rm = TRUE)
        median(time[if (inclusive) v >= q else v > q])
      })
      valley <- apply(fitted, 1, function(v) {
        q <- quantile(v, .01, na.rm = TRUE)
        median(time[if (inclusive) v <= q else v < q])
      })
      expect_equal(out$peak, peak)
      expect_equal(out$valley, valley)
    }
  }
  for (value in c(NA_real_, NaN, Inf, -Inf)) {
    expect_error(fit_trends(matrix(value, 2, 20), 1:20, method = "pretsa"), "finite")
  }
})
