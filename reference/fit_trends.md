# Fit feature trends

Fit feature trends

## Usage

``` r
fit_trends(
  x,
  time,
  method = c("gam", "pretsa"),
  family = "gaussian",
  exposure = rep(1, ncol(x)),
  use_exposure = FALSE,
  reference_exposure = NULL,
  knot = 0,
  max_knot_allowed = 10,
  padjust_method = "fdr",
  return_fitted = TRUE,
  cores = 1,
  verbose = TRUE
)
```

## Arguments

- x:

  Numeric matrix with features in rows and observations in columns.

- time:

  Finite numeric coordinate per observation. Named coordinates are
  aligned to column names; unnamed coordinates are positional.

- method:

  Fitting method: `"gam"` or `"pretsa"`.

- family:

  GAM distribution, scalar or one named value per feature.

- exposure:

  Positive observation exposures for GAM offsets.

- use_exposure:

  Logical scalar or feature vector selecting which non-Gaussian fits use
  observation exposures.

- reference_exposure:

  Positive scalar offset for other fits and reference scale for
  predictions. Defaults to the median positive finite exposure.

- knot:

  Nonnegative number of interior knots or `"auto"` for BIC selection.

- max_knot_allowed:

  Maximum interior knots in automatic selection.

- padjust_method:

  Method passed to
  [`stats::p.adjust()`](https://rdrr.io/r/stats/p.adjust.html).

- return_fitted:

  Include fitted, lower and upper matrices in the result.

- cores:

  Number of GAM workers.

- verbose:

  Report fitting failures and progress.

## Value

A list with `statistics`, `time`, and optional `fitted`, `lower`, and
`upper` matrices, in input order. Statistics contain feature names,
counts above each feature's minimum, fit scores, peak/valley times,
P-values and adjusted P-values. Failed GAM fits return NA. GAM bounds
use +/- 2 standard errors on the link scale; PreTSA bounds equal fitted
values, not confidence intervals.

## Details

Inputs are not transformed. PreTSA requires finite values; GAM omits
unusable observations. Peak/valley times summarize the upper/lower 1% of
fitted values, including ties for PreTSA.

## References

Zhuang and Ji, PreTSA, https://github.com/haotian-zhuang/PreTSA

## Examples

``` r
t <- seq(0, 1, length.out = 40)
x <- rbind(a = sin(t * 5), b = cos(t * 3))
fit_trends(x, t, method = "pretsa")$statistics
#>   features n_above_min      r.sq  dev.expl   peaktime valleytime       pvalue
#> a        a          39 0.9919829 0.9919829 0.28205128          1 9.113069e-38
#> b        b          39 0.9999850 0.9999850 0.02564103          1 7.272095e-87
#>        padjust
#> a 9.113069e-38
#> b 1.454419e-86
```
