# Coefficient of determination (\\R^2\\)

Coefficient of determination (\\R^2\\)

## Usage

``` r
r_square(y_true, y_pred)
```

## Arguments

- y_true:

  A numeric vector with ground truth values.

- y_pred:

  A numeric vector with predicted values.

## Value

The \\R^2\\ value.

## Examples

``` r
y <- rnorm(100)
y_pred <- y + rnorm(100, sd = 0.5)
r_square(y, y_pred)
#> [1] 0.7778111
```
