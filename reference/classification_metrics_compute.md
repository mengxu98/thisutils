# Compute classification metrics

Compute multi-class classification metrics from predicted and true
labels, including accuracy, macro-F1, purity, NMI, ARI, and rare-class
recall.

## Usage

``` r
classification_metrics_compute(predicted, truth, rare_threshold = 0.05)
```

## Arguments

- predicted:

  Character vector of predicted class labels.

- truth:

  Character vector of true class labels (same length as `predicted`).

- rare_threshold:

  Proportion threshold for rare-class recall. Classes with support
  proportion \\\<=\\ this value contribute to the `rare_recall` metric.
  Default is `0.05`.

## Value

A list with the following components:

- accuracy:

  Overall accuracy (scalar).

- macro_f1:

  Macro-averaged F1 score (scalar).

- purity:

  Cluster purity (scalar).

- nmi:

  Normalized Mutual Information (scalar).

- ari:

  Adjusted Rand Index (scalar).

- rare_recall:

  Mean recall on rare classes (scalar, or `NA` if no rare classes).

- class_table:

  A data frame of per-class precision, recall, F1, and support.

## Examples

``` r
predicted <- c("A", "A", "B", "B", "C")
truth <- c("A", "B", "B", "B", "C")
classification_metrics_compute(predicted, truth)
#> $accuracy
#> [1] 0.8
#> 
#> $macro_f1
#> [1] 0.8222222
#> 
#> $purity
#> [1] 0.8
#> 
#> $nmi
#> [1] 0.6712695
#> 
#> $ari
#> [1] 0.2105263
#> 
#> $rare_recall
#> [1] NA
#> 
#> $class_table
#>   class precision    recall        f1 support
#> 1     A       0.5 1.0000000 0.6666667       1
#> 2     B       1.0 0.6666667 0.8000000       3
#> 3     C       1.0 1.0000000 1.0000000       1
#> 
```
