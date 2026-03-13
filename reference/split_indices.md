# Split indices.

An optimised version of split for the special case of splitting row
indices into groups.

## Usage

``` r
split_indices(group, n = 0L)
```

## Arguments

- group:

  Integer indices

- n:

  The largest integer (may not appear in index). This is hint: if the
  largest value of `group` is bigger than `n`, the output will silently
  expand.

## Value

A list of vectors of indices.

## References

<https://github.com/hadley/plyr/blob/d57f9377eb5d56107ba3136775f2f0f005f33aa3/src/split-numeric.cpp#L20>

## Examples

``` r
split_indices(sample(10, 100, rep = TRUE))
#> [[1]]
#>  [1]  4 11 14 38 39 52 70 72 83 95
#> 
#> [[2]]
#> [1]  2 49 75 76 82 84 98
#> 
#> [[3]]
#>  [1]  10  15  22  32  34  35  40  48  54  57  67  92  97 100
#> 
#> [[4]]
#> [1] 33 42 43 61 78 80 93
#> 
#> [[5]]
#>  [1]  6  8 17 30 46 56 59 62 74 87 89
#> 
#> [[6]]
#>  [1]  3 12 19 24 36 44 55 60 71 85 96
#> 
#> [[7]]
#>  [1]  1 13 16 18 27 28 53 69 73 88 90 99
#> 
#> [[8]]
#>  [1]  5  7  9 25 41 50 51 77 79 91 94
#> 
#> [[9]]
#> [1] 21 26 29 37 45 63 65 81
#> 
#> [[10]]
#> [1] 20 23 31 47 58 64 66 68 86
#> 
split_indices(sample(10, 100, rep = TRUE), 10)
#> [[1]]
#>  [1] 21 26 29 30 33 44 45 78 85 99
#> 
#> [[2]]
#>  [1]  2  7 11 19 24 34 39 47 50 51 67 70 81 83
#> 
#> [[3]]
#>  [1]  1  5 12 16 17 32 38 42 46 88 94
#> 
#> [[4]]
#>  [1]   4   9  10  22  27  31  43  89  91  96  98 100
#> 
#> [[5]]
#>  [1] 14 20 28 37 40 41 58 62 63 71 79 87
#> 
#> [[6]]
#> [1] 18 23 25 35 65 76 77
#> 
#> [[7]]
#> [1]  6 13 52 54 59 64 86
#> 
#> [[8]]
#>  [1] 15 48 49 53 56 61 72 90 92 97
#> 
#> [[9]]
#>  [1]  3 36 55 68 69 74 80 82 84 93
#> 
#> [[10]]
#> [1]  8 57 60 66 73 75 95
#> 
```
