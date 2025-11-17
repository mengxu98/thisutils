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
#>  [1]  3 10 13 37 38 51 69 71 82 94
#> 
#> [[2]]
#> [1]  1 48 74 75 81 83 97
#> 
#> [[3]]
#>  [1]   9  14  21  31  33  34  39  47  53  56  66  91  96  99 100
#> 
#> [[4]]
#> [1] 32 41 42 60 77 79 92
#> 
#> [[5]]
#>  [1]  5  7 16 29 45 55 58 61 73 86 88
#> 
#> [[6]]
#>  [1]  2 11 18 23 35 43 54 59 70 84 95
#> 
#> [[7]]
#>  [1] 12 15 17 26 27 52 68 72 87 89 98
#> 
#> [[8]]
#>  [1]  4  6  8 24 40 49 50 76 78 90 93
#> 
#> [[9]]
#> [1] 20 25 28 36 44 62 64 80
#> 
#> [[10]]
#> [1] 19 22 30 46 57 63 65 67 85
#> 
split_indices(sample(10, 100, rep = TRUE), 10)
#> [[1]]
#>  [1] 20 25 28 29 32 43 44 77 84 98
#> 
#> [[2]]
#>  [1]  1  6 10 18 23 33 38 46 49 50 66 69 80 82
#> 
#> [[3]]
#>  [1]  4 11 15 16 31 37 41 45 87 93
#> 
#> [[4]]
#>  [1]   3   8   9  21  26  30  42  88  90  95  97  99 100
#> 
#> [[5]]
#>  [1] 13 19 27 36 39 40 57 61 62 70 78 86
#> 
#> [[6]]
#> [1] 17 22 24 34 64 75 76
#> 
#> [[7]]
#> [1]  5 12 51 53 58 63 85
#> 
#> [[8]]
#>  [1] 14 47 48 52 55 60 71 89 91 96
#> 
#> [[9]]
#>  [1]  2 35 54 67 68 73 79 81 83 92
#> 
#> [[10]]
#> [1]  7 56 59 65 72 74 94
#> 
```
