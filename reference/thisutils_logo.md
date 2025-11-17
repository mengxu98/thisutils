# The logo of thisutils

The thisutils logo, using ASCII or Unicode characters Use
[cli::ansi_strip](https://cli.r-lib.org/reference/ansi_strip.html) to
get rid of the colors.

## Usage

``` r
thisutils_logo(unicode = cli::is_utf8_output())
```

## Arguments

- unicode:

  Unicode symbols on UTF-8 platforms. Default is
  [cli::is_utf8_output](https://cli.r-lib.org/reference/is_utf8_output.html).

## References

<https://github.com/tidyverse/tidyverse/blob/main/R/logo.R>

## Examples

``` r
thisutils_logo()
#>           ⬢          .        ⬡             ⬢     .
#>             __  __    _              __  _  __
#>            / /_/ /_  (_)_____ __  __/ /_(_)/ /_____
#>           / __/ __ ./ // ___// / / / __/ // // ___/
#>          / /_/ / / / /(__  )/ /_/ / /_/ // /(__  )
#>          .__/_/ /_/_//____/ .__,_/.__/_//_//____/
#>       ⬡               ⬢      .        ⬡          ⬢
```
