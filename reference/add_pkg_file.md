# Add package file

Automatically generate a file containing functions and related code for
R package development.

## Usage

``` r
add_pkg_file(
  use_figlet = TRUE,
  figlet_font = "Slant",
  colors = c("red", "yellow", "green", "magenta", "cyan", "yellow", "green", "white",
    "magenta", "cyan"),
  unicode = TRUE,
  verbose = TRUE
)
```

## Arguments

- use_figlet:

  Whether to use figlet for ASCII art generation. Default is `TRUE`.
  Details see
  [figlet](https://mengxu98.github.io/thisutils/reference/figlet.md).

- figlet_font:

  Character string, figlet font to use. Default is `"Slant"`.

- colors:

  Character vector, colors to use for the logo elements.

- unicode:

  Whether to use Unicode symbols. Default is `TRUE`.

- verbose:

  Whether to print progress messages. Default is `TRUE`.

## Value

Creates a file in specified output directory.
