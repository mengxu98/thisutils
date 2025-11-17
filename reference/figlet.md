# The figlet function

Create ASCII art text using figlet.

## Usage

``` r
figlet(
  text,
  font = "Slant",
  width = getOption("width", 80),
  justify = "left",
  absolute = FALSE,
  strip = TRUE
)
```

## Arguments

- text:

  Text to make bigger.

- font:

  Name of font, path to font, or `figlet_font` object.

- width:

  Width to use when justifying and breaking lines.

- justify:

  Text justification to use in rendering ("left", "centre", "right").

- absolute:

  Logical, indicating if alignment is absolute.

- strip:

  Logical, indicating if whitespace should be removed.

## Value

An object of class `figlet_text` which is a character vector with a
handy print method.

## References

<https://github.com/richfitz/rfiglet>,
<https://github.com/jbkunst/figletr>, <http://www.figlet.org/>

## Examples

``` r
figlet("thisutils")
#>    __  __    _              __  _  __
#>   / /_/ /_  (_)_____ __  __/ /_(_)/ /_____
#>  / __/ __ ./ // ___// / / / __/ // // ___/
#> / /_/ / / / /(__  )/ /_/ / /_/ // /(__  )
#> .__/_/ /_/_//____/ .__,_/.__/_//_//____/
```
