# Print formatted message

Integrate the message printing function with the
[cli](https://cli.r-lib.org) package, and the
[base::message](https://rdrr.io/r/base/message.html) function. The
message could be suppressed by
[base::suppressMessages](https://rdrr.io/r/base/message.html).

## Usage

``` r
log_message(
  ...,
  verbose = TRUE,
  message_type = c("info", "success", "warning", "error", "running", "ask"),
  cli_model = TRUE,
  level = 1,
  symbol = "  ",
  text_color = NULL,
  back_color = NULL,
  text_style = NULL,
  multiline_indent = FALSE,
  timestamp = TRUE,
  timestamp_format = paste0("[", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "] "),
  timestamp_style = FALSE,
  plain_text = FALSE,
  .envir = parent.frame(),
  .frame = .envir
)
```

## Arguments

- ...:

  The message to print.

- verbose:

  Whether to print the message. Default is `TRUE`.

- message_type:

  Type of message. Could be choose one of `"info"`, `"success"`,
  `"warning"`, `"error"`, `"running"`, and `"ask"`. When `"ask"` is
  used, the function will prompt the user for a Yes/No/Cancel response
  using [utils::askYesNo](https://rdrr.io/r/utils/askYesNo.html), and
  returns `TRUE` for Yes, `FALSE` for No, and `NA` for Cancel. Default
  is `"info"`.

- cli_model:

  Whether to use the `cli` package to print the message. Default is
  `TRUE`.

- level:

  The level of the message, which affects the indentation. Level `1` has
  no indentation, higher levels add more indentation. Default is `1`.

- symbol:

  The symbol used for indentation. When specified, it ignores the level
  parameter and uses the symbol directly. Default is `" "` (two spaces).

- text_color:

  Color for the message text. Supports R color names (e.g., `"orange"`),
  hexadecimal colors (e.g., `"#000000"`), basic colors: `"red"`,
  `"green"`, `"blue"`, `"yellow"`, `"magenta"`, `"cyan"`, `"white"`,
  `"black"`, `"grey"`, `"silver"`, `"none"`, and bright colors:
  `"br_red"`, `"br_green"`, `"br_blue"`, `"br_yellow"`, `"br_magenta"`,
  `"br_cyan"`, `"br_white"`, `"br_black"`. Default is `NULL`.

- back_color:

  Background color for the message text. Details see parameter
  `text_color`. Default is `NULL`.

- text_style:

  Text styles to apply. Can be one or more of: `"bold"`, `"italic"`,
  `"underline"`, `"strikethrough"`, `"dim"`, `"inverse"`. Multiple
  styles can be combined (e.g., `c("bold", "underline")`). Default is
  `NULL`.

- multiline_indent:

  Whether to apply consistent formatting (timestamp and indentation) to
  each line in multiline messages. When `TRUE`, each line gets the full
  formatting; when `FALSE`, only the first line gets the timestamp.
  Default is `FALSE`.

- timestamp:

  Whether to show the current time in the message. Default is `TRUE`.

- timestamp_format:

  Format string for timestamp display. Default is `"%Y-%m-%d %H:%M:%S"`.

- timestamp_style:

  Whether to apply the same text styling to the timestamp as the message
  text. When `TRUE`, timestamp formatting matches the message; when
  `FALSE`, timestamp keeps its default appearance. Default is `FALSE`.

- plain_text:

  Whether to print only the text content. When `TRUE`, level, symbol,
  timestamp, and message type formatting are suppressed, but color and
  multiline settings still apply.

- .envir:

  The environment to evaluate calls in. Default is
  [parent.frame](https://rdrr.io/r/base/sys.parent.html).

- .frame:

  The frame to use for error reporting. Default is `.envir`.

## Value

Formated message, or a logical value (`TRUE`/`FALSE`/`NA`) if
`message_type = "ask"`.

## References

<https://cli.r-lib.org/articles/index.html>

## Examples

``` r
# basic usage
log_message("Hello, ", "world!")
#> ℹ [2026-02-19 06:13:32] Hello, world!

log_message("hello, world!")
#> ℹ [2026-02-19 06:13:32] Hello, world!

log_message("Hello, world!", timestamp = FALSE)
#> ℹ Hello, world!

log_message(
  "Hello, ", "world!",
  message_type = "success"
)
#> ✔ [2026-02-19 06:13:32] Hello, world!

log_message(
  "Hello, world!",
  message_type = "warning"
)
#> ! [2026-02-19 06:13:32] Hello, world!

log_message(
  "Processing data...",
  message_type = "running"
)
#> ◌ [2026-02-19 06:13:32] Processing data...

log_message(
  "Hello, ", "world!",
  cli_model = FALSE
)
#> Hello, world!


# suppress messages
suppressMessages(log_message("Hello, world!"))
log_message("Hello, world!", verbose = FALSE)
options(log_message.verbose = FALSE)
log_message("Hello, world!")

# for global verbose option
options(log_message.verbose = TRUE)
log_message("Hello, world!", verbose = FALSE)
#> ℹ [2026-02-19 06:13:32] Hello, world!
options(log_message.verbose = NULL)


# cli inline markup
log_message("{.arg abc} is a argument")
#> ℹ [2026-02-19 06:13:32] `abc` is a argument

## 'message' can not deal with cli inline markup
message("hello, {.code world}!")
#> hello, {.code world}!

log_message("{.val list('abc')} is a {.cls {class(list('abc'))}}")
#> ℹ [2026-02-19 06:13:32] "list('abc')" is a <list>

log_message("{.code lm(y ~ x)} is a code example")
#> ℹ [2026-02-19 06:13:32] `lm(y ~ x)` is a code example

log_message("{.dt List}list('abc')")
#> ℹ [2026-02-19 06:13:32] List: list('abc')

log_message("address: {.email example@example.com}")
#> ℹ [2026-02-19 06:13:32] Address: example@example.com

log_message("{.emph R} is a programming language")
#> ℹ [2026-02-19 06:13:32] R is a programming language

log_message("{.envvar R_HOME}")
#> ℹ [2026-02-19 06:13:32] `R_HOME`

log_message("{.file log_message.R} is a file")
#> ℹ [2026-02-19 06:13:32] log_message.R is a file

log_message("{.fn lm} is a function")
#> ℹ [2026-02-19 06:13:32] `lm()` is a function

log_message("{.fun lm} is a function")
#> ℹ [2026-02-19 06:13:32] `lm()` is a function

log_message("{.help lm} to get help")
#> ℹ [2026-02-19 06:13:32] `?lm()` to get help

log_message("... see {.help [{.fun lm}](stats::lm)} to learn more")
#> ℹ [2026-02-19 06:13:32] ... see `lm()` (`?stats::lm()`) to learn more

log_message(
  "See the {.href [cli homepage](https://cli.r-lib.org)} for details"
)
#> ℹ [2026-02-19 06:13:32] See the cli homepage (<https://cli.r-lib.org>) for details

log_message("press {.kbd ENTER}")
#> ℹ [2026-02-19 06:13:32] Press [ENTER]

log_message("press {.key ENTER}")
#> ℹ [2026-02-19 06:13:32] Press [ENTER]

log_message("URL: {.url https://cli.r-lib.org}")
#> ℹ [2026-02-19 06:13:32] URL: <https://cli.r-lib.org>

log_message("Some {.field field}")
#> ℹ [2026-02-19 06:13:32] Some field

log_message("{.path /usr/bin/R} is a path")
#> ℹ [2026-02-19 06:13:32] /usr/bin/R is a path

log_message("{.pkg cli} is a package")
#> ℹ [2026-02-19 06:13:32] cli is a package

log_message("{.val object} is a variable")
#> ℹ [2026-02-19 06:13:32] "object" is a variable

log_message("{.run Rscript log_message.R} is a runnable file")
#> ℹ [2026-02-19 06:13:32] `Rscript log_message.R` is a runnable file

log_message("{.str object} is a string")
#> ℹ [2026-02-19 06:13:32] "object" is a string

log_message("{.strong abc} is a strong string")
#> ℹ [2026-02-19 06:13:32] abc is a strong string

log_message("{.topic stats::lm} is a topic")
#> ℹ [2026-02-19 06:13:32] `?stats::lm` is a topic

log_message("{.vignette cli} is a vignette")
#> ℹ [2026-02-19 06:13:32] `vignette(cli)` is a vignette


# set indentation
log_message("Hello, world!", level = 2)
#> ℹ [2026-02-19 06:13:32]   Hello, world!

log_message("Hello, world!", symbol = "->")
#> ℹ [2026-02-19 06:13:32] -> Hello, world!

log_message(
  "Hello, world!",
  symbol = "#####",
  level = 3
)
#> ℹ [2026-02-19 06:13:32] ############### Hello, world!

# color formatting
log_message(
  "This is a red message",
  text_color = "#ff9900"
)
#> ℹ [2026-02-19 06:13:32] This is a red message

log_message(
  "This is a message with background",
  back_color = "#EE4000"
)
#> ℹ [2026-02-19 06:13:32] This is a message with background

log_message(
  "This is a message with both text and background",
  text_color = "white",
  back_color = "cyan"
)
#> ℹ [2026-02-19 06:13:32] This is a message with both text and background

log_message(
  "This is a message with background",
  back_color = "#EE4000",
  cli_model = FALSE
)
#> This is a message with background

log_message(
  "This is a message with both text and background",
  text_color = "red",
  back_color = "cyan",
  cli_model = FALSE
)
#> This is a message with both text and background

log_message(
  "Hex color with {.arg cli_model = FALSE}",
  text_color = "#FF5733",
  cli_model = FALSE
)
#> Hex color with `cli_model = FALSE`

log_message(
  "Bright red text",
  text_color = "br_red"
)
#> ℹ [2026-02-19 06:13:32] Bright red text

log_message(
  "Bright background",
  back_color = "br_yellow"
)
#> ℹ [2026-02-19 06:13:32] Bright background

log_message(
  "Combined grey and style",
  text_color = "grey",
  text_style = "bold"
)
#> ℹ [2026-02-19 06:13:32] Combined grey and style

# text style formatting
log_message(
  "Bold message",
  text_style = "bold"
)
#> ℹ [2026-02-19 06:13:32] Bold message

log_message(
  "Italic message",
  text_style = "italic"
)
#> ℹ [2026-02-19 06:13:32] Italic message

log_message(
  "Underlined message",
  text_style = "underline"
)
#> ℹ [2026-02-19 06:13:32] Underlined message

log_message(
  "Combined styles",
  text_style = c("bold", "underline")
)
#> ℹ [2026-02-19 06:13:32] Combined styles

log_message(
  "Color and style",
  text_color = "blue",
  text_style = c("bold", "italic")
)
#> ℹ [2026-02-19 06:13:32] Color and style

log_message(
  "Hex color and style",
  text_color = "#FF5733",
  text_style = c("bold", "underline")
)
#> ℹ [2026-02-19 06:13:32] Hex color and style


# multiline message
log_message(
  "Line 1\nLine 2\nLine 3",
  multiline_indent = TRUE,
  text_style = "italic"
)
#> ℹ [2026-02-19 06:13:32] Line 1
#> ℹ [2026-02-19 06:13:32] Line 2
#> ℹ [2026-02-19 06:13:32] Line 3

log_message(
  "Multi-line\ncolored\nmessage",
  text_color = "blue",
  text_style = "italic"
)
#> ℹ [2026-02-19 06:13:32] Multi-line
#> ℹ                       colored
#> ℹ                       message

log_message(
  "Multi-line\ncolored\nmessage",
  text_color = "blue",
  timestamp = FALSE
)
#> ℹ Multi-line
#> ℹ colored
#> ℹ message

# timestamp styling
log_message(
  "Multi-line message\nwith timestamp styling",
  text_color = "red",
  text_style = "bold",
  timestamp_style = TRUE
)
#> ℹ [2026-02-19 06:13:32] Multi-line message
#> ℹ                       with timestamp styling

log_message(
  "Multi-line message\nwithout timestamp styling",
  text_color = "#669999",
  text_style = c("bold", "italic")
)
#> ℹ [2026-02-19 06:13:32] Multi-line message
#> ℹ                       without timestamp styling


# combine cli package and log_message
log_message(
  cli::col_green(
    "I am a green line ",
    cli::col_blue(
      cli::style_underline(
        cli::style_bold("with a blue substring")
      )
    ),
    " that becomes green again!"
  )
)
#> ℹ [2026-02-19 06:13:32] I am a green line with a blue substring that becomes green again!

# cli variables
fun <- function(x = 1) {
  log_message("{.val x}")
  log_message("{.val {x}}")
  log_message("{.val {x + 1}}")
}
fun()
#> ℹ [2026-02-19 06:13:32] "x"
#> ℹ [2026-02-19 06:13:33] 1
#> ℹ [2026-02-19 06:13:33] 2


# print objects directly
df <- data.frame(
  x = 1:3,
  y = letters[1:3],
  z = c(" a", "b  ", "c")
)
log_message("Content:\n", df)
#> ℹ [2026-02-19 06:13:33] Content:
#> ℹ                         x y   z
#> ℹ                       1 1 a   a
#> ℹ                       2 2 b b  
#> ℹ                       3 3 c   c

# interactive prompt
if (interactive()) {
  log_message(
    "Do you want to continue?",
    message_type = "ask"
  )
}
```
