#!/usr/bin/env python3

import os
import sys

from log_message import log_message, style_formatting

# basic usage
log_message("Hello, ", "world!")
log_message("hello, world!")
log_message("Hello, world!", timestamp=False)
log_message("Hello, ", "world!", message_type="success")
log_message("Hello, world!", message_type="warning")
log_message("Processing data...", message_type="running")
log_message("Hello, ", "world!", cli_model=False)

# suppress messages
log_message("Hello, world!", verbose=False)
os.environ["LOG_MESSAGE_VERBOSE"] = "false"
log_message("Hello, world!")
os.environ["LOG_MESSAGE_VERBOSE"] = "true"
log_message("Hello, world!", verbose=False)
os.environ.pop("LOG_MESSAGE_VERBOSE", None)

# cli inline markup
log_message("{.arg abc} is an argument")
log_message("{.val list('abc')} is a {.cls {class(list('abc'))}}")
log_message("{.code print(\"hello\")} is a code example")
log_message("{.dt List}list('abc')")
log_message("address: {.email example@example.com}")
log_message("{.emph Python} is a programming language")
log_message("{.envvar PYTHONPATH}")
log_message("{.file log_message.py} is a file")
log_message("{.fn print} is a function")
log_message("{.fun print} is a function")
log_message("Use {.code help(print)} to get help")
log_message("... see {.code python3 -m pydoc builtins.print} to learn more")
log_message("See the {.href [cli homepage](https://cli.r-lib.org)} for details")
log_message("press {.kbd ENTER}")
log_message("press {.key ENTER}")
log_message("URL: {.url https://cli.r-lib.org}")
log_message("Some {.field field}")
log_message("{.path /usr/bin/python3} is a path")
log_message("{.pkg rich} is a package")
log_message("{.val object} is a variable")
log_message("{.run python3 scripts/log_message.py} is a runnable file")
log_message("{.str object} is a string")
log_message("{.strong abc} is a strong string")
log_message("{.topic print} is a Python help target")

# set indentation
log_message("Hello, world!", level=2)
log_message("Hello, world!", symbol="->")
log_message("Hello, world!", symbol="#####", level=3)

# color formatting
log_message("This is a red message", text_color="#ff9900")
log_message("This is a message with background", back_color="#EE4000")
log_message(
    "This is a message with both text and background",
    text_color="white",
    back_color="cyan",
)
log_message("This is a message with background", back_color="#EE4000", cli_model=False)
log_message(
    "This is a message with both text and background",
    text_color="red",
    back_color="cyan",
    cli_model=False,
)
log_message(
    "Hex color with {.arg cli_model = FALSE}", text_color="#FF5733", cli_model=False
)
log_message("Bright red text", text_color="br_red")
log_message("Bright background", back_color="br_yellow")
log_message("Combined grey and style", text_color="grey", text_style=["bold"])

# text style formatting
log_message("Bold message", text_style=["bold"])
log_message("Italic message", text_style=["italic"])
log_message("Underlined message", text_style=["underline"])
log_message("Combined styles", text_style=["bold", "underline"])
log_message("Color and style", text_color="blue", text_style=["bold", "italic"])
log_message(
    "Hex color and style", text_color="#FF5733", text_style=["bold", "underline"]
)

# multiline message
log_message("Line 1\nLine 2\nLine 3", multiline_indent=True, text_style=["italic"])
log_message("Multi-line\ncolored\nmessage", text_color="blue", text_style=["italic"])
log_message("Multi-line\ncolored\nmessage", text_color="blue", timestamp=False)

# timestamp styling
log_message(
    "Multi-line message\nwith timestamp styling",
    text_color="red",
    text_style=["bold"],
    timestamp_style=True,
)
log_message(
    "Multi-line message\nwithout timestamp styling",
    text_color="#669999",
    text_style=["bold", "italic"],
)

# combine style and log_message (green text + blue highlighted substring)
part1 = style_formatting("I am a green line ", "green", None, None)
part2 = style_formatting("with a blue substring", "blue", None, ["bold", "underline"])
part3 = style_formatting(" that becomes green again!", "green", None, None)
log_message(part1, part2, part3)

# print objects directly
df_text = "  x y  z\n1 1 a  a\n2 2 b b  \n3 3 c  c"
log_message("Content:\n", df_text)

# interactive prompt
if sys.stdin.isatty() and sys.stdout.isatty():
    answer = log_message("Do you want to continue?", message_type="ask")
    print("ask result:", answer)
