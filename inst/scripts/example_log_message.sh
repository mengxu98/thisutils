#!/usr/bin/env bash

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

source "$SCRIPT_DIR/log_message.sh"

# basic usage
log_message "Hello, " "world!"
log_message "hello, world!"
log_message "Hello, world!" --timestamp false
log_message "Hello, " "world!" --message-type success
log_message "Hello, world!" --message-type warning
log_message "Processing data..." --message-type running
log_message "Hello, " "world!" --cli-model false

# suppress messages
log_message "Hello, world!" --verbose false
LOG_MESSAGE_VERBOSE=false log_message "Hello, world!"
LOG_MESSAGE_VERBOSE=true log_message "Hello, world!" --verbose false

# cli inline markup
log_message "{.arg abc} is an argument"
log_message "{.val list('abc')} is a {.cls {class(list('abc'))}}"
log_message "{.code echo hello} is a code example"
log_message "{.dt List}list('abc')"
log_message "address: {.email example@example.com}"
log_message "{.emph Bash} is a scripting language"
log_message "{.envvar SHELL}"
log_message "{.file log_message.sh} is a file"
log_message "{.fn echo} is a function"
log_message "{.fun echo} is a function"
log_message "Use {.help bash} to get help"
log_message "See the {.href [cli homepage](https://cli.r-lib.org)} for details"
log_message "press {.kbd ENTER}"
log_message "press {.key ENTER}"
log_message "URL: {.url https://cli.r-lib.org}"
log_message "Some {.field field}"
log_message "{.path /bin/bash} is a path"
log_message "{.pkg bash} is a package"
log_message "{.val object} is a variable"
log_message "{.run bash scripts/log_message.sh} is a runnable file"
log_message "{.str object} is a string"
log_message "{.strong abc} is a strong string"
log_message "{.topic echo} is a shell help target"

# set indentation
log_message "Hello, world!" --level 2
log_message "Hello, world!" --symbol "->"
log_message "Hello, world!" --symbol "#####" --level 3

# color formatting
log_message "This is a red message" --text-color "#ff9900"
log_message "This is a message with background" --back-color "#EE4000"
log_message "This is a message with both text and background" --text-color "white" --back-color "cyan"
log_message "This is a message with background" --back-color "#EE4000" --cli-model false
log_message "This is a message with both text and background" --text-color "red" --back-color "cyan" --cli-model false
log_message "Hex color with {.arg cli_model = FALSE}" --text-color "#FF5733" --cli-model false
log_message "Bright red text" --text-color "br_red"
log_message "Bright background" --back-color "br_yellow"
log_message "Combined grey and style" --text-color "grey" --text-style "bold"

# text style formatting
log_message "Bold message" --text-style "bold"
log_message "Italic message" --text-style "italic"
log_message "Underlined message" --text-style "underline"
log_message "Combined styles" --text-style "bold,underline"
log_message "Color and style" --text-color "blue" --text-style "bold,italic"
log_message "Hex color and style" --text-color "#FF5733" --text-style "bold,underline"

# multiline message
log_message $'Line 1\nLine 2\nLine 3' --multiline-indent --text-style "italic"
log_message $'Multi-line\ncolored\nmessage' --text-color "blue" --text-style "italic"
log_message $'Multi-line\ncolored\nmessage' --text-color "blue" --timestamp false

# timestamp styling
log_message $'Multi-line message\nwith timestamp styling' --text-color "red" --text-style "bold" --timestamp-style
log_message $'Multi-line message\nwithout timestamp styling' --text-color "#669999" --text-style "bold,italic"

# combine style and log_message (green text + blue highlighted substring)
part1="$(style_formatting 'I am a green line ' 'green' '' '')"
part2="$(style_formatting 'with a blue substring' 'blue' '' 'bold,underline')"
part3="$(style_formatting ' that becomes green again!' 'green' '' '')"
log_message "${part1}${part2}${part3}"

# print objects directly
df_text=$'  x y  z\n1 1 a  a\n2 2 b b  \n3 3 c  c'
log_message "Content:\n${df_text}"

# interactive prompt
if [[ -t 0 && -t 1 ]]; then
	log_message "Do you want to continue?" --message-type ask
fi
