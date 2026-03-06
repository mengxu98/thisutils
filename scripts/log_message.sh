#!/usr/bin/env bash

set -o nounset
set -o pipefail

is_hex_color() {
  [[ "$1" =~ ^#([0-9a-fA-F]{3}|[0-9a-fA-F]{6})$ ]]
}

check_color() {
  local color="$1"
  case "$color" in
    black|red|green|yellow|blue|magenta|cyan|white|grey|silver|none|br_black|br_red|br_green|br_yellow|br_blue|br_magenta|br_cyan|br_white)
      return 0
      ;;
  esac

  if is_hex_color "$color"; then
    return 0
  fi

  return 1
}

hex_to_rgb() {
  local hex="${1#\#}"
  if [[ ${#hex} -eq 3 ]]; then
    hex="${hex:0:1}${hex:0:1}${hex:1:1}${hex:1:1}${hex:2:1}${hex:2:1}"
  fi
  local r=$((16#${hex:0:2}))
  local g=$((16#${hex:2:2}))
  local b=$((16#${hex:4:2}))
  echo "$r;$g;$b"
}

make_color_code() {
  local color="$1"
  local bg="$2"

  if is_hex_color "$color"; then
    local rgb
    rgb=$(hex_to_rgb "$color")
    if [[ "$bg" == "true" ]]; then
      echo "48;2;${rgb}"
    else
      echo "38;2;${rgb}"
    fi
    return 0
  fi

  if [[ "$bg" == "true" ]]; then
    case "$color" in
      black) echo "40" ;;
      red) echo "41" ;;
      green) echo "42" ;;
      yellow) echo "43" ;;
      blue) echo "44" ;;
      magenta) echo "45" ;;
      cyan) echo "46" ;;
      white) echo "47" ;;
      none) echo "49" ;;
      br_black) echo "100" ;;
      br_red) echo "101" ;;
      br_green) echo "102" ;;
      br_yellow) echo "103" ;;
      br_blue) echo "104" ;;
      br_magenta) echo "105" ;;
      br_cyan) echo "106" ;;
      br_white) echo "107" ;;
      *) echo "" ;;
    esac
  else
    case "$color" in
      black) echo "30" ;;
      red) echo "31" ;;
      green) echo "32" ;;
      yellow) echo "33" ;;
      blue) echo "34" ;;
      magenta) echo "35" ;;
      cyan) echo "36" ;;
      white) echo "37" ;;
      grey) echo "90" ;;
      silver) echo "37" ;;
      none) echo "39" ;;
      br_black) echo "90" ;;
      br_red) echo "91" ;;
      br_green) echo "92" ;;
      br_yellow) echo "93" ;;
      br_blue) echo "94" ;;
      br_magenta) echo "95" ;;
      br_cyan) echo "96" ;;
      br_white) echo "97" ;;
      *) echo "" ;;
    esac
  fi
}

style_formatting() {
  local msg="$1"
  local text_color="$2"
  local back_color="$3"
  local text_style="$4"

  local codes=()

  if [[ -n "$text_color" ]]; then
    local code
    code=$(make_color_code "$text_color" false)
    [[ -n "$code" ]] && codes+=("$code")
  fi

  if [[ -n "$back_color" ]]; then
    local code
    code=$(make_color_code "$back_color" true)
    [[ -n "$code" ]] && codes+=("$code")
  fi

  if [[ -n "$text_style" ]]; then
    local old_ifs="$IFS"
    IFS=',' read -r -a styles <<< "$text_style"
    IFS="$old_ifs"
    local style
    for style in "${styles[@]}"; do
      case "$style" in
        bold) codes+=("1") ;;
        italic) codes+=("3") ;;
        underline) codes+=("4") ;;
        strikethrough) codes+=("9") ;;
        dim) codes+=("2") ;;
        inverse) codes+=("7") ;;
      esac
    done
  fi

  if [[ ${#codes[@]} -eq 0 ]]; then
    printf '%s' "$msg"
    return 0
  fi

  local joined
  joined=$(IFS=';'; echo "${codes[*]}")
  printf '\033[%sm%s\033[0m' "$joined" "$msg"
}

capitalize() {
  local x="$1"
  [[ -z "$x" ]] && { printf '%s' "$x"; return 0; }

  local first_word
  first_word=$(echo "$x" | awk -F'[ -]+' '{print $1}')
  local lower
  lower=$(printf '%s' "$first_word" | tr '[:upper:]' '[:lower:]')

  if [[ "$first_word" == "$lower" ]]; then
    local first="${x:0:1}"
    local rest="${x:1}"
    printf '%s' "$(printf '%s' "$first" | tr '[:lower:]' '[:upper:]')${rest}"
  else
    printf '%s' "$x"
  fi
}

build_message() {
  local msg=""
  local part
  for part in "$@"; do
    msg+="$part"
  done
  capitalize "$msg"
}

parse_inline_expressions() {
  local msg="$1"

  eval_inline_expression() {
    local expr="$1"
    expr="$(printf '%s' "$expr" | sed -E 's/^[[:space:]]+|[[:space:]]+$//g')"

    if [[ "$expr" =~ ^[A-Za-z_][A-Za-z0-9_]*$ ]]; then
      local var_value="${!expr-}"
      if [[ -n "${var_value}" ]]; then
        printf '%s' "$var_value"
        return 0
      fi
      printf '%s' "$expr"
      return 0
    fi

    if [[ "$expr" =~ ^[0-9[:space:]+*/%().-]+$ ]]; then
      local calc
      calc=$(awk "BEGIN { v = ($expr); if (v == int(v)) printf \"%d\", v; else printf \"%s\", v }")
      printf '%s' "$calc"
      return 0
    fi

    printf '%s' "$expr"
  }

  while [[ "$msg" =~ \{\.val[[:space:]]+\{([^{}]+)\}\} ]]; do
    local expr="${BASH_REMATCH[1]}"
    local full_match="${BASH_REMATCH[0]}"
    local value
    value=$(eval_inline_expression "$expr")
    value=$(style_formatting "$value" "blue" "" "")
    msg="${msg/$full_match/$value}"
  done

  while [[ "$msg" =~ \{([^{}.][^{}]*)\} ]]; do
    local expr="${BASH_REMATCH[1]}"
    local full_match="${BASH_REMATCH[0]}"
    if [[ "$expr" =~ ^[[:space:]]*$ ]]; then
      break
    fi
    local value
    value=$(eval_inline_expression "$expr")
    msg="${msg/$full_match/$value}"
  done

  msg=$(printf '%s' "$msg" | perl -CSDA -pe '
    s/\{\.([A-Za-z_][A-Za-z0-9_]*)(\s{2,})([^{}]+)\}/"{.$1 ".(" " x (length($2)-1))."$3}"/ge;
    s/\{\.arg\s([^{}]+)\}/`$1`/g;
    s/\{\.code\s([^{}]+)\}/`$1`/g;
    s/\{\.run\s([^{}]+)\}/`$1`/g;
    s/\{\.str\s([^{}]+)\}/"$1"/g;
    s/\{\.val\s([^{}]+)\}/\e[34m"$1"\e[0m/g;
    s/\{\.(?:fn|fun)\s([^{}]+)\}/`$1()`/g;
    s/\{\.help\s([^{}]+)\}/`$1 --help`/g;
    s/\{\.topic\s([^{}]+)\}/`help $1`/g;
    s/\{\.vignette\s([^{}]+)\}/`vignette($1)`/g;
    s/\{\.url\s([^{}]+)\}/\e[3;34m<$1>\e[0m/g;
    s/\{\.email\s([^{}]+)\}/\e[34m\x27$1\x27\e[0m/g;
    s/\{\.path\s([^{}]+)\}/\e[34m$1\e[0m/g;
    s/\{\.file\s([^{}]+)\}/\e[34m\x27$1\x27\e[0m/g;
    s/\{\.pkg\s([^{}]+)\}/\e[34m$1\e[0m/g;
    s/\{\.field\s([^{}]+)\}/$1/g;
    s/\{\.emph\s([^{}]+)\}/$1/g;
    s/\{\.strong\s([^{}]+)\}/\e[1m$1\e[0m/g;
    s/\{\.envvar\s([^{}]+)\}/`$1`/g;
    s/\{\.kbd\s([^{}]+)\}/\e[34m[$1]\e[0m/g;
    s/\{\.key\s([^{}]+)\}/\e[34m[$1]\e[0m/g;
    s/\{\.dt\s([^{}]+)\}/$1: /g;
    s/\{\.cls\s([^{}]+)\}/<$1>/g;
    s/\{\.href\s\[([^\]]+)\]\(([^\)]+)\)\}/$1 (\e[3;34m<$2>\e[0m)/g;
    s/\{\.([A-Za-z_][A-Za-z0-9_]*)\s\{([^{}]+)\}\}/$2/g;
    s/\{\.([A-Za-z_][A-Za-z0-9_]*)\s([^{}]+)\}/$2/g;
  ')

  printf '%s' "$msg"
}

parse_bool() {
  local value="${1:-}"
  local normalized
  normalized=$(printf '%s' "$value" | tr '[:upper:]' '[:lower:]')
  case "$normalized" in
    true|t|1|yes|y) echo "true" ;;
    false|f|0|no|n) echo "false" ;;
    *) echo "invalid" ;;
  esac
}

get_verbose() {
  local local_verbose="$1"
  local global_verbose="${LOG_MESSAGE_VERBOSE:-}"

  if [[ -z "$global_verbose" ]]; then
    if [[ -z "$local_verbose" ]]; then
      echo "true"
      return 0
    fi

    local parsed_local
    parsed_local=$(parse_bool "$local_verbose")
    if [[ "$parsed_local" == "invalid" ]]; then
      printf '%s\n' "WARNING: verbose is not a logical value, set to TRUE" >&2
      echo "true"
      return 0
    fi

    echo "$parsed_local"
    return 0
  fi

  local parsed_global
  parsed_global=$(parse_bool "$global_verbose")
  if [[ "$parsed_global" == "invalid" ]]; then
    printf '%s\n' "WARNING: LOG_MESSAGE_VERBOSE is not a logical value, treated as NULL" >&2
    printf '%s\n' "WARNING: or use LOG_MESSAGE_VERBOSE=true/false" >&2
    echo "false"
    return 0
  fi

  echo "$parsed_global"
}

get_indent_part() {
  local symbol="$1"
  local level="$2"

  if [[ "$symbol" != "  " ]]; then
    local repeated=""
    local i
    for ((i = 0; i < level; i++)); do
      repeated+="$symbol"
    done
    printf '%s ' "$repeated"
  elif (( level > 1 )); then
    local repeated=""
    local i
    for ((i = 1; i < level; i++)); do
      repeated+="  "
    done
    printf '%s' "$repeated"
  else
    printf ''
  fi
}

validate_params() {
  local level="$1"
  local symbol="$2"
  local text_color="$3"
  local back_color="$4"
  local text_style="$5"
  local timestamp_style="$6"

  if ! [[ "$level" =~ ^[0-9]+$ ]] || (( level < 1 )); then
    printf '%s\n' "ERROR: level must be a positive integer" >&2
    return 1
  fi

  if [[ -z "$symbol" ]]; then
    printf '%s\n' "ERROR: symbol must be a single character string" >&2
    return 1
  fi

  if [[ -n "$text_color" ]] && ! check_color "$text_color"; then
    printf '%s\n' "ERROR: text_color must be a valid color name or hexadecimal color code" >&2
    return 1
  fi

  if [[ -n "$back_color" ]] && ! check_color "$back_color"; then
    printf '%s\n' "ERROR: back_color must be a valid color name or hexadecimal color code" >&2
    return 1
  fi

  if [[ -n "$text_color" && -n "$back_color" && "$text_color" == "$back_color" ]]; then
    printf '%s\n' "ERROR: text_color and back_color cannot be the same color" >&2
    return 1
  fi

  if [[ -n "$text_style" ]]; then
    local old_ifs="$IFS"
    IFS=',' read -r -a styles <<< "$text_style"
    IFS="$old_ifs"
    local style
    local valid
    for style in "${styles[@]}"; do
      valid="false"
      local candidate
      for candidate in bold italic underline strikethrough dim inverse; do
        if [[ "$style" == "$candidate" ]]; then
          valid="true"
          break
        fi
      done
      if [[ "$valid" != "true" ]]; then
        printf '%s\n' "ERROR: text_style must be one or more of: bold, italic, underline, strikethrough, dim, inverse" >&2
        return 1
      fi
    done
  fi

  local parsed_ts
  parsed_ts=$(parse_bool "$timestamp_style")
  if [[ "$parsed_ts" == "invalid" ]]; then
    printf '%s\n' "ERROR: timestamp_style must be a single logical value" >&2
    return 1
  fi

  return 0
}

output_cli_message() {
  local message="$1"
  local message_type="$2"

  local symbol=""
  case "$message_type" in
    info) symbol="ℹ" ;;
    success) symbol="✔" ;;
    warning) symbol="!" ;;
    running) symbol="◌" ;;
    ask) symbol="?" ;;
  esac

  local symbol_color=""
  case "$message_type" in
    info) symbol_color="cyan" ;;
    success) symbol_color="green" ;;
    warning) symbol_color="yellow" ;;
    running) symbol_color="#FFA500" ;;
    ask) symbol_color="magenta" ;;
  esac

  if [[ -n "$symbol" && -n "$symbol_color" ]]; then
    symbol=$(style_formatting "$symbol" "$symbol_color" "" "")
  fi

  printf '%s %s\n' "$symbol" "$message" >&2
}

plain_text_output() {
  local text="$1"
  local _cli_model="$2"
  local text_color="$3"
  local back_color="$4"
  local text_style="$5"

  if [[ -n "$text_color" || -n "$back_color" || -n "$text_style" ]]; then
    text=$(style_formatting "$text" "$text_color" "$back_color" "$text_style")
  fi

  printf '%s\n' "$text" >&2
}

format_line_with_style() {
  local line="$1"
  local prefix="$2"
  local text_color="$3"
  local back_color="$4"
  local text_style="$5"
  local timestamp_style="$6"

  if [[ -z "$text_color" && -z "$back_color" && -z "$text_style" ]]; then
    printf '%s%s' "$prefix" "$line"
    return 0
  fi

  if [[ "$timestamp_style" == "true" ]]; then
    style_formatting "${prefix}${line}" "$text_color" "$back_color" "$text_style"
  else
    local styled_line
    styled_line=$(style_formatting "$line" "$text_color" "$back_color" "$text_style")
    printf '%s%s' "$prefix" "$styled_line"
  fi
}

output_message() {
  local msg="$1"
  local message_type="$2"
  local cli_model="$3"
  local text_color="$4"
  local back_color="$5"
  local text_style="$6"
  local timestamp="$7"
  local timestamp_format="$8"
  local level="$9"
  local symbol="${10}"
  local multiline_indent="${11}"
  local timestamp_style="${12}"
  local plain_text="${13}"

  if [[ "$plain_text" == "true" ]]; then
    if [[ "$msg" == *$'\n'* ]]; then
      while IFS= read -r line || [[ -n "$line" ]]; do
        plain_text_output "$line" "$cli_model" "$text_color" "$back_color" "$text_style"
      done <<< "$msg"
    else
      plain_text_output "$msg" "$cli_model" "$text_color" "$back_color" "$text_style"
    fi
    return 0
  fi

  if [[ "$cli_model" == "true" && "$msg" == *$'\n'* ]]; then
    local i=0
    local line
    while IFS= read -r line || [[ -n "$line" ]]; do
      i=$((i + 1))
      local indent_part
      indent_part=$(get_indent_part "$symbol" "$level")
      local prefix

      if (( i == 1 )) || [[ "$multiline_indent" == "true" ]]; then
        local timestamp_part=""
        if [[ "$timestamp" == "true" ]]; then
          timestamp_part="$timestamp_format"
        fi
        prefix="${timestamp_part}${indent_part}"
      else
        local alignment_spaces=""
        if [[ "$timestamp" == "true" ]]; then
          local width=${#timestamp_format}
          alignment_spaces=$(printf '%*s' "$width" '')
        fi
        prefix="${alignment_spaces}${indent_part}"
      fi

      local formatted_line
      formatted_line=$(format_line_with_style "$line" "$prefix" "$text_color" "$back_color" "$text_style" "$timestamp_style")
      output_cli_message "$formatted_line" "$message_type"
    done <<< "$msg"
    return 0
  fi

  if [[ "$cli_model" == "true" ]]; then
    local timestamp_part=""
    if [[ "$timestamp" == "true" ]]; then
      timestamp_part="$timestamp_format"
    fi

    local indent_part
    indent_part=$(get_indent_part "$symbol" "$level")

    local final_msg
    if [[ "$symbol" != "  " ]]; then
      local repeated=""
      local i
      for ((i = 0; i < level; i++)); do
        repeated+="$symbol"
      done
      final_msg="${timestamp_part}${repeated} ${msg}"
    else
      final_msg="${timestamp_part}${indent_part}${msg}"
    fi

    if [[ -n "$text_color" || -n "$back_color" || -n "$text_style" ]]; then
      if [[ "$timestamp_style" == "true" ]]; then
        final_msg=$(style_formatting "$final_msg" "$text_color" "$back_color" "$text_style")
      else
        local styled_msg
        styled_msg=$(style_formatting "$msg" "$text_color" "$back_color" "$text_style")

        if [[ "$symbol" != "  " ]]; then
          local repeated=""
          local i
          for ((i = 0; i < level; i++)); do
            repeated+="$symbol"
          done
          final_msg="${timestamp_part}${repeated} ${styled_msg}"
        else
          final_msg="${timestamp_part}${indent_part}${styled_msg}"
        fi
      fi
    fi

    output_cli_message "$final_msg" "$message_type"
    return 0
  fi

  local formatted_msg="$msg"
  if [[ -n "$text_color" || -n "$back_color" || -n "$text_style" ]]; then
    formatted_msg=$(style_formatting "$formatted_msg" "$text_color" "$back_color" "$text_style")
  fi

  local prefix=""
  case "$message_type" in
    info) prefix="" ;;
    success) prefix="SUCCESS: " ;;
    warning) prefix="WARNING: " ;;
    running) prefix="RUNNING: " ;;
    ask) prefix="? " ;;
  esac

  printf '%s%s\n' "$prefix" "$formatted_msg" >&2
}

ask_yes_no_cancel() {
  while true; do
    read -r -p "[y/n/c]: " answer || { echo "NA"; return 0; }
    local normalized
    normalized=$(printf '%s' "$answer" | tr '[:upper:]' '[:lower:]')
    case "$normalized" in
      y|yes) echo "TRUE"; return 0 ;;
      n|no) echo "FALSE"; return 0 ;;
      c|cancel|"") echo "NA"; return 0 ;;
    esac
  done
}

log_message() {
  local verbose="true"
  local message_type="info"
  local cli_model="true"
  local level=1
  local symbol="  "
  local text_color=""
  local back_color=""
  local text_style=""
  local multiline_indent="false"
  local timestamp="true"
  local timestamp_format=""
  local timestamp_style="false"
  local plain_text="false"

  local message_parts=()

  while (($# > 0)); do
    case "$1" in
      --verbose)
        verbose="$2"
        shift 2
        ;;
      --message-type)
        message_type="$2"
        shift 2
        ;;
      --cli-model)
        cli_model="$2"
        shift 2
        ;;
      --level)
        level="$2"
        shift 2
        ;;
      --symbol)
        symbol="$2"
        shift 2
        ;;
      --text-color)
        text_color="$2"
        shift 2
        ;;
      --back-color)
        back_color="$2"
        shift 2
        ;;
      --text-style)
        text_style="$2"
        shift 2
        ;;
      --multiline-indent)
        multiline_indent="true"
        shift
        ;;
      --timestamp)
        timestamp="$2"
        shift 2
        ;;
      --timestamp-format)
        timestamp_format="$2"
        shift 2
        ;;
      --timestamp-style)
        timestamp_style="true"
        shift
        ;;
      --plain-text)
        plain_text="true"
        shift
        ;;
      --)
        shift
        while (($# > 0)); do
          message_parts+=("$1")
          shift
        done
        ;;
      *)
        message_parts+=("$1")
        shift
        ;;
    esac
  done

  local valid_type="false"
  local t
  for t in info success warning error running ask; do
    if [[ "$message_type" == "$t" ]]; then
      valid_type="true"
      break
    fi
  done

  if [[ "$valid_type" != "true" ]]; then
    printf '%s\n' "ERROR: message_type must be one of: info, success, warning, error, running, ask" >&2
    return 1
  fi

  local msg
  msg=$(build_message "${message_parts[@]}")
  msg=$(parse_inline_expressions "$msg")
  msg="${msg//\\n/$'\n'}"

  if [[ "$message_type" == "error" ]]; then
    printf '%s\n' "ERROR: ${msg}" >&2
    return 1
  fi

  local verbose_value
  verbose_value=$(get_verbose "$verbose")
  if [[ "$verbose_value" != "true" ]]; then
    return 0
  fi

  local cli_model_parsed timestamp_parsed timestamp_style_parsed multiline_indent_parsed
  cli_model_parsed=$(parse_bool "$cli_model")
  timestamp_parsed=$(parse_bool "$timestamp")
  timestamp_style_parsed=$(parse_bool "$timestamp_style")
  multiline_indent_parsed=$(parse_bool "$multiline_indent")

  if [[ "$cli_model_parsed" == "invalid" || "$timestamp_parsed" == "invalid" || "$timestamp_style_parsed" == "invalid" || "$multiline_indent_parsed" == "invalid" ]]; then
    printf '%s\n' "ERROR: cli_model/timestamp/multiline_indent/timestamp_style must be logical values" >&2
    return 1
  fi

  if ! validate_params "$level" "$symbol" "$text_color" "$back_color" "$text_style" "$timestamp_style_parsed"; then
    return 1
  fi

  if [[ -z "$timestamp_format" ]]; then
    timestamp_format="[$(date '+%Y-%m-%d %H:%M:%S')] "
  fi

  output_message \
    "$msg" \
    "$message_type" \
    "$cli_model_parsed" \
    "$text_color" \
    "$back_color" \
    "$text_style" \
    "$timestamp_parsed" \
    "$timestamp_format" \
    "$level" \
    "$symbol" \
    "$multiline_indent_parsed" \
    "$timestamp_style_parsed" \
    "$plain_text"

  if [[ "$message_type" == "ask" ]]; then
    ask_yes_no_cancel
  fi
}

if [[ -n "${BASH_VERSION-}" && -n "${BASH_SOURCE-}" && "${BASH_SOURCE[0]}" == "$0" ]]; then
  log_message "$@"
fi
