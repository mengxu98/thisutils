#' @title figlet
#'
#' @description Create ASCII art text using figlet fonts.
#'
#' @param text Text to make bigger
#' @param font Name of font, path to font, or `figlet_font` object
#' @param width Width to use when justifying and breaking lines
#' @param justify Text justification to use in rendering ("left",
#'   "centre", "right")
#' @param absolute Logical, indicating if alignment is absolute
#' @param strip Logical, indicating if whitespace should be removed
#'
#' @return An object of class `figlet_text` which is a character
#'   vector with a handy print method
#'
#' @references
#' \url{http://www.figlet.org/}
#' \url{https://github.com/richfitz/rfiglet}
#' \url{https://github.com/jbkunst/figletr}
#'
#' @export
#'
#' @examples
#' figlet("thisutils")
figlet <- function(
    text,
    font = "Slant",
    width = getOption("width", 80),
    justify = "left",
    absolute = FALSE,
    strip = TRUE) {
  font <- figlet_font(font)
  str <- .figlet_render(
    text,
    font,
    width,
    justify,
    absolute,
    strip
  )
  class(str) <- "figlet_text"
  attr(str, "font") <- font$name
  attr(str, "text") <- text

  return(str)
}

#' @title List available figlet fonts
#'
#' @description
#' List all figlet font files available in the package or system.
#'
#' @return Character vector of available font names.
#'
#' @export
#'
#' @examples
#' list_figlet_fonts()
list_figlet_fonts <- function() {
  inst_fonts_dir <- system.file("fonts", package = "thisutils")
  available_fonts <- character(0)

  if (dir.exists(inst_fonts_dir)) {
    font_files <- list.files(
      inst_fonts_dir,
      pattern = "\\.flf$",
      full.names = FALSE
    )
    available_fonts <- tools::file_path_sans_ext(font_files)
  }

  if (!"Slant" %in% available_fonts) {
    available_fonts <- c("Slant", available_fonts)
  }

  if (length(available_fonts) > 0) {
    log_message("Available figlet fonts:")
    for (font in available_fonts) {
      log_message("Font: ", font, level = 2)
    }
  }

  invisible(available_fonts)
}

.figlet_render <- function(
    text,
    font,
    width = getOption("width", 80),
    justify = "left",
    absolute = FALSE,
    strip = TRUE) {
  if (any(grepl("\n", text, fixed = TRUE))) {
    text <- unlist(strsplit(text, "\n", fixed = TRUE))
  }
  if (length(text) == 1L) {
    dat <- .figlet_render_horizontal(text, font)
  } else {
    dat <- lapply(text, .figlet_render_horizontal, font)
  }
  lines <- .figlet_render_layout(
    dat, font$options$hard_blank, width, justify,
    absolute
  )
  mat <- .figlet_render_vertical(lines, font)
  .matrix_to_text(mat, strip = strip)
}

.figlet_render_horizontal <- function(text, font) {
  state <- list(
    curr_char_width = 0L,
    prev_char_width = 0L
  )

  height <- font$options$height
  buffer <- matrix(character(), height, 0)

  char_index <- .asc(text)
  for (char in seq_along(char_index)) {
    cur_char <- font$chars[[char_index[[char]]]]
    if (is.null(cur_char)) {
      stop(sprintf(
        "The font '%s' does not contain the characters '%s'",
        font$name, substr(text, char, char)
      ))
    }

    state$curr_char_width <- cur_char$width
    max_smush <- .smush_amount(
      buffer,
      cur_char$data,
      font$options,
      state
    )

    if (font$options$right_to_left) {
      add_left <- cur_char$data
      add_right <- buffer
    } else {
      add_left <- buffer
      add_right <- cur_char$data
    }

    n_left <- ncol(add_left)
    n_right <- ncol(add_right)

    for (i in seq_len(max_smush)) {
      idx <- n_left - max_smush + i
      if (idx >= 1L && idx <= n_left) {
        left <- add_left[, idx]
      } else {
        left <- rep("", height)
      }
      right <- add_right[, i]

      smushed <- .vcapply(
        seq_len(height), function(row) {
          .smush_chars(
            left[row],
            right[row],
            font$options,
            state
          )
        }
      )

      idx <- n_left - max_smush + i
      if (idx >= 1L && idx <= n_left) {
        add_left[, idx] <- smushed
      }
    }
    if (n_right > 0) {
      add_right <- add_right[, (max_smush + 1):n_right, drop = FALSE]
    }

    buffer <- cbind(add_left, add_right, deparse.level = 0)
    state$prev_char_width <- state$curr_char_width
  }

  buffer[buffer == font$options$hard_blank] <- " "
  buffer
}

.figlet_render_vertical <- function(lines, font) {
  if (length(lines) == 1L) {
    return(lines[[1]])
  }

  buffer <- lines[[1]]
  width <- ncol(buffer)
  for (line in 2:length(lines)) {
    cur_line <- lines[[line]]
    max_smush <- .vsmush_amount(
      buffer,
      cur_line,
      font$options
    )
    add_above <- buffer
    add_below <- cur_line
    n_above <- nrow(add_above)
    n_below <- nrow(add_below)

    for (i in seq_len(max_smush)) {
      idx <- n_above - max_smush + i
      above <- add_above[idx, ]
      below <- add_below[i, ]

      smushed <- .vcapply(seq_len(width), function(col) {
        .vsmush_chars(above[col], below[col], font$options)
      })

      idx <- n_above - max_smush + i
      if (idx >= 1L && idx <= n_above) {
        add_above[idx, ] <- smushed
      }
    }
    if (n_below > 0) {
      add_below <- add_below[(max_smush + 1):n_below, , drop = FALSE]
    }
    buffer <- rbind(add_above, add_below, deparse.level = 0)
  }

  buffer
}

.figlet_render_layout <- function(
    buffer,
    hard_blank,
    width,
    justify,
    absolute) {
  if (is.list(buffer)) {
    template <- list(
      template = character(),
      start = integer(),
      end = integer()
    )
    for (line in buffer) {
      tmp <- .figlet_render_layout_template(
        line, hard_blank,
        length(template$start),
        sum(nchar(template$template))
      )
      template <- Map(c, template, tmp)
    }
    buffer <- do.call(cbind, buffer)
  } else {
    template <- .figlet_render_layout_template(buffer, hard_blank)
  }

  template$template <- gsub("(?<= ) ", "-", template$template, perl = TRUE)
  tmp <- trimws(gsub("-", " ", strwrap(template$template, width = width)))
  lpad <- nchar(sub("[^ ].*", "", format(tmp, justify = justify)))
  rpad <- max(nchar(tmp) + lpad) - nchar(tmp) - lpad
  if (absolute && max(nchar(tmp)) < width) {
    if (justify == "right") {
      lpad <- lpad + (width - max(nchar(tmp)))
    } else if (justify == "centre") {
      lpad <- lpad + ceiling((width - max(nchar(tmp))) / 2)
    }
  }

  map <- c(1:9, letters, LETTERS)
  words <- lapply(strsplit(trimws(tmp), " +"), function(x) {
    range(match(substr(x, 1, 1), map))
  })

  line <- function(i) {
    j <- template$start[words[[i]][1]]:template$end[words[[i]][2]]
    text <- buffer[, j, drop = FALSE]
    if (lpad[i] > 0) {
      text <- cbind(matrix(" ", nrow(text), lpad[i]), text)
    }
    if (rpad[i] > 0) {
      text <- cbind(text, matrix(" ", nrow(text), rpad[i]))
    }
    text
  }

  lapply(seq_along(words), line)
}

.figlet_render_layout_template <- function(
    text, hard_blank,
    offset_char = 0,
    offset_text = 0) {
  pos <- apply(array(text %in% c(" ", hard_blank), dim(text)), 2, all)
  map <- c(1:9, letters, LETTERS)
  n <- cumsum(pos) + 1L + offset_char
  template <- map[n]
  template[pos] <- " "
  start <- c(1, which(pos) + 1L) + offset_text
  end <- c(which(pos) - 1L, ncol(text)) + offset_text
  list(
    template = paste(template, collapse = ""),
    start = start,
    end = end
  )
}

#' Get a figlet font
#'
#' @param font Path or name of the font to load
#'
#' @return A `figlet_font` object for use with [figlet]
#' @export
figlet_font <- function(font) {
  if (inherits(font, "figlet_font")) {
    return(font)
  }

  if (font == "Slant") {
    font_path <- system.file(
      "fonts/Slant.flf",
      package = "thisutils"
    )
    if (!file.exists(font_path)) {
      stop("Slant.flf font not found in package")
    }
    return(.figlet_font_read(font_path))
  }

  if (file.exists(font)) {
    return(.figlet_font_read(font))
  }

  stop(sprintf("Font '%s' not found", font))
}

.figlet_font_read <- function(filename) {
  name <- tools::file_path_sans_ext(basename(filename))
  if (!file.exists(filename)) {
    stop(sprintf("'%s' (%s) does not exist", name, filename))
  }
  data <- readLines(filename, warn = FALSE)
  options <- .figlet_font_options(data, filename, name)

  is_comment <- seq_len(options$comment_lines) + 1L
  chars <- .figlet_font_characters(
    data[-c(1L, is_comment)], options,
    filename, name
  )
  ret <- list(
    name = name,
    comments = data[is_comment],
    chars = chars,
    options = options
  )
  class(ret) <- "figlet_font"
  ret
}

.figlet_font_options <- function(data, filename, name) {
  if (length(data) == 0) {
    stop(sprintf("'%s' (%s) is empty", name, filename))
  }
  re_magic_number <- "^[tf]lf2."
  header <- data[[1]]
  if (!grepl(re_magic_number, header, perl = TRUE)) {
    stop(sprintf("'%s' (%s) is not a valid font", name, filename))
  }
  header <- strsplit(sub(re_magic_number, "", header), " ")[[1]]
  if (length(header) < 6) {
    stop(sprintf("'%s' (%s) has a malformed header", name, filename))
  }

  nms <- c(
    "hard_blank", "height", "base_line", "max_length",
    "old_layout", "comment_lines", "print_direction", "full_layout"
  )

  msg <- max(0L, length(nms) - length(header))
  if (msg > 0L) {
    header <- c(header, rep(list(NA_integer_), msg))
  }

  options <- as.list(header[seq_along(nms)])
  options[-1] <- lapply(options[-1], as.integer)
  names(options) <- nms

  if (is.na(options$full_layout)) {
    if (options$old_layout == 0L) {
      options$full_layout <- 64L
    } else if (options$old_layout < 0L) {
      options$full_layout <- 0L
    } else {
      options$full_layout <- as.integer(
        bitwOr(
          bitwAnd(options$old_layout, 31L), 128L
        )
      )
    }
  }

  options$smush_mode <- options$full_layout
  options$right_to_left <- identical(options$print_direction, 1L)
  options
}

.figlet_font_characters <- function(data, options, filename, name) {
  code_standard <- 32:126
  code_extra <- c(196, 214, 220, 228, 246, 252, 223)
  code_req <- c(code_standard, code_extra)

  get_character <- function(i, d) {
    .figlet_font_character(d[, i], options)
  }

  i_req <- seq_len(length(code_req) * options$height)
  dat_req <- matrix(data[i_req], options$height)
  chars <- vector("list", max(code_req))
  chars[code_req] <- lapply(seq_along(code_req), get_character, dat_req)
  chars
}

.figlet_font_character <- function(x, options) {
  if (any(is.na(iconv(x)))) {
    return(NULL)
  }

  re_end_marker <- ".*?(.)\\s*$"
  char <- sub(re_end_marker, "\\1", x[[1]])
  if (identical(char, "^")) {
    char <- "\\^"
  } else if (identical(char, "\\")) {
    char <- "\\\\"
  }
  re_end <- sprintf("[%s]{1,2}\\s*$", char)
  txt <- sub(re_end, "", x, perl = TRUE)
  char_width <- max(nchar(gsub(options$hard_blank, "", txt, fixed = TRUE)))

  m <- strsplit(txt, NULL)
  n <- lengths(m)
  if (any(i <- n < max(n))) {
    m[i] <- lapply(m[i], function(x) c(x, rep(" ", max(n) - length(x))))
  }
  list(
    width = as.integer(char_width),
    data = matrix(unlist(m), length(txt), byrow = TRUE)
  )
}



.asc <- function(x) {
  strtoi(charToRaw(x), 16L)
}

.vcapply <- function(X, FUN, ...) {
  vapply(X, FUN, character(1), ...)
}

.matrix_to_text <- function(m, strip) {
  ret <- apply(m, 1, paste, collapse = "")
  if (strip) {
    ret <- sub(" +$", "", ret)
    ret <- ret[nzchar(ret)]
  }
  ret <- gsub("\\\\", ".", ret)
  ret
}

.smush_chars <- function(left, right, options, state) {
  if (.is_space(left)) {
    return(right)
  } else if (.is_space(right)) {
    return(left)
  }

  if (state$prev_char_width < 2 || state$curr_char_width < 2) {
    return(NULL)
  }

  smush_mode <- options$smush_mode
  if (bitwAnd(smush_mode, 128) == 0) {
    return(NULL)
  }

  hard_blank <- options$hard_blank
  if (bitwAnd(smush_mode, 63) == 0) {
    if (left == hard_blank) {
      return(right)
    }
    if (right == hard_blank) {
      return(left)
    }
  }

  if (left == hard_blank && right == hard_blank) {
    if (bitwAnd(smush_mode, 32) > 0) {
      return(left)
    } else {
      return(NULL)
    }
  }

  if (bitwAnd(smush_mode, 1)) {
    if (left == right) {
      return(left)
    }
  }

  NULL
}

.smush_amount <- function(buffer, cur_char, options, state) {
  if (bitwAnd(options$smush_mode, bitwOr(128, 64)) == 0) {
    return(0L)
  }

  max_smush <- state$curr_char_width

  for (row in seq_len(options$height)) {
    line_left <- buffer[row, ]
    line_right <- cur_char[row, ]
    if (options$right_to_left) {
      ll <- line_left
      line_left <- line_right
      line_right <- ll
    }

    linebd <- .length_rstrip(line_left)
    if (linebd < 1L) {
      linebd <- 1L
    }

    if (linebd <= length(line_left)) {
      ch1 <- line_left[linebd]
    } else {
      linebd <- 1L
      ch1 <- ""
    }

    charbd <- length(line_right) - .length_lstrip(line_right) + 1L
    if (charbd <= length(line_right)) {
      ch2 <- line_right[charbd]
    } else {
      charbd <- length(line_right) + 1L
      ch2 <- ""
    }

    amt <- (charbd - 1L) + length(line_left) - 1L - (linebd - 1L)

    if (ch1 == "" || ch1 == " ") {
      amt <- amt + 1L
    } else if (ch2 != "" && !is.null(.smush_chars(ch1, ch2, options, state))) {
      amt <- amt + 1L
    }

    if (amt < max_smush) {
      max_smush <- amt
    }
  }

  max_smush
}

.vsmush_amount <- function(buffer, cur_line, options) {
  max_smush <- options$height
  for (col in seq_len(min(ncol(buffer), ncol(cur_line)))) {
    line_above <- buffer[, col]
    line_below <- cur_line[, col]

    linebd <- .length_rstrip(line_above)
    if (linebd < 1L) {
      linebd <- 1L
    }
    ch1 <- line_above[linebd]

    charbd <- length(line_below) - .length_lstrip(line_below) + 1L
    if (charbd <= length(line_below)) {
      ch2 <- line_below[charbd]
    } else {
      charbd <- length(line_below) + 1L
      ch2 <- ""
    }

    amt <- (charbd - 1L) + length(line_above) - 1L - (linebd - 1L)

    if (ch1 == "" || ch1 == " ") {
      amt <- amt + 1L
    } else if (ch2 != "" && !is.null(.vsmush_chars(ch1, ch2, options))) {
      amt <- amt + 1L
    }

    if (amt < max_smush) {
      max_smush <- amt
    }
  }

  max_smush
}

.vsmush_chars <- function(above, below, options) {
  if (.is_space(above)) {
    return(below)
  } else if (.is_space(below)) {
    return(above)
  }

  smush_mode <- options$smush_mode
  if (bitwAnd(smush_mode, 16384) == 0) {
    return(NULL)
  }

  if (bitwAnd(smush_mode, 256)) {
    if (above == below) {
      return(above)
    }
  }

  NULL
}

.is_space <- function(x) {
  grepl("\\s", x, perl = TRUE)
}

.length_lstrip <- function(x) {
  if (length(x) == 0) {
    return(0L)
  }
  i <- x != " "
  if (i[[1]]) {
    return(length(i))
  }
  if (!any(i)) {
    return(0L)
  }
  length(i) - which(i)[[1]] + 1L
}

.length_rstrip <- function(x) {
  if (length(x) == 0) {
    return(0L)
  }
  i <- x != " "
  n <- length(x)
  if (i[[n]]) {
    return(n)
  }
  if (!any(i)) {
    return(0L)
  }
  max(which(i))
}

#' @export
print.figlet_text <- function(x, ...) {
  cat(paste0(x, "\n", collapse = ""))
  invisible(x)
}

#' @export
print.figlet_font <- function(x, preview = TRUE, ...) {
  cat(sprintf("<figlet_font object: %s>\n", x$name))
  if (preview) {
    print(figlet(x$name, x))
  }
  invisible(x)
}

#' @export
as.character.figlet_text <- function(x, ...) {
  paste0(x, collapse = "\n")
}
