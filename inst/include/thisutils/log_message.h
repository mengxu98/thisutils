#ifndef THISUTILS_LOG_MESSAGE_H
#define THISUTILS_LOG_MESSAGE_H

#include <Rcpp.h>
#include <cstdarg>
#include <cstdio>
#include <ctime>
#include <map>
#include <string>
#include <vector>

// A C++ implementation of thisutils::log_message for use inside compiled
// kernels. Keeps the same semantics as the R version:
//   - message_type: "info", "success", "warning", "error", "running", "ask"
//   - "error" aborts with the message (R version calls cli::cli_abort)
//   - "ask" prompts the user through utils::askYesNo and returns TRUE/FALSE/NA
//   - timestamp "[YYYY-MM-DD HH:MM:SS] " prefix
//   - level / symbol indentation
//   - verbose control: explicit argument wins, otherwise the R option
//     log_message.verbose is consulted
//   - message text is capitalized like build_message() in the R version
// Messages are printed to stderr, matching the Python implementation.

namespace thisutils {

inline std::string log_message_timestamp() {
  std::time_t now = std::time(nullptr);
  std::tm tm_buf;
#if defined(_WIN32)
  localtime_s(&tm_buf, &now);
#else
  localtime_r(&now, &tm_buf);
#endif
  char buffer[21];
  std::strftime(buffer, sizeof(buffer), "[%Y-%m-%d %H:%M:%S] ", &tm_buf);
  return std::string(buffer);
}

inline std::string capitalize_message(const std::string& msg) {
  if (!msg.empty() && msg[0] >= 'a' && msg[0] <= 'z') {
    std::string out = msg;
    out[0] = static_cast<char>(out[0] - ('a' - 'A'));
    return out;
  }
  return msg;
}

inline std::string build_log_message(const std::vector<std::string>& parts) {
  std::string msg;
  for (const std::string& part : parts) {
    msg += part;
  }
  return capitalize_message(msg);
}

inline std::string ansi_style(
    const std::string& text,
    const std::string& color) {
  if (color.empty()) {
    return text;
  }
  static const std::map<std::string, std::string> codes = {
    {"black", "30"},   {"red", "31"},     {"green", "32"},
    {"yellow", "33"},  {"blue", "34"},    {"magenta", "35"},
    {"cyan", "36"},    {"white", "37"},   {"grey", "90"},
    {"silver", "37"},  {"br_black", "90"}, {"br_red", "91"},
    {"br_green", "92"}, {"br_yellow", "93"}, {"br_blue", "94"},
    {"br_magenta", "95"}, {"br_cyan", "96"}, {"br_white", "97"},
    {"none", "39"},
  };
  std::map<std::string, std::string>::const_iterator it = codes.find(color);
  if (it == codes.end()) {
    return text;
  }
  return "\033[" + it->second + "m" + text + "\033[0m";
}

inline bool log_message_verbose(bool verbose_supplied, bool verbose) {
  if (verbose_supplied) {
    return verbose;
  }
  Rcpp::Function get_option("getOption");
  Rcpp::Nullable<Rcpp::LogicalVector> opt = Rcpp::as<
      Rcpp::Nullable<Rcpp::LogicalVector> >(
      get_option("log_message.verbose", R_NilValue));
  if (opt.isNull()) {
    return true;
  }
  Rcpp::LogicalVector value(opt);
  if (value.size() != 1 || Rcpp::LogicalVector::is_na(value[0])) {
    Rcpp::Rcerr << "WARNING: log_message.verbose is not a logical value, "
                   "treated as NULL\n";
    return true;
  }
  return Rcpp::as<bool>(value);
}

inline std::string log_message_symbol(const std::string& message_type) {
  if (message_type == "info") return "\033[36mℹ\033[0m ";
  if (message_type == "success") return "\033[32m✔\033[0m ";
  if (message_type == "warning") return "\033[33m!\033[0m ";
  if (message_type == "running") return "\033[38;2;255;165;0m◌\033[0m ";
  if (message_type == "ask") return "\033[35m?\033[0m ";
  return "";
}

inline std::string log_message_indent(int level, const std::string& symbol) {
  if (symbol != "  ") {
    std::string out;
    for (int i = 0; i < level; ++i) {
      out += symbol;
    }
    return out + " ";
  }
  if (level > 1) {
    return std::string(static_cast<size_t>(2 * (level - 1)), ' ');
  }
  return "";
}

// Core entry point. Returns TRUE for "ask" when the user confirms, FALSE for
// "ask" when declined, NA for cancel, and FALSE for other message types
// (mirroring the R version's return of invisible(NULL)).
inline SEXP log_message_impl(
    const std::vector<std::string>& parts,
    const std::string& message_type = "info",
    bool verbose = true,
    bool verbose_supplied = false,
    int level = 1,
    const std::string& symbol = "  ",
    bool timestamp = true) {
  const std::string msg = build_log_message(parts);

  if (message_type == "error") {
    Rcpp::stop(msg.c_str());
  }

  if (!log_message_verbose(verbose_supplied, verbose)) {
    if (message_type == "ask") {
      return Rcpp::LogicalVector(R_NaInt);
    }
    return R_NilValue;
  }

  std::string prefix;
  if (timestamp) {
    prefix += log_message_timestamp();
  }
  prefix += log_message_indent(level, symbol);

  if (message_type == "ask") {
    Rcpp::Rcerr << prefix << log_message_symbol("ask") << msg << "\n";
    Rcpp::Function ask_yes_no = Rcpp::Function(
        "askYesNo", Rcpp::Environment::namespace_env("utils"));
    return ask_yes_no(msg);
  }

  Rcpp::Rcerr << prefix << log_message_symbol(message_type) << msg << "\n";
  return R_NilValue;
}

// Single-message convenience overload.
inline SEXP log_message(
    const std::string& msg,
    const std::string& message_type = "info",
    bool verbose = true,
    bool verbose_supplied = false,
    int level = 1,
    const std::string& symbol = "  ",
    bool timestamp = true) {
  return log_message_impl(
      std::vector<std::string>(1, msg),
      message_type,
      verbose,
      verbose_supplied,
      level,
      symbol,
      timestamp);
}

// printf-style formatting entry point. The message_type is passed first so
// the variadic arguments stay unambiguous.
inline SEXP log_message_fmt(
    const std::string& message_type,
    const char* fmt,
    ...) {
  va_list args;
  va_start(args, fmt);
  char buffer[1024];
  std::vsnprintf(buffer, sizeof(buffer), fmt, args);
  va_end(args);
  return log_message(buffer, message_type);
}

}  // namespace thisutils

#endif  // THISUTILS_LOG_MESSAGE_H
