#' @title Check and install R packages
#'
#' @md
#' @inheritParams log_message
#' @param packages Package to be installed.
#' Package source can be *CRAN*, *Bioconductor* or *Github*.
#' By default, the package name is extracted according to the `packages` parameter.
#' @param lib The location of the library directories where to install the packages.
#' @param dependencies Which dependencies to install.
#' Passed to [pak::pkg_install].
#' Default is `NA`, auto install hard dependencies: *Depends*, *Imports*,
#' and *LinkingTo*, excluding *Suggests*.
#' @param force Whether to force the installation of packages.
#' Default is `FALSE`.
#' @param install Whether missing or outdated packages may be installed.
#' Set to `FALSE` for read-only diagnostics. Default is `TRUE` for backward
#' compatibility.
#' @param timeout Maximum installation time in seconds. A finite timeout runs
#' installation in a supervised R subprocess and terminates only that process
#' tree on timeout. Default is `Inf`.
#' @param load Whether to load packages after successful installation.
#' Uses [do.call] dispatch to avoid CRAN static checks on [base::library].
#' Default is `FALSE`.
#' @param cores Number of workers used by [pak::pkg_install()]. Use `NULL`
#' (the default) to let pak select its worker count automatically.
#'
#' @return Package installation status.
#'
#' @export
check_r <- function(
  packages,
  lib = .libPaths()[1],
  dependencies = NA,
  force = FALSE,
  install = TRUE,
  timeout = Inf,
  load = FALSE,
  cores = NULL,
  verbose = TRUE
) {
  if (!is.logical(install) || length(install) != 1L || is.na(install)) {
    stop("`install` must be TRUE or FALSE", call. = FALSE)
  }
  if (!is.numeric(timeout) || length(timeout) != 1L || is.na(timeout) || timeout <= 0) {
    stop("`timeout` must be one positive number or Inf", call. = FALSE)
  }
  packages <- as.character(packages)
  package_info <- lapply(packages, function(pkg) {
    version <- NULL
    if (grepl("/", pkg)) {
      pkg_name <- strsplit(pkg, split = "/|@|==", perl = TRUE)[[1]][[2]]
    } else {
      pkg_info <- strsplit(pkg, split = "@|==", perl = TRUE)[[1]]
      pkg_name <- pkg_info[[1]]
      if (length(pkg_info) > 1) {
        version <- pkg_info[[2]]
      }
    }
    list(package = pkg, name = pkg_name, version = version)
  })
  package_names <- vapply(package_info, `[[`, character(1), "name")
  unique_names <- unique(package_names)
  package_info <- lapply(unique_names, function(pkg_name) {
    matches <- package_info[package_names == pkg_name]
    remote <- vapply(matches, function(info) grepl("/", info$package), logical(1))
    remote_index <- which(remote)
    matches[[if (length(remote_index) > 0L) remote_index[[1]] else 1L]]
  })
  names(package_info) <- unique_names

  if (!is.null(cores)) {
    cores <- suppressWarnings(as.integer(cores[[1]]))
    if (is.na(cores) || cores < 1L) {
      stop("`cores` must be a positive integer or NULL.", call. = FALSE)
    }
    old_options <- options(Ncpus = cores)
    on.exit(options(old_options), add = TRUE)
  }

  needs_install <- vapply(package_info, function(info) {
    check_pkg <- check_pkg_status(
      info$name,
      version = info$version,
      lib = lib
    )

    force_update <- FALSE
    if (check_pkg && !is.null(info$version)) {
      current_version <- utils::packageVersion(info$name, lib.loc = lib)
      force_update <- current_version < package_version(info$version)
    }
    !check_pkg || force_update || isTRUE(force)
  }, logical(1))

  error_details <- list()
  packages_to_install <- vapply(package_info[needs_install], `[[`, character(1), "package")
  package_names_to_install <- names(package_info)[needs_install]
  if (length(packages_to_install) > 0L && !isTRUE(install)) {
    for (index in seq_along(packages_to_install)) {
      info <- package_info[[package_names_to_install[[index]]]]
      is_outdated <- !is.null(info$version) &&
        check_pkg_status(info$name, version = NULL, lib = lib)
      error_details[[info$name]] <- if (is_outdated) {
        "the installed version does not satisfy the request and installation is disabled"
      } else {
        "the package is unavailable and installation is disabled"
      }
    }
  } else if (length(packages_to_install) > 0L) {
    log_message(
      "Installing {.val {length(packages_to_install)}} R packages...",
      message_type = "running",
      verbose = verbose
    )
    if (!dir.exists(lib)) {
      dir.create(lib, recursive = TRUE, showWarnings = FALSE)
    }
    install_packages <- function(pkgs) {
      if (isTRUE(verbose)) {
        pak::pkg_install(pkgs, lib = lib, ask = FALSE, dependencies = dependencies)
      } else {
        invisible(suppressMessages(
          pak::pkg_install(pkgs, lib = lib, ask = FALSE, dependencies = dependencies)
        ))
      }
    }
    install_error <- if (is.finite(timeout)) {
      NULL
    } else {
      tryCatch(install_packages(packages_to_install), error = identity)
    }
    if (is.finite(timeout) || inherits(install_error, "error")) {
      log_message(
        if (is.finite(timeout)) "Installing packages individually with a timeout." else "Batch installation failed; retrying packages individually.",
        message_type = "warning",
        verbose = verbose
      )
      for (index in seq_along(packages_to_install)) {
        pkg <- packages_to_install[[index]]
        pkg_name <- package_names_to_install[[index]]
        error <- tryCatch(
          if (is.finite(timeout)) {
          check_r_run_install(
            pkg = pkg,
            lib = lib,
            dependencies = dependencies,
            timeout = timeout,
            verbose = verbose
          )
          } else {
            install_packages(pkg)
          },
          error = identity
        )
        if (inherits(error, "error")) {
          error_details[[pkg_name]] <- tryCatch(
            cli::ansi_strip(rlang::cnd_message(error, inherit = TRUE)),
            error = function(...) cli::ansi_strip(conditionMessage(error))
          )
        }
      }
    }
  }

  status_list <- lapply(package_info, function(info) {
    check_pkg <- check_pkg_status(
      info$name,
      version = info$version,
      lib = lib
    )
    isTRUE(check_pkg)
  })
  names(status_list) <- names(package_info)

  success <- sapply(status_list, isTRUE)
  failed <- names(status_list)[!success]

  if (length(failed) > 0) {
    for (pkg_name in failed) {
      err <- error_details[[pkg_name]]
      if (!is.null(err)) {
        log_message(
          "Failed to install: {.pkg {pkg_name}}. Error: {.val {err}}",
          message_type = "warning",
          verbose = verbose
        )
      } else {
        log_message(
          "Failed to install: {.pkg {pkg_name}}. Please install manually",
          message_type = "warning",
          verbose = verbose
        )
      }
    }
  } else {
    installed_names <- names(status_list)[success]
    log_message(
      "{.pkg {installed_names}} installed successfully",
      message_type = "success",
      verbose = verbose
    )
  }

  if (isTRUE(load)) {
    load_packages(
      names(status_list)[success],
      lib = lib,
      verbose = verbose
    )
  }

  return(invisible(status_list))
}

check_r_run_install <- function(
  pkg,
  lib,
  dependencies = NA,
  timeout = Inf,
  verbose = TRUE
) {
  process <- callr::r_bg(
    func = function(pkg, lib, dependencies, verbose) {
      install_call <- function() {
        pak::pkg_install(
          pkg,
          lib = lib,
          ask = FALSE,
          dependencies = dependencies
        )
      }
      if (isTRUE(verbose)) {
        install_call()
      } else {
        invisible(suppressMessages(install_call()))
      }
      invisible(TRUE)
    },
    args = list(
      pkg = pkg,
      lib = lib,
      dependencies = dependencies,
      verbose = verbose
    ),
    libpath = unique(c(lib, .libPaths())),
    stdout = "|",
    stderr = "|",
    supervise = TRUE,
    package = FALSE
  )
  on.exit({
    if (isTRUE(process$is_alive())) {
      process$kill_tree()
    }
  }, add = TRUE)

  drain_output <- function() {
    output <- process$read_output_lines()
    errors <- process$read_error_lines()
    if (isTRUE(verbose)) {
      if (length(output) > 0L) writeLines(output)
      if (length(errors) > 0L) writeLines(errors, con = stderr())
    }
  }

  started_at <- Sys.time()
  while (isTRUE(process$is_alive())) {
    wait_ms <- 100L
    if (is.finite(timeout)) {
      elapsed_ms <- as.numeric(difftime(Sys.time(), started_at, units = "secs")) * 1000
      remaining_ms <- ceiling(timeout * 1000 - elapsed_ms)
      if (remaining_ms <= 0) break
      wait_ms <- as.integer(min(wait_ms, remaining_ms))
    }
    process$wait(timeout = wait_ms)
    drain_output()
  }

  drain_output()
  if (isTRUE(process$is_alive())) {
    process$kill_tree()
    stop(
      sprintf("Package installation timed out after %s seconds: %s", format(timeout), pkg),
      call. = FALSE
    )
  }
  process$get_result()
  invisible(TRUE)
}

load_packages <- function(pkgs, lib = .libPaths(), verbose = TRUE) {
  for (pkg in pkgs) {
    result <- tryCatch(
      expr = {
        do.call(
          "library",
          list(
            pkg,
            lib.loc = lib,
            character.only = TRUE,
            quietly = !verbose
          )
        )
        TRUE
      },
      error = function(e) FALSE
    )
    if (!isTRUE(result)) {
      log_message(
        "Failed to load: {.pkg {pkg}}",
        message_type = "warning",
        verbose = verbose
      )
    } else {
      log_message(
        "Loaded: {.pkg {pkg}}",
        message_type = "success",
        verbose = verbose
      )
    }
  }
  invisible(NULL)
}

#' @title Check and remove R packages
#'
#' @md
#' @inheritParams log_message
#' @param packages Package to be removed.
#' @param lib The location of the library directories where to remove the packages.
#'
#' @export
remove_r <- function(
  packages,
  lib = .libPaths()[1],
  verbose = TRUE
) {
  status_list <- list()
  for (pkg in packages) {
    pkg_installed <- check_pkg_status(pkg, lib = lib)

    if (pkg_installed) {
      log_message(
        "Removing: {.pkg {pkg}}...",
        verbose = verbose
      )
      status_list[[pkg]] <- FALSE
      tryCatch(
        expr = {
          pak::pkg_remove(pkg, lib = lib)
        },
        error = function(e) {
          log_message(
            "Warning during removal: {.pkg {pkg}}. Error: {.val {e$message}}",
            message_type = "warning",
            verbose = verbose
          )
        }
      )
      status_list[[pkg]] <- !check_pkg_status(pkg, lib = lib)
    } else {
      log_message(
        "{.pkg {pkg}} is not installed, skipping removal",
        message_type = "warning",
        verbose = verbose
      )
      status_list[[pkg]] <- TRUE
    }
  }

  success <- sapply(status_list, isTRUE)
  failed <- names(status_list)[!success]

  if (length(failed) > 0) {
    log_message(
      "Failed to remove: {.pkg {failed}}. Please remove manually",
      message_type = "warning",
      verbose = verbose
    )
  } else {
    log_message(
      "{.pkg {packages}} removed successfully",
      message_type = "success",
      verbose = verbose
    )
  }

  return(invisible(status_list))
}

#' @title Check if a package is installed with the specified version
#'
#' @md
#' @inheritParams check_r
#' @param pkg Package name.
#' @param version Package version to check. If `NULL`, only checks if the package is installed.
#'
#' @return `TRUE` if the package is installed with the specified version, `FALSE` otherwise.
#'
#' @export
check_pkg_status <- function(
  pkg,
  version = NULL,
  lib = .libPaths()[1]
) {
  installed_pkgs_info <- utils::installed.packages(lib.loc = lib)
  installed_pkgs <- installed_pkgs_info[, "Package"]
  installed_pkgs_version <- installed_pkgs_info[, "Version"]
  pkg_exists <- pkg %in% installed_pkgs
  if (is.null(version)) {
    version_match <- TRUE
  } else {
    version_match <- installed_pkgs_version[installed_pkgs == pkg] == version
  }

  if (isFALSE(pkg_exists) || isFALSE(version_match)) {
    return(FALSE)
  }

  TRUE
}
