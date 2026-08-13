test_that("check_r loads each successful package from the requested library", {
  loaded <- NULL
  loaded_lib <- NULL

  testthat::local_mocked_bindings(
    check_pkg_status = function(pkg, version = NULL, lib = NULL) {
      TRUE
    },
    load_packages = function(pkgs, lib = .libPaths(), verbose = TRUE) {
      loaded <<- pkgs
      loaded_lib <<- lib
      invisible(NULL)
    },
    .package = "thisutils"
  )

  lib <- tempfile("thisutils-library-")
  status <- check_r(
    c("foo", "foo", "bar"),
    lib = lib,
    load = TRUE,
    verbose = FALSE
  )

  expect_identical(status, list(foo = TRUE, bar = TRUE))
  expect_identical(loaded, c("foo", "bar"))
  expect_identical(loaded_lib, unique(c(lib, .libPaths())))
})

test_that("check_r recognizes packages in later library paths", {
  first_lib <- tempfile("thisutils-first-library-")
  dir.create(first_lib)

  status <- check_r(
    "stats",
    lib = first_lib,
    install = FALSE,
    verbose = FALSE
  )

  expect_identical(status, list(stats = TRUE))
  expect_false(dir.exists(file.path(first_lib, "stats")))
})

test_that("check_r installs an explicitly requested GitHub source", {
  source_matches <- FALSE
  installed <- character()
  testthat::local_mocked_bindings(
    check_pkg_status = function(...) TRUE,
    check_r_remote_status = function(...) source_matches,
    .package = "thisutils"
  )
  testthat::local_mocked_bindings(
    pkg_install = function(pkg, ...) {
      installed <<- pkg
      source_matches <<- TRUE
      invisible(NULL)
    },
    .package = "pak"
  )

  status <- check_r("owner/example", verbose = FALSE)

  expect_identical(unname(installed), "owner/example")
  expect_identical(status, list(example = TRUE))
})

test_that("check_r validates repository and pinned ref metadata", {
  info <- list(remote = "owner/example", ref = "abc123")
  testthat::local_mocked_bindings(
    packageDescription = function(...) {
      list(
        RemotePkgRef = "owner/example@abc123456789",
        RemoteRef = "main",
        RemoteSha = "abc123456789"
      )
    },
    .package = "utils"
  )

  remote_status <- getFromNamespace("check_r_remote_status", "thisutils")
  expect_true(remote_status(info))
  info$remote <- "another/example"
  expect_false(remote_status(info))
})

test_that("check_r temporarily extends an unset pak startup timeout", {
  old_timeout <- Sys.getenv("PKG_SUBPROCESS_TIMEOUT", unset = NA_character_)
  on.exit(
    if (is.na(old_timeout)) {
      Sys.unsetenv("PKG_SUBPROCESS_TIMEOUT")
    } else {
      Sys.setenv(PKG_SUBPROCESS_TIMEOUT = old_timeout)
    },
    add = TRUE
  )
  Sys.unsetenv("PKG_SUBPROCESS_TIMEOUT")
  observed <- NULL
  testthat::local_mocked_bindings(
    check_pkg_status = function(...) FALSE,
    .package = "thisutils"
  )
  testthat::local_mocked_bindings(
    pkg_install = function(...) {
      observed <<- Sys.getenv("PKG_SUBPROCESS_TIMEOUT", unset = NA_character_)
      invisible(NULL)
    },
    .package = "pak"
  )

  check_r("missing", verbose = FALSE)

  expect_identical(observed, "30000")
  expect_true(is.na(Sys.getenv("PKG_SUBPROCESS_TIMEOUT", unset = NA_character_)))
})

test_that("check_r supports read-only package diagnostics", {
  install_called <- FALSE
  testthat::local_mocked_bindings(
    check_pkg_status = function(pkg, version = NULL, lib = NULL) FALSE,
    check_r_run_install = function(...) {
      install_called <<- TRUE
      invisible(TRUE)
    },
    .package = "thisutils"
  )

  status <- check_r(
    c("missing", "owner/remote"),
    install = FALSE,
    verbose = FALSE
  )

  expect_identical(status, list(missing = FALSE, remote = FALSE))
  expect_false(install_called)
})

test_that("check_r forwards timeout to the supervised installer", {
  observed_timeout <- NULL
  testthat::local_mocked_bindings(
    check_pkg_status = function(pkg, version = NULL, lib = NULL) FALSE,
    check_r_run_install = function(pkg, lib, dependencies, timeout, verbose) {
      observed_timeout <<- timeout
      invisible(TRUE)
    },
    .package = "thisutils"
  )

  check_r("missing", timeout = 2.5, verbose = FALSE)
  expect_identical(observed_timeout, 2.5)
})

test_that("check_r validates installation controls", {
  expect_error(check_r("stats", install = NA), "install")
  expect_error(check_r("stats", timeout = 0), "timeout")
})

test_that("check_r_run_install drains child output while waiting", {
  events <- character()
  process <- local({
    alive <- TRUE
    list(
      is_alive = function() alive,
      wait = function(timeout) {
        events <<- c(events, "wait")
        alive <<- FALSE
        invisible(NULL)
      },
      read_output_lines = function() {
        events <<- c(events, "stdout")
        character()
      },
      read_error_lines = function() {
        events <<- c(events, "stderr")
        character()
      },
      kill_tree = function() invisible(NULL),
      get_result = function() TRUE
    )
  })
  testthat::local_mocked_bindings(
    r_bg = function(...) process,
    .package = "callr"
  )

  check_r_run_install("foo", lib = tempdir(), verbose = FALSE)

  expect_identical(events[1:3], c("wait", "stdout", "stderr"))
})

test_that("check_r batches missing packages and forwards cores to pak", {
  status <- c(foo = FALSE, bar = FALSE)
  installed <- NULL
  workers <- NULL

  testthat::local_mocked_bindings(
    check_pkg_status = function(pkg, version = NULL, lib = NULL) status[[pkg]],
    .package = "thisutils"
  )
  testthat::local_mocked_bindings(
    pkg_install = function(pkg, ...) {
      installed <<- pkg
      workers <<- getOption("Ncpus")
      status[pkg] <<- TRUE
      invisible(NULL)
    },
    .package = "pak"
  )

  result <- check_r(c("foo", "bar"), cores = 2, verbose = FALSE)

  expect_identical(installed, stats::setNames(c("foo", "bar"), c("foo", "bar")))
  expect_identical(workers, 2L)
  expect_identical(result, list(foo = TRUE, bar = TRUE))
})

test_that("check_r falls back to remotes for malformed GitHub DESCRIPTION files", {
  status <- c(yaGST = FALSE, SCEVAN = FALSE)
  installed_repo <- NULL
  installed_ref <- NULL
  installed_upgrade <- NULL

  testthat::local_mocked_bindings(
    check_pkg_status = function(pkg, version = NULL, lib = NULL) status[[pkg]],
    check_r_remote_status = function(info, ...) status[[info$name]],
    .package = "thisutils"
  )
  testthat::local_mocked_bindings(
    pkg_install = function(...) {
      pkg <- list(...)[[1]]
      if (length(pkg) > 1L) {
        stop("Could not solve package dependencies")
      }
      if (identical(pkg, "miccec/yaGST")) {
        stop("Can't parse DESCRIPTION file in GitHub repo miccec/yaGST: Duplicate DESCRIPTION fields")
      }
      expect_identical(pkg, "AntonioDeFalco/SCEVAN")
      expect_true(status[["yaGST"]])
      status[["SCEVAN"]] <<- TRUE
      invisible(NULL)
    },
    .package = "pak"
  )
  testthat::local_mocked_bindings(
    install_github = function(repo, ref, lib, dependencies, upgrade, force, quiet) {
      installed_repo <<- repo
      installed_ref <<- ref
      installed_upgrade <<- upgrade
      status[["yaGST"]] <<- TRUE
      invisible(NULL)
    },
    .package = "remotes"
  )

  result <- check_r(
    c("miccec/yaGST", "AntonioDeFalco/SCEVAN"),
    verbose = FALSE
  )

  expect_identical(result, list(yaGST = TRUE, SCEVAN = TRUE))
  expect_identical(installed_repo, "miccec/yaGST")
  expect_identical(installed_ref, "HEAD")
  expect_identical(installed_upgrade, "never")
})
