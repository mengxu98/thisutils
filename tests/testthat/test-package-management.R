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
  expect_identical(loaded_lib, lib)
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
