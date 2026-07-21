local_parallel_test_workers <- function(workers = 2L, .env = parent.frame()) {
  testthat::local_mocked_bindings(
    cores_detect = function(cores, num_session) {
      min(as.integer(workers), as.integer(cores), as.integer(num_session))
    },
    .package = "thisutils",
    .env = .env
  )
}

test_that("parallelize_fun works with single core", {
  result <- suppressMessages(
    parallelize_fun(1:3, function(x) x^2, verbose = FALSE)
  )
  expect_equal(length(result), 3)
  expect_equal(result[[1]], 1)
  expect_equal(result[[2]], 4)
  expect_equal(result[[3]], 9)
})

test_that("parallelize_fun preserves historical positional arguments", {
  result <- suppressMessages(
    parallelize_fun(
      1:2,
      identity,
      1L,
      "unused_export",
      FALSE,
      FALSE,
      10L,
      "[test] ",
      FALSE
    )
  )

  expect_identical(unname(unlist(result)), 1:2)
})

test_that("parallelize_fun handles errors with clean_result = FALSE", {
  result <- suppressMessages(
    parallelize_fun(1:3, function(x) {
      if (x == 2) stop("fail")
      x
    }, verbose = FALSE, throw_error = FALSE)
  )
  expect_equal(length(result), 3)
  expect_true(inherits(result[[2]], "parallelize_error"))
})

test_that("parallelize_fun handles errors with clean_result = TRUE", {
  result <- suppressMessages(
    parallelize_fun(1:3, function(x) {
      if (x == 2) stop("fail")
      x
    }, clean_result = TRUE, verbose = FALSE, throw_error = FALSE)
  )
  expect_equal(length(result), 2)
})

test_that("parallelize_fun preserves names for named vectors", {
  x <- c(a = 1, b = 2, c = 3)
  result <- suppressMessages(
    parallelize_fun(x, function(v) v^2, verbose = FALSE)
  )
  expect_equal(names(result), c("a", "b", "c"))
})

test_that("parallelize_fun names output with values for unnamed vectors", {
  result <- suppressMessages(
    parallelize_fun(1:3, function(x) x^2, verbose = FALSE)
  )
  expect_equal(names(result), c("1", "2", "3"))
})

test_that("parallelize_fun does not set names for list inputs", {
  result <- suppressMessages(
    parallelize_fun(list(1, 2, 3), function(x) x^2, verbose = FALSE)
  )
  expect_null(names(result))
})

test_that("parallelize_fun restores cli options on error", {
  old_show <- getOption("cli.progress_show_after")
  old_clear <- getOption("cli.progress_clear")
  tryCatch(
    parallelize_fun(1:3, function(x) {
      stop("intentional error")
    }, verbose = TRUE, throw_error = FALSE),
    error = function(e) NULL,
    message = function(m) NULL
  )
  expect_equal(getOption("cli.progress_show_after"), old_show)
  expect_equal(getOption("cli.progress_clear"), old_clear)
})

test_that("parallelize_fun with verbose progress bar", {
  expect_message(
    parallelize_fun(1:3, function(x) x^2, verbose = TRUE)
  )
})

test_that("parallel progress bar uses supplied width", {
  bar <- parallel_progress_bar(6, 10, 12L)
  expect_equal(nchar(cli::ansi_strip(bar)), 12)
})

test_that("parallel progress bar falls back to default width", {
  bar <- parallel_progress_bar(6, 10)
  expect_equal(nchar(cli::ansi_strip(bar)), 10)
})

test_that("parallelize_fun accepts progress_bar_width argument", {
  expect_no_error(
    suppressMessages(
      parallelize_fun(1:2, function(x) x^2, verbose = FALSE, progress_bar_width = 9L)
    )
  )
})

test_that("parallelize_fun preserves input order for multi-core execution", {
  local_parallel_test_workers()

  result <- suppressMessages(
    parallelize_fun(1:6, function(x) x, cores = 2, verbose = TRUE)
  )

  expect_equal(unname(unlist(result)), 1:6)
  expect_equal(names(result), as.character(1:6))
})

test_that("parallelize_fun preserves input order for multi-core execution without verbose", {
  local_parallel_test_workers()

  result <- suppressMessages(
    parallelize_fun(1:6, function(x) x * 2, cores = 2, verbose = FALSE)
  )

  expect_equal(unname(unlist(result)), (1:6) * 2)
  expect_equal(names(result), as.character(1:6))
})

test_that("parallelize_fun handles uneven multi-core workloads in verbose mode", {
  local_parallel_test_workers()

  delays <- c(0.15, 0.01, 0.12, 0.02, 0.08, 0.01)
  result <- suppressMessages(
    parallelize_fun(1:6, function(x) {
      Sys.sleep(delays[[x]])
      x
    }, cores = 2, verbose = TRUE)
  )

  expect_equal(unname(unlist(result)), 1:6)
  expect_equal(names(result), as.character(1:6))
})

test_that("parallelize_fun exports requested dependencies in multi-core mode", {
  local_parallel_test_workers()

  offset <- 5
  add_offset <- function(x) x + offset

  result <- suppressMessages(
    parallelize_fun(
      1:4,
      add_offset,
      cores = 2,
      verbose = FALSE,
      export_fun = "offset"
    )
  )

  expect_equal(unname(unlist(result)), 6:9)
  expect_equal(names(result), as.character(1:4))
})

test_that("cores_detect falls back to at least one core", {
  expect_gte(cores_detect(cores = 2, num_session = 4), 1)
})

test_that("parallelize_fun normalizes invalid and oversized core requests", {
  invalid <- suppressMessages(
    parallelize_fun(1:3, identity, cores = NA, verbose = FALSE)
  )
  oversized <- suppressMessages(
    parallelize_fun(1:2, identity, cores = 128, verbose = FALSE)
  )

  expect_identical(unname(unlist(invalid)), 1:3)
  expect_identical(unname(unlist(oversized)), 1:2)
})

test_that("parallelize_fun preserves NULL results on a single core", {
  result <- suppressMessages(
    parallelize_fun(
      1:3,
      function(x) if (x == 2) NULL else x,
      verbose = FALSE
    )
  )

  expect_length(result, 3)
  expect_equal(result[[1]], 1)
  expect_null(result[[2]])
  expect_equal(result[[3]], 3)
})

test_that("parallelize_fun reuses a bounded set of workers", {
  skip_on_os("windows")
  local_parallel_test_workers()

  worker_pids <- suppressMessages(
    parallelize_fun(
      1:12,
      function(x) Sys.getpid(),
      cores = 2,
      verbose = FALSE
    )
  )

  expect_lte(length(unique(unlist(worker_pids))), 2L)
})

test_that("parallel worker liveness checks do not terminate the process", {
  cl <- parallel::makePSOCKcluster(1L)
  on.exit(try(parallel::stopCluster(cl), silent = TRUE), add = TRUE)
  worker_pid <- parallel::clusterCall(cl, Sys.getpid)[[1L]]

  expect_true(parallel_process_alive(worker_pid))
  expect_identical(parallel::clusterCall(cl, function() TRUE), list(TRUE))
})

test_that("parallelize_fun uses PSOCK while collecting coverage", {
  local_parallel_test_workers()

  old_covr <- Sys.getenv("R_COVR", unset = NA_character_)
  on.exit({
    if (is.na(old_covr)) {
      Sys.unsetenv("R_COVR")
    } else {
      Sys.setenv(R_COVR = old_covr)
    }
  }, add = TRUE)
  Sys.setenv(R_COVR = "true")

  is_fork_child <- suppressMessages(
    parallelize_fun(
      1:4,
      function(x) getFromNamespace("isChild", "parallel")(),
      cores = 2,
      verbose = FALSE
    )
  )

  expect_false(any(unlist(is_fork_child)))
})

test_that("parallelize_fun resolves a supplied closure before PSOCK serialization", {
  local_parallel_test_workers()

  old_covr <- Sys.getenv("R_COVR", unset = NA_character_)
  on.exit({
    if (is.na(old_covr)) {
      Sys.unsetenv("R_COVR")
    } else {
      Sys.setenv(R_COVR = old_covr)
    }
  }, add = TRUE)
  Sys.setenv(R_COVR = "true")

  add_offset <- local({
    offset <- 11L
    function(x) x + offset
  })
  result <- suppressMessages(
    parallelize_fun(1:6, add_offset, cores = 2, verbose = FALSE)
  )

  expect_identical(unname(unlist(result)), 12:17)
})

test_that("parallelize_fun reuses PSOCK workers", {
  local_parallel_test_workers()

  old_covr <- Sys.getenv("R_COVR", unset = NA_character_)
  on.exit({
    if (is.na(old_covr)) {
      Sys.unsetenv("R_COVR")
    } else {
      Sys.setenv(R_COVR = old_covr)
    }
  }, add = TRUE)
  Sys.setenv(R_COVR = "true")

  worker_pids <- suppressMessages(
    parallelize_fun(
      1:12,
      function(x) Sys.getpid(),
      cores = 2,
      verbose = FALSE
    )
  )

  expect_lte(length(unique(unlist(worker_pids))), 2L)
})

test_that("parallelize_fun supports explicit fork and PSOCK backends", {
  skip_on_os("windows")
  local_parallel_test_workers()
  skip_if(
    identical(Sys.getenv("R_COVR"), "true"),
    "fork-specific backend test"
  )

  is_fork <- function(backend) {
    suppressMessages(
      parallelize_fun(
        1:4,
        function(x) getFromNamespace("isChild", "parallel")(),
        cores = 2,
        backend = backend,
        verbose = FALSE
      )
    )
  }

  expect_true(all(unlist(is_fork("fork"))))
  expect_false(any(unlist(is_fork("psock"))))
})

test_that("auto uses PSOCK while a future worker plan is active", {
  skip_on_os("windows")
  skip_if_not_installed("future")
  local_parallel_test_workers()

  old_plan <- future::plan()
  on.exit(future::plan(old_plan), add = TRUE)
  future::plan(future::multisession, workers = 2L)

  is_fork_child <- suppressMessages(
    parallelize_fun(
      1:4,
      function(x) getFromNamespace("isChild", "parallel")(),
      cores = 2,
      backend = "auto",
      verbose = FALSE
    )
  )

  expect_false(any(unlist(is_fork_child)))
})

test_that("auto completes work that uses an active future plan", {
  skip_on_os("windows")
  skip_if_not_installed("future")
  skip_if_not_installed("future.apply")
  local_parallel_test_workers()

  old_plan <- future::plan()
  on.exit(future::plan(old_plan), add = TRUE)
  future::plan(future::multisession, workers = 2L)

  result <- suppressMessages(
    parallelize_fun(
      1:4,
      function(i) {
        sum(unlist(future.apply::future_lapply(
          1:4,
          function(j) i + j,
          future.seed = TRUE
        )))
      },
      cores = 2,
      backend = "auto",
      timeout = 30,
      verbose = FALSE
    )
  )

  expect_identical(unname(unlist(result)), c(14L, 18L, 22L, 26L))
})

test_that("explicit fork warns while a future worker plan is active", {
  skip_on_os("windows")
  skip_if_not_installed("future")
  local_parallel_test_workers()
  skip_if(
    identical(Sys.getenv("R_COVR"), "true"),
    "fork-specific warning test"
  )

  old_plan <- future::plan()
  on.exit(future::plan(old_plan), add = TRUE)
  future::plan(future::multisession, workers = 2L)

  expect_warning(
    suppressMessages(
      parallelize_fun(
        1:2,
        identity,
        cores = 2,
        backend = "fork",
        verbose = FALSE
      )
    ),
    "active non-sequential future plan"
  )
})

test_that("parallel tasks fail within the requested timeout", {
  local_parallel_test_workers()

  started <- proc.time()[["elapsed"]]

  expect_error(
    suppressMessages(
      parallelize_fun(
        1:2,
        function(x) {
          Sys.sleep(30)
          x
        },
        cores = 2,
        backend = "psock",
        timeout = 0.3,
        verbose = FALSE
      )
    ),
    class = "parallelize_timeout"
  )

  expect_lt(proc.time()[["elapsed"]] - started, 10)
})

test_that("timed-out PSOCK tasks leave no worker processes", {
  local_parallel_test_workers()

  pid_base <- tempfile("thisutils-timeout-worker-")
  on.exit(unlink(Sys.glob(paste0(pid_base, ".*"))), add = TRUE)

  expect_error(
    suppressMessages(
      parallelize_fun(
        1:2,
        function(x) {
          file.create(paste0(pid_base, ".", Sys.getpid()))
          Sys.sleep(5)
          x
        },
        cores = 2,
        backend = "psock",
        timeout = 0.5,
        verbose = FALSE
      )
    ),
    class = "parallelize_timeout"
  )

  worker_files <- Sys.glob(paste0(pid_base, ".*"))
  worker_pids <- as.integer(substring(worker_files, nchar(pid_base) + 2L))
  expect_length(worker_pids, 2L)
  expect_false(any(vapply(worker_pids, parallel_process_alive, logical(1))))
})

test_that("fatal workers fail promptly with a worker error", {
  local_parallel_test_workers()

  backends <- if (
    .Platform$OS.type == "windows" ||
      identical(Sys.getenv("R_COVR"), "true")
  ) {
    "psock"
  } else {
    c("fork", "psock")
  }

  for (backend in backends) {
    started <- proc.time()[["elapsed"]]
    expect_error(
      suppressMessages(
        parallelize_fun(
          1:2,
          function(x) {
            if (x == 1L) {
              if (.Platform$OS.type == "windows") {
                q(save = "no", status = 3L, runLast = FALSE)
              } else {
                tools::pskill(Sys.getpid(), tools::SIGKILL)
              }
            }
            Sys.sleep(5)
            x
          },
          cores = 2,
          backend = backend,
          timeout = 10,
          verbose = FALSE
        )
      ),
      class = "parallelize_worker_error"
    )
    expect_lt(proc.time()[["elapsed"]] - started, 8)
  }
})

test_that("single-core and parallel modes keep the same mixed-result contract", {
  local_parallel_test_workers()

  run_backend <- function(backend, verbose, cores = 2L) {
    suppressMessages(
      parallelize_fun(
        1:8,
        function(x) {
          if (x %in% c(3L, 7L)) stop("expected failure")
          if (x == 5L) return(NULL)
          x * 2L
        },
        cores = cores,
        backend = backend,
        verbose = verbose,
        throw_error = FALSE
      )
    )
  }

  sequential <- run_backend("auto", verbose = FALSE, cores = 1L)
  psock <- run_backend("psock", verbose = FALSE)
  progress <- run_backend("psock", verbose = TRUE)

  expect_identical(sequential, psock)
  expect_identical(psock, progress)
  expect_true(inherits(psock[[3L]], "parallelize_error"))
  expect_null(psock[[5L]])

  if (
    .Platform$OS.type != "windows" &&
      !identical(Sys.getenv("R_COVR"), "true")
  ) {
    fork <- run_backend("fork", verbose = FALSE)
    fork_progress <- run_backend("fork", verbose = TRUE)
    expect_identical(sequential, fork)
    expect_identical(fork, fork_progress)
  }
})

test_that("nested fork calls retain the outer call context", {
  skip_on_os("windows")
  local_parallel_test_workers()
  skip_if(
    identical(Sys.getenv("R_COVR"), "true"),
    "fork-specific nested-process test"
  )
  old_covr <- Sys.getenv("R_COVR", unset = NA_character_)
  on.exit({
    if (is.na(old_covr)) {
      Sys.unsetenv("R_COVR")
    } else {
      Sys.setenv(R_COVR = old_covr)
    }
  }, add = TRUE)
  Sys.unsetenv("R_COVR")

  result <- suppressMessages(
    parallelize_fun(
      1:4,
      function(i) {
        inner <- parallelize_fun(
          1:3,
          function(j) i + j,
          cores = 2,
          verbose = FALSE
        )
        unname(unlist(inner))
      },
      cores = 2,
      verbose = FALSE
    )
  )

  expect_identical(
    unname(result),
    list(2:4, 3:5, 4:6, 5:7)
  )
})

test_that("nested PSOCK calls resolve parallelize_fun in global closures", {
  local_parallel_test_workers()

  worker <- function(i) {
    unname(unlist(parallelize_fun(
      1:3,
      function(j) i + j,
      cores = 2,
      verbose = FALSE
    )))
  }
  environment(worker) <- globalenv()

  result <- suppressMessages(
    parallelize_fun(
      1:4,
      worker,
      cores = 2,
      backend = "psock",
      verbose = FALSE
    )
  )

  expect_identical(unname(result), list(2:4, 3:5, 4:6, 5:7))
})

test_that("an interrupted fork call cleans workers and can be followed by another call", {
  skip_on_os("windows")
  local_parallel_test_workers()
  skip_if(
    identical(Sys.getenv("R_COVR"), "true"),
    "fork-specific interrupt test"
  )
  old_covr <- Sys.getenv("R_COVR", unset = NA_character_)
  on.exit({
    setTimeLimit(cpu = Inf, elapsed = Inf, transient = FALSE)
    if (is.na(old_covr)) {
      Sys.unsetenv("R_COVR")
    } else {
      Sys.setenv(R_COVR = old_covr)
    }
  }, add = TRUE)
  Sys.unsetenv("R_COVR")

  setTimeLimit(elapsed = 0.5, transient = TRUE)
  interrupted <- tryCatch({
    parallelize_fun(
      1:4,
      function(i) {
        Sys.sleep(2)
        i
      },
      cores = 2,
      verbose = FALSE
    )
    NULL
  }, error = identity)
  setTimeLimit(cpu = Inf, elapsed = Inf, transient = FALSE)

  expect_s3_class(interrupted, "error")
  expect_length(getFromNamespace("children", "parallel")(), 0L)
  expect_identical(
    unname(unlist(parallelize_fun(1:3, identity, cores = 2, verbose = FALSE))),
    1:3
  )
})
