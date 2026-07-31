local_parallel_test_workers <- function(workers = 2L, .env = parent.frame()) {
  workers <- min(2L, as.integer(workers))
  testthat::local_mocked_bindings(
    cores_detect = function(cores, num_session) {
      min(as.integer(workers), as.integer(cores), as.integer(num_session))
    },
    .package = "thisutils",
    .env = .env
  )
}

parallel_fork_tests_enabled <- function() {
  .Platform$OS.type != "windows" &&
    !identical(Sys.getenv("R_COVR"), "true")
}

test_that("timed-out PSOCK tasks close worker connections", {
  local_parallel_test_workers()

  observed <- NULL
  warnings <- character()
  withCallingHandlers(
    {
      observed <- tryCatch(
        suppressMessages(
          parallelize_fun(
            1:2,
            function(x) {
              Sys.sleep(5)
              x
            },
            cores = 2,
            backend = "psock",
            timeout = 0.2,
            verbose = FALSE
          )
        ),
        error = identity
      )
      gc()
    },
    warning = function(w) {
      warnings <<- c(warnings, conditionMessage(w))
      invokeRestart("muffleWarning")
    }
  )

  expect_s3_class(observed, "parallelize_timeout")
  expect_false(any(grepl(
    "closing unused connection",
    warnings,
    fixed = TRUE
  )))
})

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

test_that("parallelize_fun handles empty and singleton inputs", {
  empty <- suppressMessages(
    parallelize_fun(integer(), identity, cores = 8, verbose = FALSE)
  )
  singleton <- suppressMessages(
    parallelize_fun(
      list(list(value = 1L)),
      identity,
      cores = 8,
      backend = "psock",
      verbose = FALSE
    )
  )

  expect_identical(empty, list())
  expect_identical(singleton, list(list(value = 1L)))
})

test_that("PSOCK matches sequential results for heterogeneous inputs", {
  local_parallel_test_workers(workers = 4L)
  x <- list(
    1:5,
    c(NA_real_, NaN, Inf, -Inf, 0),
    c("a", NA_character_, "\u4E2D\u6587"),
    as.raw(c(0, 127, 255)),
    matrix(seq_len(12), nrow = 3),
    data.frame(a = 1:3, b = c(TRUE, FALSE, NA)),
    NULL
  )
  worker <- function(value) {
    list(
      class = class(value),
      type = typeof(value),
      length = length(value),
      value = value
    )
  }

  sequential <- suppressMessages(
    parallelize_fun(x, worker, cores = 1, verbose = FALSE)
  )
  psock <- suppressMessages(
    parallelize_fun(
      x,
      worker,
      cores = 4,
      backend = "psock",
      verbose = FALSE
    )
  )

  expect_identical(psock, sequential)
})

test_that("PSOCK preserves order at a larger task count", {
  local_parallel_test_workers(workers = 4L)
  x <- seq_len(4096L)

  result <- suppressMessages(
    parallelize_fun(
      x,
      function(i) as.integer((i * 17L) %% 997L),
      cores = 4,
      backend = "psock",
      verbose = FALSE
    )
  )

  expect_identical(
    unname(unlist(result, use.names = FALSE)),
    as.integer((x * 17L) %% 997L)
  )
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

test_that("parallel task chunks are bounded and preserve every input index", {
  chunks <- parallel_task_chunks(total = 1000L, cores = 4L, timeout = Inf)

  expect_lte(length(chunks), 16L)
  expect_identical(unlist(chunks, use.names = FALSE), seq_len(1000L))
  expect_true(all(lengths(chunks) > 0L))
})

test_that("finite task timeouts disable batching", {
  chunks <- parallel_task_chunks(total = 20L, cores = 4L, timeout = 1)

  expect_length(chunks, 20L)
  expect_identical(unlist(chunks, use.names = FALSE), seq_len(20L))
  expect_true(all(lengths(chunks) == 1L))
})

test_that("seed produces identical streams across worker counts", {
  local_parallel_test_workers(workers = 4L)

  worker <- function(i) c(runif(3), rnorm(2))
  sequential <- suppressMessages(
    parallelize_fun(1:40, worker, cores = 1, seed = 123, verbose = FALSE)
  )
  psock_two <- suppressMessages(
    parallelize_fun(
      1:40,
      worker,
      cores = 2,
      backend = "psock",
      seed = 123,
      verbose = FALSE
    )
  )
  psock_four <- suppressMessages(
    parallelize_fun(
      1:40,
      worker,
      cores = 4,
      backend = "psock",
      seed = 123,
      verbose = FALSE
    )
  )

  expect_identical(sequential, psock_two)
  expect_identical(psock_two, psock_four)
  expect_equal(length(unique(vapply(
    psock_four,
    function(value) value[[1L]],
    numeric(1)
  ))), 40L)

  if (parallel_fork_tests_enabled()) {
    fork <- suppressMessages(
      parallelize_fun(
        1:40,
        worker,
        cores = 4,
        backend = "fork",
        seed = 123,
        verbose = FALSE
      )
    )
    expect_identical(sequential, fork)
  }
})

test_that("seed restores the caller random-number state", {
  set.seed(987)
  state_before <- get(".Random.seed", envir = globalenv(), inherits = FALSE)

  suppressMessages(
    parallelize_fun(
      1:20,
      function(i) runif(5),
      cores = 2,
      backend = "psock",
      seed = 42,
      verbose = FALSE
    )
  )

  expect_identical(
    get(".Random.seed", envir = globalenv(), inherits = FALSE),
    state_before
  )
})

test_that("seeded results remain stable when some inputs fail", {
  local_parallel_test_workers(workers = 4L)
  worker <- function(i) {
    value <- c(runif(2), rnorm(2))
    if (i %% 11L == 0L) {
      stop("expected seeded failure")
    }
    value
  }

  sequential <- suppressMessages(
    parallelize_fun(
      1:500,
      worker,
      cores = 1,
      seed = 20260731,
      throw_error = FALSE,
      verbose = FALSE
    )
  )
  psock <- suppressMessages(
    parallelize_fun(
      1:500,
      worker,
      cores = 4,
      backend = "psock",
      seed = 20260731,
      throw_error = FALSE,
      verbose = FALSE
    )
  )

  expect_identical(psock, sequential)
  expect_identical(
    unname(which(vapply(psock, inherits, logical(1), "parallelize_error"))),
    which(seq_len(500L) %% 11L == 0L)
  )
})

test_that("seed validation rejects invalid values", {
  expect_error(
    parallelize_fun(1:2, identity, seed = NA, verbose = FALSE),
    "seed must be NULL or a single integer"
  )
  expect_error(
    parallelize_fun(1:2, identity, seed = c(1, 2), verbose = FALSE),
    "seed must be NULL or a single integer"
  )
  expect_error(
    parallelize_fun(1:2, identity, seed = 1.5, verbose = FALSE),
    "seed must be NULL or a single integer"
  )
})

test_that("parallel worker liveness checks do not terminate the process", {
  cl <- parallel::makePSOCKcluster(1L)
  on.exit(try(parallel::stopCluster(cl), silent = TRUE), add = TRUE)
  worker_pid <- parallel::clusterCall(cl, Sys.getpid)[[1L]]

  expect_true(parallel_process_alive(worker_pid))
  expect_identical(parallel::clusterCall(cl, function() TRUE), list(TRUE))
})

test_that("PSOCK launcher temporary files are scoped and removed", {
  skip_if(.Platform$OS.type != "windows")
  local_parallel_test_workers()

  old_tmpdir <- Sys.getenv("TMPDIR", unset = NA_character_)
  before <- Sys.glob(file.path(tempdir(), "thisutils-psock-*"))
  result <- suppressMessages(
    parallelize_fun(
      1:2,
      identity,
      cores = 2,
      backend = "psock",
      verbose = FALSE
    )
  )

  expect_identical(unname(unlist(result)), 1:2)
  expect_identical(
    Sys.getenv("TMPDIR", unset = NA_character_),
    old_tmpdir
  )
  expect_setequal(
    Sys.glob(file.path(tempdir(), "thisutils-psock-*")),
    before
  )
})

test_that("the auto backend always uses PSOCK", {
  local_parallel_test_workers()

  expect_identical(parallel_backend("auto"), "psock")
  main_pid <- Sys.getpid()
  worker_pids <- suppressMessages(
    parallelize_fun(
      1:4,
      function(x) Sys.getpid(),
      cores = 2,
      verbose = FALSE
    )
  )

  expect_true(all(vapply(worker_pids, is.integer, logical(1))))
  expect_false(main_pid %in% unlist(worker_pids, use.names = FALSE))
})

test_that("parallelize_fun resolves a supplied closure before PSOCK serialization", {
  local_parallel_test_workers()

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
  skip_on_covr()
  local_parallel_test_workers()

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

test_that("the fork backend is rejected clearly on Windows", {
  skip_if(.Platform$OS.type != "windows")

  expect_error(
    parallel_backend("fork"),
    "fork backend is unavailable on Windows"
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

test_that("parallel calls fail within the total timeout", {
  local_parallel_test_workers()

  started <- proc.time()[["elapsed"]]
  expect_error(
    suppressMessages(
      parallelize_fun(
        1:20,
        function(x) {
          Sys.sleep(0.2)
          x
        },
        cores = 2,
        backend = "psock",
        total_timeout = 0.5,
        verbose = FALSE
      )
    ),
    class = "parallelize_total_timeout"
  )
  expect_lt(proc.time()[["elapsed"]] - started, 10)
})

test_that("single-core total timeout is enforced between inputs", {
  started <- proc.time()[["elapsed"]]
  expect_error(
    suppressMessages(
      parallelize_fun(
        1:10,
        function(x) {
          Sys.sleep(0.1)
          x
        },
        cores = 1,
        total_timeout = 0.25,
        verbose = FALSE
      )
    ),
    class = "parallelize_total_timeout"
  )
  expect_lt(proc.time()[["elapsed"]] - started, 2)
})

test_that("total timeout validates its input", {
  expect_error(
    parallelize_fun(1:2, identity, total_timeout = 0, verbose = FALSE),
    "total_timeout must be a positive number or Inf"
  )
})

test_that("the earliest timeout deadline determines the error class", {
  local_parallel_test_workers()

  task_first <- tryCatch(
    suppressMessages(
      parallelize_fun(
        1:2,
        function(x) {
          Sys.sleep(5)
          x
        },
        cores = 2,
        backend = "psock",
        timeout = 0.2,
        total_timeout = 10,
        verbose = FALSE
      )
    ),
    error = identity
  )
  total_first <- tryCatch(
    suppressMessages(
      parallelize_fun(
        1:2,
        function(x) {
          Sys.sleep(5)
          x
        },
        cores = 2,
        backend = "psock",
        timeout = 10,
        total_timeout = 0.2,
        verbose = FALSE
      )
    ),
    error = identity
  )

  expect_s3_class(task_first, "parallelize_timeout")
  expect_s3_class(total_first, "parallelize_total_timeout")
})

test_that("total timeouts leave no worker processes", {
  local_parallel_test_workers()

  pid_base <- tempfile("thisutils-total-timeout-worker-")
  on.exit(unlink(Sys.glob(paste0(pid_base, ".*"))), add = TRUE)
  worker <- local({
    path <- pid_base
    function(x) {
      file.create(paste0(path, ".", Sys.getpid()))
      Sys.sleep(10)
      x
    }
  })

  expect_error(
    suppressMessages(
      parallelize_fun(
        1:4,
        worker,
        cores = 2,
        backend = "psock",
        total_timeout = 2,
        verbose = FALSE
      )
    ),
    class = "parallelize_total_timeout"
  )

  worker_files <- Sys.glob(paste0(pid_base, ".*"))
  worker_pids <- as.integer(substring(worker_files, nchar(pid_base) + 2L))
  expect_gte(length(worker_pids), 1L)
  expect_false(any(vapply(worker_pids, parallel_process_alive, logical(1))))
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

  backends <- if (parallel_fork_tests_enabled()) {
    c("fork", "psock")
  } else {
    "psock"
  }

  for (backend in backends) {
    pid_base <- tempfile(paste0("thisutils-fatal-", backend, "-worker-"))
    on.exit(unlink(Sys.glob(paste0(pid_base, ".*"))), add = TRUE)
    started <- proc.time()[["elapsed"]]
    expect_error(
      suppressMessages(
        parallelize_fun(
          1:2,
          function(x) {
            file.create(paste0(pid_base, ".", Sys.getpid()))
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

    worker_files <- Sys.glob(paste0(pid_base, ".*"))
    worker_pids <- as.integer(substring(worker_files, nchar(pid_base) + 2L))
    expect_gte(length(worker_pids), 1L)
    expect_false(any(vapply(
      worker_pids,
      parallel_process_alive,
      logical(1)
    )))
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

  if (parallel_fork_tests_enabled()) {
    fork <- run_backend("fork", verbose = FALSE)
    fork_progress <- run_backend("fork", verbose = TRUE)
    expect_identical(sequential, fork)
    expect_identical(fork, fork_progress)
  }
})

test_that("nested fork calls retain the outer call context", {
  skip_on_os("windows")
  skip_on_covr()
  local_parallel_test_workers()

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

test_that("worker depth is scoped to each task", {
  local_parallel_test_workers()
  old_options <- options(thisutils.parallel.depth = NULL)
  on.exit(options(old_options), add = TRUE)

  depths <- suppressMessages(
    parallelize_fun(
      1:8,
      function(i) {
        depth <- getOption("thisutils.parallel.depth", 0L)
        options(thisutils.parallel.depth = 99L)
        depth
      },
      cores = 2,
      backend = "psock",
      verbose = FALSE
    )
  )

  expect_identical(unname(unlist(depths)), rep(1L, 8L))
  expect_null(getOption("thisutils.parallel.depth"))
})

test_that("an interrupted fork call cleans workers and can be followed by another call", {
  skip_on_os("windows")
  skip_on_covr()
  local_parallel_test_workers()
  on.exit(
    setTimeLimit(cpu = Inf, elapsed = Inf, transient = FALSE),
    add = TRUE
  )

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
