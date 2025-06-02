devtools::document()

test_matrix <- simulate_sparse_matrix(100, 100) |> as_matrix()
t1 <- inferCSN(test_matrix)
t2 <- inferCSN(test_matrix, cores = 6)
identical(t1, t2)

bench::mark(
  t1 <- inferCSN(test_matrix),
  t1f <- inferCSN(test_matrix, verbose = F),
  t2 <- inferCSN(test_matrix, cores = 4),
  t2f <- inferCSN(test_matrix, cores = 4, verbose = F),
  memory = F
)
identical(t1, t1f)
identical(t1, t2)
identical(t2, t2f)
