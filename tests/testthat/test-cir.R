context("cir")

test_that("cir_query()", {
  skip_on_cran()

  Sys.sleep(5)
  expect_equal(cir_query('Triclosan', 'mw', verbose = FALSE)[[1]], 289.5451)
  Sys.sleep(5)
  expect_equal(cir_query('xxxxxxx', 'mw', verbose = FALSE)[[1]], NA)
  Sys.sleep(5)
  expect_equal(cir_query("3380-34-5", 'stdinchikey', resolver = 'cas_number', verbose = FALSE)[[1]],
            "InChIKey=XEFQLINVKFYRCS-UHFFFAOYSA-N")
  Sys.sleep(5)
  expect_true(length(cir_query('Triclosan', 'cas', verbose = FALSE)[[1]]) > 1)
  Sys.sleep(5)
  expect_equal(length(cir_query('Triclosan', 'cas', first = TRUE, verbose = FALSE)[[1]]), 1)
  Sys.sleep(5)
  expect_equal(length(cir_query(c('Triclosan', 'Aspirin'), 'cas', verbose = FALSE)), 2)

  Sys.sleep(5)
  expect_equal(cir_query('acetic acid', 'mw', first = TRUE), c(`acetic acid` = 60.0524))
})
