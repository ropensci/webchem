context("cir")

test_that("cir()", {
  expect_equal(cir_query('Triclosan', 'mw', verbose = FALSE), '289.5451')
  expect_error(cir_query(c('Triclosan', 'Benzol'), 'mw'))
  expect_equal(cir_query('xxxxxxx', 'mw', verbose = FALSE), NA)
  expect_warning(cir_query(NA, 'mw', verbose = FALSE))
  expect_equal(cir_query("3380-34-5", 'stdinchikey', resolver = 'cas_number', verbose = FALSE), "InChIKey=XEFQLINVKFYRCS-UHFFFAOYSA-N")
  expect_true(length(cir_query('Triclosan', 'cas', verbose = FALSE)) > 1)
  expect_equal(length(cir_query('Triclosan', 'cas', first = TRUE, verbose = FALSE)), 1)
})
