context("cir")

test_that("cir()", {
  expect_equal(cir_query('Triclosan', 'mw'), '289.5451')
  expect_error(cir_query(c('Triclosan', 'Benzol'), 'mw'))
  expect_warning(cir_query('xxxxxxx', 'mw'))
  expect_equal(length(cir_query('Triclosan', 'cas')), 3)
  expect_equal(length(cir_query('Triclosan', 'cas'), first = TRUE), 1)
})
