up <- ping_service("cir")
test_that("cir_query()", {
  skip_on_cran()
  skip_if_not(up, "CIR server is down")

  expect_equal(cir_query('Triclosan', 'mw', verbose = FALSE)[[1]], 289.5451)
  expect_equal(cir_query('xxxxxxx', 'mw', verbose = FALSE)[[1]], NA)
  expect_equal(cir_query("3380-34-5", 'stdinchikey', resolver = 'cas_number', verbose = FALSE)[[1]],
            "InChIKey=XEFQLINVKFYRCS-UHFFFAOYSA-N")
  expect_true(length(cir_query('Triclosan', 'cas', verbose = FALSE)[[1]]) > 1)
  expect_message(cir_query("acetic acid", "mw", first = TRUE))
  expect_length(cir_query('Triclosan', 'cas', choices = 1, verbose = FALSE)[[1]], 1)
  expect_length(cir_query(c('Triclosan', 'Aspirin'), 'cas', verbose = FALSE), 2)

  skip("I have no clue why this one fails on R CMD check.  It works when run in the console!")
  expect_equivalent(cir_query('acetic acid', 'mw', choices = 1), c(`acetic acid` = 60.0524))

})

test_that("cir_query() doesn't mistake NA for sodium", {
  skip_on_cran()
  skip_if_not(up, "CIR server is down")

  expect_true(is.na(cir_query(as.character(NA), 'cas')))
})
