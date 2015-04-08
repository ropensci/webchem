context("cts")

test_that("cts_compinfo()", {
  expect_error(cts_compinfo(c('xxxxx', 'aaaaaaa')))
  expect_equal(cts_compinfo('xxxxx'), NA)
  expect_equal(length(cts_compinfo("XEFQLINVKFYRCS-UHFFFAOYSA-N")), 7)
  expect_equal(round(cts_compinfo("XEFQLINVKFYRCS-UHFFFAOYSA-N")[[3]], 4), 289.5418)
})


test_that("cts_convert()", {
  expect_error(cts_convert(c('xxxxx', 'aaaaaaa'), 'Chemical Name', 'CAS'))
  expect_error(cts_convert('Triclosan', c('Chemical Name', 'CAS'), 'CAS'))
  expect_error(cts_convert('Triclosan', 'CAS'))
  expect_equal(cts_convert('Triclosan', 'Chemical Name', 'CAS')[1], '105093-20-7')
  expect_equal(cts_convert('xxxxxx', 'Chemical Name', 'CAS'), NA)
})

# combine cts_compinfo with others to retrieve inchi