context('ppdb')

test_that("ppdb_buildidx", {
  y <- ppdb_buildidx()

  expect_is(y, 'data.frame')
  expect_equal(ncol(y), 2)

  # ppdb_idx returns no duplicates
  expect_false(any(duplicated(y$cas)))
})


test_that("ppdb", {
  y <- ppdb_query('1071-83-6')
  z <- ppdb_query('50-00-0')
  xx <- ppdb_query('xxx')

  b1 <- ppdb_query('553-82-2') # BUG: failed because of dups in ppdb_idx
  b2 <- ppdb_query('1327-53-3') # BUG: failed because wrong encoding in website

  expect_is(y, 'list')
  expect_is(z, 'list')
  expect_is(b1, 'list')
  expect_is(b2, 'list')
  expect_equal(length(y), 10)
  expect_equal(length(z), 10)
  expect_equal(length(b1), 10)
  expect_equal(length(b2), 10)

  expect_is(y$ec_regulation, 'data.frame')
  expect_equal(y$general[y$general$variable == 'CAS RN', 'value'], "1071-83-6")
  expect_true(is.na(xx))
})