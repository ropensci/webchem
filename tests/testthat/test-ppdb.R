context('ppdb')

test_that("ppdb_buildidx", {
  y <- ppdb_buildidx()

  expect_is(y, 'data.frame')
  expect_equal(ncol(y), 2)

  # ppdb_idx returns no duplicates
  expect_false(any(duplicated(y$cas)))
})


test_that("ppdb", {
  xx <- ppdb_query('xxx')

  y <- ppdb_query('553-82-2')
  z <- ppdb_query('1327-53-3')

  expect_is(y, 'list')
  expect_is(z, 'list')
  expect_equal(length(y), 11)
  expect_equal(length(z), 11)

  expect_is(y$ec_regulation, 'data.frame')
  expect_equal(y$general[y$general$variable == 'CAS RN', 'value'], "553-82-2")
  expect_true(is.na(xx))
})