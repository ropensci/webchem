context('ppdb')

test_that("ppdb_buildidx", {
  y <- ppdb_buildidx()

  expect_is(y, 'data.frame')
  expect_equal(ncol(y), 2)
})

test_that("ppdb_query", {
  y <- ppdb_query('1071-83-6')
  xx <- ppdb_query('xxx')

  expect_is(y, 'list')
  expect_true(is.na(xx))
})