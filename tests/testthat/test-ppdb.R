context('ppdb')

test_that("ppdb_buildidx", {
  y <- ppdb_buildidx()

  expect_is(y, 'data.frame')
  expect_equal(ncol(y), 2)
})

test_that("ppdb", {
  y <- ppdb('1071-83-6')
  z <- ppdb('50-00-0')
  xx <- ppdb('xxx')

  expect_is(y, 'list')
  expect_is(z, 'list')
  expect_equal(length(y), 10)
  expect_equal(length(z), 10)

  expect_is(y$ec_regulation, 'data.frame')
  expect_equal(y$general[y$general$variable == 'CAS RN', 'value'], "1071-83-6")
  expect_true(is.na(xx))
})