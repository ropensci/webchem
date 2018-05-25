context("opsin")


test_that("opsin_query()", {
  skip_on_cran()

  o1 <- opsin_query(c('Cyclopropane', 'Octane'))
  o2 <- opsin_query(c('xxxx'))

  # issue #146
  b1 <- opsin_query('Acetic acid')
  expect_equal(b1$query, 'Acetic acid')

  expect_is(o1, 'data.frame')
  expect_equal(ncol(o1), 6)
  expect_equal(ncol(o2), 6)
  expect_equal(nrow(o1), 2)
  expect_equal(nrow(o2), 1)
  expect_equal(o1$query, c('Cyclopropane', 'Octane'))
  expect_equal(o2$query, c('xxxx'))
})


