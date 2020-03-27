context("nist")

a <- nist_ri("78-70-6")
b <- nist_ri("78-70-6", type = 'linear', polarity = 'non-polar')
c <- nist_ri("78-70-6", type = 'linear', temp_prog = 'custom')

test_that("nist returns correct results", {
  skip_on_cran()

  expect_is(a, 'data.frame')
  expect_is(b, 'data.frame')
  expect_is(c, 'data.frame')

  expect_equal(names(a)[1], 'CAS')
  expect_equal(names(b)[1], 'CAS')
  expect_equal(names(c)[1], 'CAS')

  expect_warning(nist_ri("78-70-6", type = 'linear', polarity = 'non-polar'))
})