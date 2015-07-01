context("allanwood")

fl <- allanwood('Fluazinam', type = 'commonname')
xx <- allanwood('xxxxx', type = 'commonname')

test_that("allanwood, commonname", {
  expect_equal(fl$cas, "79622-59-6", verbose = FALSE)
  expect_equal(xx, NA, verbose = FALSE)
  expect_equal(length(fl), 8)
})
