context("utils")


test_that("is.cas() returns correct results", {
  expect_true(is.cas('64-17-5'))
  expect_false(is.cas('64175'))
  expect_false(is.cas('4-17-5'))
  expect_false(is.cas('64-177-6'))
  expect_false(is.cas('64-17-55'))
  expect_false(is.cas('64-17-6'))
})
