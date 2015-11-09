context("utils")


test_that("is.cas() returns correct results", {
  expect_true(is.cas('64-17-5'))
  expect_false(is.cas('64175'))
  expect_false(is.cas('4-17-5'))
  expect_false(is.cas('64-177-6'))
  expect_false(is.cas('64-17-55'))
  expect_false(is.cas('64-17-6'))
})


test_that("is.inchikey() returns correct results", {
  expect_true(is.inchikey('BQJCRHHNABKAKU-KBQPJGBKSA-N'))
  expect_false(is.inchikey('BQJCRHHNABKAKU-KBQPJGBKSA'))
  expect_false(is.inchikey('BQJCRHHNABKAKU-KBQPJGBKSA-5'))
  expect_false(is.inchikey('BQJCRHHNABKAKU-KBQPJGBKSA-n'))
  expect_false(is.inchikey('BQJCRHHNABKAKU/KBQPJGBKSA/N'))
  expect_false(is.inchikey('BQJCRHHNABKAKU-KBQPJGBKXA-N'))
  expect_false(is.inchikey('BQJCRHHNABKAKU-KBQPJGBKSB-N'))
})