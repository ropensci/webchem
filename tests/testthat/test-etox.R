context("etox")

test_that("get_etoxid returns correct results", {
  do <- get_etoxid('Triclosan')
  xx <- get_etoxid('xxxxx')

  expect_equal(c(do), "20179")
  expect_equal(attr(do, "matched"), "Triclosan ( 20179 )")
  expect_equal(c(xx), NA)
})


test_that("etox_basic returns correct results", {
  do2 <- etox_basic('20179')
  xx2 <- etox_basic('xxx')

  expect_equal(do2$cas, "3380-34-5")
  expect_equal(length(do2), 4)
  expect_is(do2$synonyms, 'data.frame')
  expect_equal(xx2, NA)
})


test_that("etox_targets returns correct results", {
  do3 <- etox_targets('20179')
  xx3 <- etox_targets('xxxx')

  expect_equal(do3$Substance[1], "Triclosan")
  expect_equal(ncol(do3), 32)
  expect_is(do3, 'data.frame')
  expect_equal(xx3, NA)
})


test_that("etox integration tests", {
  do <- get_etoxid('Triclosan')
  xx <- get_etoxid('xxxxx')
  int1 <- etox_basic(do)
  int2 <- etox_targets(do)
  int3 <- etox_basic(xx)
  int4 <- etox_targets(xx)

  expect_equal(int1$cas, "3380-34-5")
  expect_equal(length(int1), 4)
  expect_is(int1$synonyms, 'data.frame')

  expect_equal(int2$Substance[1], "Triclosan")
  expect_equal(ncol(int2), 32)
  expect_is(int2, 'data.frame')

  expect_equal(int3, NA)
  expect_equal(int4, NA)
})