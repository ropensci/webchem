context("etox")

do <- get_etoxid('Triclosan')
xx <- get_etoxid('xxxxx')

test_that("get_etoxid returns correct results", {
  expect_equal(c(do), "20179")
  expect_equal(attr(do, "matched"), "Triclosan ( 20179 )")
  expect_equal(c(xx), NA)
})

do2 <- etox_basic('20179')
xx2 <- etox_basic('xxx')

test_that("etox_basic returns correct results", {
  expect_equal(do2$cas, "3380-34-5")
  expect_equal(length(do2), 4)
  expect_is(do2$synonyms, 'data.frame')
  expect_equal(xx2, NA)
})

do3 <- etox_targets('20179')
xx3 <- etox_targets('xxxx')

test_that("etox_basic returns correct results", {
  expect_equal(do3$Stoff[1], "Triclosan")
  expect_equal(ncol(do3), 32)
  expect_is(do3, 'data.frame')
  expect_equal(xx3, NA)
})

int1 <- etox_basic(do)
int2 <- etox_targets(do)
int3 <- etox_basic(xx)
int4 <- etox_targets(xx)
test_that("etox integration tests", {
  expect_equal(int1$cas, "3380-34-5")
  expect_equal(length(int1), 4)
  expect_is(int1$synonyms, 'data.frame')

  expect_equal(int2$Stoff[1], "Triclosan")
  expect_equal(ncol(int2), 32)
  expect_is(int2, 'data.frame')

  expect_equal(int3, NA)
  expect_equal(int4, NA)
})