context("etox")

do <- get_etoxid('Triclosan')
xx <- get_etoxid('xxxxx')

test_that("get_etoxid returns correct results", {
  expect_equal(c(do), "20179")
  expect_equal(attr(do, "matched"), "Triclosan ( 20179 )")
  expect_equal(c(xx), NA)
})

do2 <- etox_basic(do)
xx2 <- etox_basic('xxx')

test_that("etox_basic returns correct results", {
  expect_equal(do2$cas, "3380-34-5")
  expect_equal(length(do2), 4)
  expect_is(do2$synonyms, 'data.frame')
  expect_equal(xx2, NA)
})

