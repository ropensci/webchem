context("etox")

do <- get_etoxid('Triclosan')
xx <- get_etoxid('xxxxx')

test_that("get_etoxid returns correct results", {
  expect_equal(c(do), "20179")
  expect_equal(attr(do, "matched"), "Triclosan ( 20179 )")
  expect_equal(c(xx), NA)
})



