context("physprop")

fl <- physprop('50-00-0')
xx <- physprop('xxxxx')

test_that("physprop returns correct results", {
  expect_equal(length(fl), 4)
  expect_equal(fl$cas, "50-00-0")
  expect_equal(xx, NA)
  expect_equal(names(fl$prop), c("variable", "value", "unit", "temp", "type", "ref"))
})



