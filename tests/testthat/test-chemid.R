context("chemid")


test_that("chemid returns correct results", {
  xx <- chemid('xxxxx', type = 'name')
  fl <- chemid('50-00-0', type = 'rn')

  expect_equal(fl$cas, "50-00-0")
  expect_equal(fl$name[1], "Formaldehyde [USP]")
  expect_equal(fl$physprop$Value[fl$physprop[ , 1] == 'Water Solubility'], 400000)
  expect_equal(length(fl), 8)
  expect_true(is.data.frame(fl$physprop))
  expect_equal(xx, NA)
})



