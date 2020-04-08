context("smartsview")

a <- smarts_viewer("[CX3](=[OX1])[OX2][CX3](=[OX1])", "image", "png", 1, "both" )
b <- smarts_viewer("[CX3](=[OX1])[OX2][CX3](=[OX1])", "download", "png", 1, "both", "smartsview_test.png" )

test_that("nist returns correct results", {
  skip_on_cran()

  expect_is(a[[1]], 'array')
  expect_equal(b[[1]], 0)
  expect_true(file.exists('smartsview_test.png'))
  expect_error(smarts_viewer("[CX3](=[OX1])[OX2][CX3](=[OX1])", "download", "png", 1, "both"))
})