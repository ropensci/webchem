context("comptox")

test_that("webtest_query()", {
  skip_on_cran()
  expect_s3_class(webtest_query("test"), "data.frame")
  expect_equal(webtest_query("CCO")$endpoint,
               "Fathead minnow LC50 (96 hr)")
  expect_equal(webtest_query("CCO", endpoint = "BCF")$endpoint,
               "Bioaccumulation factor")
  expect_equal(webtest_query("CCO", endpoint = "WS")$endpoint,
               "Water solubility at 25°C")
  expect_equal(webtest_query("Clc2cc(Cl)ccc2Oc1ccc(Cl)cc1O")$query,
               "Clc2cc(Cl)ccc2Oc1ccc(Cl)cc1O")
  expect_equal(nrow(webtest_query(c("CCO", "O=C(O)CNCP(=O)(O)O", "blub"))), 3)
  expect_equal(webtest_query(c("CCO", "blub"))$predictionTime[2], NA_real_)
})