up <- ping_service("srs")

test_that("SRS returns correct results", {
  skip_on_cran()
  skip_if_not(up, "SRS is down")

  a <- srs_query(NA)
  b <- srs_query("balloon")
  c <- srs_query("50-00-0", from = "cas")
  d <- srs_query("aniline", from = "name")
  e <- srs_query(c("50-00-0", "balloon", NA), from = "cas")

  expect_true(is.na(a))
  expect_true(is.na(b))
  expect_type(c, "list")
  expect_s3_class(c$`50-00-0`, "data.frame")
  expect_equal(c$`50-00-0`$systematicName, "Formaldehyde")

  expect_equal(d$aniline$systematicName, "Benzenamine")

  expect_equal(length(e), 3)
  expect_true(is.na(e$`balloon`))
  expect_true(is.na(e$`NA`))
})
