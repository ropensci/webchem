up <- ping_service("srs")

test_that("SRS returns correct results", {
  skip_on_cran()
  skip_if_not(up, "SRS is down")

  a_messages <- capture_messages(srs_query(NA))
  a <- srs_query(NA)
  b_messages <- capture_messages(srs_query("balloon"))
  b <- srs_query("balloon")
  c_messages <- capture_messages(srs_query("50-00-0", from = "cas"))
  c <- srs_query("50-00-0", from = "cas")
  d <- srs_query("aniline", from = "name")
  e <- srs_query(c("50-00-0", "balloon", NA), from = "cas")

  #newline to be fixed
  #expect_true(standard_string("na") %in% a_messages)
  expect_true(is.na(a))

  expect_true(standard_string("query", "balloon") %in% b_messages)
  expect_true("Internal Server Error (HTTP 500)." %in% b_messages)
  expect_true(is.na(b))

  expect_true(standard_string("query", "50-00-0") %in% c_messages)
  expect_true("OK (HTTP 200)." %in% c_messages)
  expect_is(c, "list")
  expect_is(c$`50-00-0`, "data.frame")
  expect_equal(c$`50-00-0`$systematicName, "Formaldehyde")

  expect_equal(d$aniline$systematicName, "Benzenamine")

  expect_equal(length(e), 3)
  expect_true(is.na(e$`balloon`))
  expect_true(is.na(e$`NA`))
})
