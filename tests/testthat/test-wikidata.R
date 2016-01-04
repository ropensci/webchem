context("wikidata")


test_that("get_wdid returns correct results", {
  do <- get_wdid('Triclosan', language = 'en')
  do2 <- get_wdid('ddt', language = 'en')
  do3 <- get_wdid('Triclosan', language = 'en', first = TRUE)
  xx <- get_wdid('xxxxxx', language = 'en')

  expect_error(get_wdid(c('Triclosan', 'xxx'), language = 'en'))
  expect_equal(c(do), "Q408646")
  expect_equal(attr(do, "matched"), "Triclosan")
  expect_true(length(do2) > 1)
  expect_true(length(do3) == 1)
  expect_equal(c(xx), NA)
})


test_that("wd_ident returns correct results", {
  id <- "Q407232" # glyphosate
  d <- wd_ident(id)
  f <- wd_ident('xxxxx')

  expect_error(wd_ident(c(id, id)))
  expect_equal(d$cas, "3380-34-5")
  expect_equal(length(d), 12)
  expect_equal(f, NA)
  expect_message(wd_ident('xxxxx'))
})


test_that("wd integration test", {
  d <- wd_ident(get_wdid('Glyphosate', 'en', TRUE))
  f <- wd_ident(get_wdid('xxxxx', 'en', TRUE))

  expect_equal(d$cas, "3380-34-5")
  expect_equal(length(d), 12)
  expect_equal(f, NA)
})