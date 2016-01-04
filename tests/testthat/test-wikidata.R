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