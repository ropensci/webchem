context("etox")

require(RCurl)
chk_etox <- function(){
  qurl <- 'http://webetox.uba.de/webETOX/public/basics/stoff.do?id=20179'
  Sys.sleep(0.2)
  cont <- try(getURL(qurl, .encoding = 'UTF-8', .opts = list(timeout = 3)),
              silent = TRUE)
  if (inherits(cont, 'try-error'))
    skip("Server is down!")
}

test_that("get_etoxid returns correct results", {
  chk_etox()

  do <- get_etoxid('Triclosan', mult = 'best')
  do2 <- get_etoxid('Thiamethoxam')

  xx <- get_etoxid('xxxxx')

  expect_error(get_etoxid(c('Triclosan', 'xxx')))
  expect_equal(c(do), "20179")
  expect_equal(c(do2), "98867")
  expect_equal(attr(do, "matched"), "Triclosan ( 20179 )")
  expect_equal(attr(do2, "distance"), 'direct match')
  expect_equal(c(xx), NA)

  # only synonyms found
  expect_warning(get_etoxid('Tetracyclin'))
  # test multiple hits
  m1 <- get_etoxid('Triclosan', mult = 'all')
    expect_true(length(m1) > 1)
    expect_true(length(attr(m1, 'matched')) > 1)
    expect_equal(attr(m1, 'd'), 'all')
  m2 <- get_etoxid('Triclosan', mult = 'na')
    expect_equal(c(m2), NA)
    expect_equal(attr(m2, 'd'), NA)
  m3 <- get_etoxid('Triclosan', mult = 'first')
    expect_true(length(m3) == 1)
    expect_true(length(attr(m3, 'matched')) == 1)
    expect_equal(c(m3), "20179")
    expect_equal(attr(m3, 'd'), 'first')
    # get_etoxid('Triclosan', mult = 'ask')

})


test_that("etox_basic returns correct results", {
  chk_etox()

  do2 <- etox_basic('20179')
  xx2 <- etox_basic('xxx')
  expect_equal(etox_basic(NA), NA)

  expect_error(etox_basic(c('20179', 'xxx')))
  expect_equal(do2$cas, "3380-34-5")
  expect_equal(length(do2), 5)
  expect_is(do2$synonyms, 'data.frame')
  expect_equal(xx2, NA)
})


test_that("etox_targets returns correct results", {
  chk_etox()

  do3 <- etox_targets('20179')
  xx3 <- etox_targets('xxxx')
  xxx3 <- etox_targets('9051')
  expect_equal(etox_targets(NA), NA)

  expect_error(etox_targets(c('20179', 'xxx')))
  expect_equal(do3$res$Substance[1], "Triclosan")
  expect_equal(ncol(do3$res), 32)
  expect_is(do3, 'list')
  expect_equal(xx3, NA)
  expect_equal(xxx3, NA)
})

test_that("etox_tests returns correct results", {
  chk_etox()

  do4 <- etox_tests('20179')
  xx4 <- etox_tests('xxxx')
  expect_equal(etox_tests(NA), NA)

  expect_error(etox_tests(c('20179', 'xxx')))
  expect_equal(do4$res$Substance[1], "Triclosan")
  expect_equal(ncol(do4$res), 41)
  expect_is(do4$res, 'data.frame')
  expect_equal(length(do4), 2)
  expect_equal(xx4, NA)
})


test_that("etox integration tests", {
  chk_etox()

  do <- get_etoxid('Triclosan', mult = 'best')
  xx <- get_etoxid('xxxxx')

  int1 <- etox_basic(do)
  int2 <- etox_targets(do)
  int5 <- etox_tests(do)

  int3 <- etox_basic(xx)
  int4 <- etox_targets(xx)
  int6 <- etox_tests(xx)

  expect_equal(int1$cas, "3380-34-5")
  expect_equal(length(int1), 5)
  expect_is(int1$synonyms, 'data.frame')

  expect_equal(int2$res$Substance[1], "Triclosan")
  expect_equal(length(int2),2)
  expect_equal(ncol(int2$res), 32)
  expect_is(int2$res, 'data.frame')

  expect_equal(int5$res$Substance[1], "Triclosan")
  expect_equal(ncol(int5$res), 41)
  expect_is(int5$res, 'data.frame')

  expect_equal(int3, NA)
  expect_equal(int4, NA)
  expect_equal(int6, NA)
})