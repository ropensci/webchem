# EXAMPLE VCR USAGE: RUN AND DELETE ME

foo <- function() crul::ok('https://httpbin.org/get')

test_that("foo works", {
  vcr::use_cassette("testing", {
    x <- foo()
  })
  expect_true(x)
})
