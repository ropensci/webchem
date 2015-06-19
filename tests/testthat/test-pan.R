context("pan")

check_api <- function() {
  if (is.na(pan('2,4-dichlorophenol', verbose = FALSE))) {
    skip("API not available")
  }
}

test_that("pan()", {
  check_api()
  expect_error(pan(c('xxxxx', 'aaaaaaa')))
  expect_warning(pan(NA))
  expect_equal(pan('xxxxx', verbose = FALSE), NA)
  expect_equal(length(pan('2,4-dichlorophenol', verbose = FALSE)), 73)
  expect_equal(pan('Triclosan', verbose = FALSE)[[2]], "3380-34-5")
  expect_equal(length(pan('Chlorpyrifos', verbose = FALSE, match = 'all')[[1]]), 9)
  expect_equal(length(pan('Chlorpyrifos', verbose = FALSE, match = 'first')[[1]]), 1)
  expect_equal(length(pan('Chlorpyrifos', verbose = FALSE, match = 'best')[[1]]), 1)
  expect_equal(pan('Chlorpyrifos', verbose = FALSE, match = 'best')[[1]], "Chlorpyrifos\nChlorpyrifos")
})