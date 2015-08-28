context("pan")

require(RCurl)
chk_pan <- function(){
  qurl <- 'http://www.pesticideinfo.org/List_Chemicals.jsp?ChemName=2,4-dichlorophenol'
  Sys.sleep(0.2)
  cont <- try(getURL(qurl, .encoding = 'UTF-8', .opts = list(timeout = 3)),
              silent = TRUE)
  if (inherits(cont, 'try-error'))
    skip("Server is down!")
}




test_that("pan()", {
  chk_pan()

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