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




test_that("pan_query()", {
  chk_pan()

  expect_error(pan_query(c('xxxxx', 'aaaaaaa')))
  expect_warning(pan_query(NA))
  expect_equal(pan_query('xxxxx', verbose = FALSE), NA)
  expect_equal(length(pan_query('2,4-dichlorophenol', verbose = FALSE)), 73)
  expect_equal(pan_query('Triclosan', verbose = FALSE)[[2]], "3380-34-5")
  expect_equal(length(pan_query('Chlorpyrifos', verbose = FALSE, match = 'all')[[1]]), 9)
  expect_equal(length(pan_query('Chlorpyrifos', verbose = FALSE, match = 'first')[[1]]), 1)
  expect_equal(length(pan_query('Chlorpyrifos', verbose = FALSE, match = 'best')[[1]]), 1)
  expect_equal(pan_query('Chlorpyrifos', verbose = FALSE, match = 'best')[[1]], "Chlorpyrifos\nChlorpyrifos")
})