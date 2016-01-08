context("cts")

require(RCurl)
chk_cts <- function(){
  qurl <- 'http://cts.fiehnlab.ucdavis.edu/service/compound/XEFQLINVKFYRCS-UHFFFAOYSA-N'
  Sys.sleep(0.3)
  cont <- try(getURL(qurl, .encoding = 'UTF-8', .opts = list(timeout = 3)),
              silent = TRUE)
  if (inherits(cont, 'try-error'))
    skip("Server is down!")
}

chk_cir <- function(){
  qurl <- 'http://cactus.nci.nih.gov/chemical/structure/Triclosan/cas/xml'
  Sys.sleep(0.3)
  cont <- try(getURL(qurl, .encoding = 'UTF-8', .opts = list(timeout = 3)),
              silent = TRUE)
  if (inherits(cont, 'try-error'))
    skip("Server is down!")
}


test_that("cts_compinfo()", {
  chk_cts()
  expect_error(cts_compinfo(c('xxxxx', 'aaaaaaa')))
  expect_error(cts_compinfo('xxx'))
  expect_equal(cts_compinfo("XEFQLINVKFYRCS-UHFFFAOYSA-X", verbose = FALSE), NA)
  expect_warning(cts_compinfo("XEFQLINVKFYRCS-UHFFFAOYSA-X", verbose = FALSE))
  expect_equal(length(cts_compinfo("XEFQLINVKFYRCS-UHFFFAOYSA-N", verbose = FALSE)), 7)
  expect_equal(round(cts_compinfo("XEFQLINVKFYRCS-UHFFFAOYSA-N", verbose = FALSE)[[3]], 3), 289.542)
})


test_that("cts_convert()", {
  chk_cts()
  expect_error(cts_convert(c('xxxxx', 'aaaaaaa'), 'Chemical Name', 'CAS'))
  expect_error(cts_convert('Triclosan', c('Chemical Name', 'CAS'), 'CAS'))
  expect_error(cts_convert('Triclosan', 'CAS'))
  expect_equal(cts_convert('Triclosan', 'Chemical Name', 'inchikey', first = TRUE, verbose = FALSE), 'XEFQLINVKFYRCS-UHFFFAOYSA-N')
  expect_equal(length(cts_convert('Triclosan', 'Chemical Name', 'inchikey', first = TRUE, verbose = FALSE)), 1)
  expect_equal(cts_convert('xxxxxx', 'Chemical Name', 'inchikey', verbose = FALSE), NA)
  expect_warning(cts_convert(NA, 'Chemical Name', 'inchikey', verbose = FALSE))
})


# integration tests
test_that("cts_compinfo(cir_query())", {
  chk_cts()
  chk_cir()
  inchikey <- cir_query('Triclosan', representation = 'stdinchikey', verbose = FALSE)
  inchikey <- gsub('InChIKey=', '', inchikey)
  expect_equal(round(cts_compinfo(inchikey, verbose = FALSE)[[3]], 3), 289.542)

})
