context("cts")

chk_cts <- function(){
  qurl <- 'http://cts.fiehnlab.ucdavis.edu/service/compound/XEFQLINVKFYRCS-UHFFFAOYSA-N'
  Sys.sleep(0.2)
  cont <- try(getURL(qurl, .encoding = 'UTF-8', .opts = list(timeout = 3)),
              silent = TRUE)
  if (inherits(cont, 'try-error'))
    skip("Server is down!")
}

chk_cir <- function(){
  qurl <- 'http://cactus.nci.nih.gov/chemical/structure/Triclosan/cas/xml'
  Sys.sleep(0.2)
  cont <- try(getURL(qurl, .encoding = 'UTF-8', .opts = list(timeout = 3)),
              silent = TRUE)
  if (inherits(cont, 'try-error'))
    skip("Server is down!")
}


test_that("cts_compinfo()", {
  chk_cts()
  expect_error(cts_compinfo(c('xxxxx', 'aaaaaaa')))
  expect_equal(cts_compinfo('xxxxx', verbose = FALSE), NA)
  expect_equal(length(cts_compinfo("XEFQLINVKFYRCS-UHFFFAOYSA-N", verbose = FALSE)), 7)
  expect_equal(round(cts_compinfo("XEFQLINVKFYRCS-UHFFFAOYSA-N", verbose = FALSE)[[3]], 3), 289.542)
})


test_that("cts_convert()", {
  chk_cts()
  expect_error(cts_convert(c('xxxxx', 'aaaaaaa'), 'Chemical Name', 'CAS'))
  expect_error(cts_convert('Triclosan', c('Chemical Name', 'CAS'), 'CAS'))
  expect_error(cts_convert('Triclosan', 'CAS'))
  expect_equal(cts_convert('Triclosan', 'Chemical Name', 'Inchikey', verbose = FALSE), 'XEFQLINVKFYRCS-UHFFFAOYSA-N')
  expect_equal(length(cts_convert('Triclosan', 'Chemical Name', 'CAS', first = TRUE, verbose = FALSE)), 1)
  expect_equal(cts_convert('xxxxxx', 'Chemical Name', 'CAS', verbose = FALSE), NA)
  expect_warning(cts_convert(NA, 'Chemical Name', 'CAS', verbose = FALSE))
  expect_warning(cts_convert('XEFQLINVKFYRCS-UHFFFAOYSA-N', 'inchike', 'Chemical Name'))
})

test_that("cts_compinfo(cir_query())", {
  chk_cts()
  chk_cir()
  inchikey <- cir_query('Triclosan', representation = 'stdinchikey', verbose = FALSE)
  inchikey <- gsub('InChIKey=', '', inchikey)
  expect_equal(round(cts_compinfo(inchikey, verbose = FALSE)[[3]], 3), 289.542)

})