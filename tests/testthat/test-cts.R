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
  expect_error(cts_compinfo('xxx'))

  o1 <- cts_compinfo("XEFQLINVKFYRCS-UHFFFAOYSA-N", verbose = FALSE)
  o2 <- cts_compinfo(c("XEFQLINVKFYRCS-UHFFFAOYSA-N", "XEFQLINVKFYRCS-UHFFFAOYSA-X"), verbose = FALSE)
  expect_equal(cts_compinfo("XEFQLINVKFYRCS-UHFFFAOYSA-X", verbose = FALSE)[[1]], NA)
  expect_warning(cts_compinfo("XEFQLINVKFYRCS-UHFFFAOYSA-X", verbose = FALSE))
  expect_equal(length(o1[[1]]), 7)
  expect_equal(round(o1[[1]][["molweight"]], 3), 289.542)
  expect_equal(length(o2), 2)
  expect_true(is.na(o2[[2]]))
})


test_that("cts_convert()", {
  chk_cts()
  comp <- c('XEFQLINVKFYRCS-UHFFFAOYSA-N', 'BSYNRYMUTXBXSQ-UHFFFAOYSA-N')
  expect_error(cts_convert(comp, c('Chemical Name', 'CAS'), 'CAS'))
  expect_error(cts_convert('Triclosan', 'CAS'))
  o1 <- cts_convert(comp, 'Chemical Name', 'inchikey', first = TRUE, verbose = FALSE)
  expect_equal(o1[[1]], 'XEFQLINVKFYRCS-UHFFFAOYSA-N')
  expect_equal(length(o1), 2)
  expect_true(is.na(cts_convert('xxxx', 'inchikey', 'Chemical Name')[[1]]))
})


# integration tests
test_that("cts_compinfo(cir_query())", {
  chk_cts()
  chk_cir()
  inchikey <- cir_query('Triclosan', representation = 'stdinchikey', verbose = FALSE)
  inchikey <- gsub('InChIKey=', '', inchikey)
  expect_equal(round(cts_compinfo(inchikey, verbose = FALSE)[[1]][["molweight"]], 3), 289.542)
})


test_that("fromto", {
  to <- cts_to()
  from <- cts_from()

  expect_true(is.character(to))
  expect_true(is.character(from))
})
