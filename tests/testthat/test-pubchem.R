context("pubchem")

require(RCurl)
chk_pubchem <- function(){
  qurl <- 'http://eutils.ncbi.nlm.nih.gov/entrez/eutils/esearch.fcgi?retmax=100000&db=pccompound&term=Triclosan'
  Sys.sleep(0.2)
  cont <- try(getURL(qurl, .encoding = 'UTF-8', .opts = list(timeout = 3)),
              silent = TRUE)
  if (inherits(cont, 'try-error'))
    skip("Server is down!")
}

test_that("get_cid()", {
  chk_pubchem()
  expect_equal(get_cid('Triclosan')[1], '5564')
  expect_equal(length(get_cid('Triclosan', first = TRUE, verbose = FALSE)), 1)
  chk_pubchem()
  expect_equal(get_cid('xxxxx', verbose = FALSE), NA)
  expect_error(get_cid(c('xxxxx', 'aaaaaaa')))
  expect_warning(get_cid(NA))
})


test_that("cid_compinfo", {
  chk_pubchem()

  expect_error(cid_compinfo(c('xxxxx', 'aaaaaaa')))
  chk_pubchem()
  expect_equal(cid_compinfo('5564', verbose = FALSE)$CanonicalSmiles, "C1=CC(=C(C=C1Cl)O)OC2=C(C=C(C=C2)Cl)Cl")
  expect_equal(length(cid_compinfo('5564', verbose = FALSE)), 25)
  chk_pubchem()
  expect_equal(length(cid_compinfo('5564', first = TRUE, verbose = FALSE)$synonyms), 1)
  expect_equal(cid_compinfo('xxxxx', verbose = FALSE), NA)
  expect_warning(cid_compinfo('xxxxx', verbose = TRUE))
})


test_that("cid integration tests", {
  chk_pubchem()
  expect_equal(cid_compinfo(get_cid('Triclosan')[1])$CanonicalSmiles, "C1=CC(=C(C=C1Cl)O)OC2=C(C=C(C=C2)Cl)Cl")
  expect_true(is.na(cid_compinfo(get_cid('xxxxxx')[1])))
  expect_warning(cid_compinfo(get_cid('xxxxxx')[1]))
})