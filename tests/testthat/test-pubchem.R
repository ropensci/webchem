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

test_that("get_pcid()", {
  chk_pubchem()
  expect_equal(get_pcid('Triclosan')[1], '5564')
  expect_equal(length(get_pcid('Triclosan', first = TRUE, verbose = FALSE)), 1)
  chk_pubchem()
  expect_equal(get_pcid('xxxxx', verbose = FALSE), NA)
  expect_error(get_pcid(c('xxxxx', 'aaaaaaa')))
  expect_warning(get_pcid(NA))
})


test_that("pc_compinfo", {
  chk_pubchem()

  expect_error(pc_compinfo(c('xxxxx', 'aaaaaaa')))
  chk_pubchem()
  a <- pc_compinfo('5564', verbose = FALSE)
  expect_equal(a$CanonicalSmiles, "C1=CC(=C(C=C1Cl)O)OC2=C(C=C(C=C2)Cl)Cl")
  expect_equal(length(a), 26)
  chk_pubchem()
  expect_equal(pc_compinfo('xxxxx', verbose = FALSE), NA)
  expect_warning(pc_compinfo('xxxxx', verbose = TRUE))
})


test_that("cid integration tests", {
  chk_pubchem()
  expect_equal(pc_compinfo(get_pcid('Triclosan')[1])$CanonicalSmiles, "C1=CC(=C(C=C1Cl)O)OC2=C(C=C(C=C2)Cl)Cl")
  expect_true(is.na(pc_compinfo(get_pcid('xxxxxx')[1])))
  expect_warning(pc_compinfo(get_pcid('xxxxxx')[1]))
})