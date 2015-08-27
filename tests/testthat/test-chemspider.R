context("chemspider")
token <- '37bf5e57-9091-42f5-9274-650a64398aaf'

chk_cs <- function(){
  qurl <- 'http://www.chemspider.com/Search.asmx/SimpleSearch?query=Triclosan&token=37bf5e57-9091-42f5-9274-650a64398aaf'
  Sys.sleep(0.2)
  cont <- try(getURL(qurl, .encoding = 'UTF-8', .opts = list(timeout = 3)),
              silent = TRUE)
  if (inherits(cont, 'try-error'))
    skip("Server is down!")
}




test_that("get_csid()", {
  chk_cs()

  expect_equal(get_csid("Triclosan", token = token, verbose = FALSE), '5363')
  expect_equal(get_csid("xxxxxxxxx", token = token, verbose = FALSE), NA)
  expect_error(get_csid(c("a", "b"), token = token))
  expect_warning(get_csid(NA, token = token))
  expect_true(is.vector(get_csid("Triclosan", token = token, verbose = FALSE)))
})

test_that("csid_compinfo()", {
  chk_cs()

  expect_equal(length(csid_compinfo('5363', token, verbose = FALSE)), 4)
  expect_equal(csid_compinfo('5363', token, verbose = FALSE)['SMILES'],
               c(SMILES = "c1cc(c(cc1Cl)O)Oc2ccc(cc2Cl)Cl"))
  expect_warning(csid_compinfo('aaaa', token, verbose = FALSE))
  expect_error(csid_compinfo(c("a", "b"), token = token))
  })

test_that("csid_extcompinfo()", {
  chk_cs()

  expect_equal(csid_extcompinfo('5363', token = token, verbose = FALSE)['AverageMass'],
               c(AverageMass = '289.5418'))
  expect_equal(length(csid_extcompinfo('5363', token = token, verbose = FALSE)), 12)
  expect_warning(csid_extcompinfo('aaaa', token, verbose = FALSE))
  expect_error(csid_extcompinfo(c("a", "b"), token = token))
})


test_that("csid_extcompinfo(get_cid())", {
  chk_cs()

  expect_equal(csid_extcompinfo(get_csid('Triclosan', token = token, verbose = FALSE),
                                token = token, verbose = FALSE)['AverageMass'],
               c(AverageMass = '289.5418'))
  expect_equal(length(csid_extcompinfo(get_csid('Triclosan', token = token, verbose = FALSE),
                                       token = token, verbose = FALSE)), 12)
})