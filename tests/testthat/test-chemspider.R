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

test_that("cs_compinfo()", {
  chk_cs()

  expect_equal(length(cs_compinfo('5363', token, verbose = FALSE)), 4)
  expect_equal(cs_compinfo('5363', token, verbose = FALSE)['SMILES'],
               c(SMILES = "c1cc(c(cc1Cl)O)Oc2ccc(cc2Cl)Cl"))
  expect_warning(cs_compinfo('aaaa', token, verbose = FALSE))
  expect_error(cs_compinfo(c("a", "b"), token = token))
  })

test_that("cs_extcompinfo()", {
  chk_cs()

  expect_equal(cs_extcompinfo('5363', token = token, verbose = FALSE)['AverageMass'],
               c(AverageMass = '289.5418'))
  expect_equal(length(cs_extcompinfo('5363', token = token, verbose = FALSE)), 12)
  expect_warning(cs_extcompinfo('aaaa', token, verbose = FALSE))
  expect_error(cs_extcompinfo(c("a", "b"), token = token))
})


test_that("csid_extcompinfo(get_cid())", {
  chk_cs()

  expect_equal(cs_extcompinfo(get_csid('Triclosan', token = token, verbose = FALSE),
                                token = token, verbose = FALSE)['AverageMass'],
               c(AverageMass = '289.5418'))
  expect_equal(length(cs_extcompinfo(get_csid('Triclosan', token = token, verbose = FALSE),
                                       token = token, verbose = FALSE)), 12)
})


test_that("cs_csid_mol()", {
  m1 <- cs_csid_mol(5363, token = token, verbose = FALSE)
  m2 <- cs_csid_mol(5363, token = token, parse = FALSE, verbose = FALSE)

  expect_warning(cs_csid_mol('xxxx', token = token))
  expect_equal(cs_csid_mol('xxxx', token = token), NA)

  expect_error(cs_csid_mol(c(5363,5363), token = token))
  expect_message(cs_csid_mol(5363, token = token))

  expect_is(m1, 'list')
  expect_equal(length(m1), 4)
  expect_is(m1$ab, 'data.frame')
  expect_is(m1$bb, 'data.frame')
  expect_equal(unname(m1$cl[1]), "17")
  expect_equal(unname(m1$cl[2]), "18")

  expect_is(m2, 'character')
  expect_equal(length(m2), 1)
})


test_that("is.inchikey_cs", {
  expect_message(is.inchikey_cs('BQJCRHHNABKAKU-KBQPJGBKSA-N'))
  g <- is.inchikey_cs('BQJCRHHNABKAKU-KBQPJGBKSA-N')
  b <- is.inchikey_cs('BQJCRHHNABKAKU-KBQPJGBKSA')

  expect_true(g)
  expect_false(b)

  expect_error(is.inchikey_cs(c('BQJCRHHNABKAKU-KBQPJGBKSA', 'BQJCRHHNABKAKU-KBQPJGBKSA-N')))

  expect_equal(length(g), 1)
})


test_that("cs_inchikey_csid()", {
  m1 <- cs_inchikey_csid('BQJCRHHNABKAKU-KBQPJGBKSA-N', token = token)

  expect_error(cs_inchikey_csid(c('BQJCRHHNABKAKU-KBQPJGBKSA-N','BQJCRHHNABKAKU-KBQPJGBKSA-N'), token = token))
  expect_message(cs_inchikey_csid('BQJCRHHNABKAKU-KBQPJGBKSA-N', token = token))

  expect_is(m1, 'character')
  expect_equal(length(m1), 1)
  expect_equal(m1, "4450907")

  expect_warning(cs_inchikey_csid('xxx', token = token))
  expect_equal(cs_inchikey_csid('xxx', token = token), NA)
})