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
  m1 <- cs_inchikey_csid('BQJCRHHNABKAKU-KBQPJGBKSA-N')

  expect_error(cs_inchikey_csid(c('BQJCRHHNABKAKU-KBQPJGBKSA-N','BQJCRHHNABKAKU-KBQPJGBKSA-N')))
  expect_message(cs_inchikey_csid('BQJCRHHNABKAKU-KBQPJGBKSA-N'))

  expect_is(m1, 'character')
  expect_equal(length(m1), 1)
  expect_equal(m1, "4450907")

  expect_warning(cs_inchikey_csid('xxx'))
  expect_equal(cs_inchikey_csid('xxx'), NA)
})


test_that("cs_inchikey_inchi()", {
  m1 <- cs_inchikey_inchi('BQJCRHHNABKAKU-KBQPJGBKSA-N')

  expect_error(cs_inchikey_inchi(c('BQJCRHHNABKAKU-KBQPJGBKSA-N','BQJCRHHNABKAKU-KBQPJGBKSA-N')))
  expect_message(cs_inchikey_inchi('BQJCRHHNABKAKU-KBQPJGBKSA-N'))

  expect_is(m1, 'character')
  expect_equal(length(m1), 1)
  expect_equal(m1,  "InChI=1S/C17H19NO3/c1-18-7-6-17-10-3-5-13(20)16(17)21-15-12(19)4-2-9(14(15)17)8-11(10)18/h2-5,10-11,13,16,19-20H,6-8H2,1H3/t10-,11+,13-,16-,17-/m0/s1")

  expect_warning(cs_inchikey_inchi('xxx'))
  expect_equal(cs_inchikey_inchi('xxx'), NA)
})


test_that("cs_inchikey_mol()", {
  m1 <- cs_inchikey_mol('BQJCRHHNABKAKU-KBQPJGBKSA-N', verbose = FALSE)
  m2 <- cs_inchikey_mol('BQJCRHHNABKAKU-KBQPJGBKSA-N', parse = FALSE, verbose = FALSE)

  expect_warning(cs_inchikey_mol('xxxx'))
  expect_equal(cs_inchikey_mol('xxxx'), NA)

  expect_error(cs_inchikey_mol(c('BQJCRHHNABKAKU-KBQPJGBKSA-N', 'BQJCRHHNABKAKU-KBQPJGBKSA-N')))
  expect_message(cs_inchikey_mol('BQJCRHHNABKAKU-KBQPJGBKSA-N'))

  expect_is(m1, 'list')
  expect_equal(length(m1), 4)
  expect_is(m1$ab, 'data.frame')
  expect_is(m1$bb, 'data.frame')
  expect_equal(unname(m1$cl[1]), "21")
  expect_equal(unname(m1$cl[2]), "25")

  expect_is(m2, 'character')
  expect_equal(length(m2), 1)
})



test_that("cs_inchi_csid()", {
  inchi <-  "InChI=1S/C17H19NO3/c1-18-7-6-17-10-3-5-13(20)16(17)21-15-12(19)4-2-9(14(15)17)8-11(10)18/h2-5,10-11,13,16,19-20H,6-8H2,1H3/t10-,11+,13-,16-,17-/m0/s1"
  m1 <- cs_inchi_csid(inchi)

  expect_error(cs_inchi_csid(c(inchi, inchi)))
  expect_message(cs_inchi_csid(inchi))

  expect_is(m1, 'character')
  expect_equal(length(m1), 1)
  expect_equal(m1, "4450907")

  expect_warning(cs_inchi_csid('xxx'))
  expect_equal(cs_inchi_csid('xxx'), NA)
})


test_that("cs_inchi_inchikey()", {
  inchi <-  "InChI=1S/C17H19NO3/c1-18-7-6-17-10-3-5-13(20)16(17)21-15-12(19)4-2-9(14(15)17)8-11(10)18/h2-5,10-11,13,16,19-20H,6-8H2,1H3/t10-,11+,13-,16-,17-/m0/s1"
  m1 <- cs_inchi_inchikey(inchi)

  expect_error(cs_inchi_inchikey(c(inchi, inchi)))
  expect_message(cs_inchi_inchikey(inchi))

  expect_is(m1, 'character')
  expect_equal(length(m1), 1)
  expect_equal(m1, "BQJCRHHNABKAKU-KBQPJGBKSA-N")

  expect_warning(cs_inchi_inchikey('xxx'))
  expect_equal(cs_inchi_inchikey('xxx'), NA)
})


test_that("cs_inchi_mol()", {
  inchi <-  "InChI=1S/C17H19NO3/c1-18-7-6-17-10-3-5-13(20)16(17)21-15-12(19)4-2-9(14(15)17)8-11(10)18/h2-5,10-11,13,16,19-20H,6-8H2,1H3/t10-,11+,13-,16-,17-/m0/s1"
  m1 <- cs_inchi_mol(inchi, verbose = FALSE)
  m2 <- cs_inchi_mol(inchi, parse = FALSE, verbose = FALSE)

  expect_warning(cs_inchi_mol('xxxx'))
  expect_equal(cs_inchi_mol('xxxx'), NA)

  expect_error(cs_inchi_mol(c(inchi, inchi)))
  expect_message(cs_inchi_mol(inchi))

  expect_is(m1, 'list')
  expect_equal(length(m1), 4)
  expect_is(m1$ab, 'data.frame')
  expect_is(m1$bb, 'data.frame')
  expect_equal(unname(m1$cl[1]), "25")
  expect_equal(unname(m1$cl[2]), "29")

  expect_is(m2, 'character')
  expect_equal(length(m2), 1)
})


test_that("cs_inchi_smiles()", {
  inchi <-  "InChI=1S/C17H19NO3/c1-18-7-6-17-10-3-5-13(20)16(17)21-15-12(19)4-2-9(14(15)17)8-11(10)18/h2-5,10-11,13,16,19-20H,6-8H2,1H3/t10-,11+,13-,16-,17-/m0/s1"
  m1 <- cs_inchi_smiles(inchi)

  expect_error(cs_inchi_smiles(c(inchi, inchi)))
  expect_message(cs_inchi_smiles(inchi))

  expect_is(m1, 'character')
  expect_equal(length(m1), 1)
  expect_equal(m1,  "CN1CC[C@]23[C@H]4C=C[C@@H]([C@@H]3Oc3c(ccc(C[C@@H]14)c23)O)O")

  expect_warning(cs_inchi_smiles('xxx'))
  expect_equal(cs_inchi_smiles('xxx'), NA)
})


test_that("cs_smiles_inchi()", {
  smiles <- "CN1CC[C@]23[C@H]4C=C[C@@H]([C@@H]3Oc3c(ccc(C[C@@H]14)c23)O)O"
  m1 <- cs_smiles_inchi(smiles)

  expect_error(cs_smiles_inchi(c(smiles, smiles)))
  expect_message(cs_smiles_inchi(smiles))

  expect_is(m1, 'character')
  expect_equal(length(m1), 1)

  expect_warning(cs_smiles_inchi('xxx'))
  expect_equal(cs_smiles_inchi('xxx'), NA)
})


test_that("cs_convert()", {
  inchikey <- 'BQJCRHHNABKAKU-KBQPJGBKSA-N'
  csid <- "4450907"
  inchi <-  "InChI=1S/C17H19NO3/c1-18-7-6-17-10-3-5-13(20)16(17)21-15-12(19)4-2-9(14(15)17)8-11(10)18/h2-5,10-11,13,16,19-20H,6-8H2,1H3/t10-,11+,13-,16-,17-/m0/s1"
  smiles <- "CN1CC[C@]23[C@H]4C=C[C@@H]([C@@H]3Oc3c(ccc(C[C@@H]14)c23)O)O"

  expect_error(cs_convert(c(inchikey, inchikey), from = 'inchikey', to = 'csid'))
  expect_error(cs_convert(csid, from = 'csid', to = 'mol'))
  expect_error(cs_convert(csid, from = 'csid', to = 'inchikey'))

  m1 <- cs_convert(csid, from = 'csid', to = 'mol', token = token)
  m1r <- cs_convert(csid, from = 'csid', to = 'mol', token = token, parse = FALSE)
  expect_is(m1, 'list')
  expect_equal(length(m1), 4)
  expect_is(m1$ab, 'data.frame')
  expect_is(m1$bb, 'data.frame')
  expect_equal(unname(m1$cl[1]), "22")
  expect_equal(unname(m1$cl[2]), "26")
  expect_is(m1r, 'character')
  expect_equal(length(m1r), 1)

  m2 <- cs_convert(inchikey, from = 'inchikey', to = 'csid')
  expect_equal(m2, csid)

  m3 <- cs_convert(inchikey, from = 'inchikey', to = 'inchi')
  expect_equal(m3, inchi)

  m4 <- cs_convert(inchikey, from = 'inchikey', to = 'mol')
  expect_is(m4, 'list')
  expect_equal(length(m4), 4)
  expect_is(m4$ab, 'data.frame')
  expect_is(m4$bb, 'data.frame')
  expect_equal(unname(m4$cl[1]), "21")
  expect_equal(unname(m4$cl[2]), "25")

  m5 <- cs_convert(inchi, from = 'inchi', to = 'csid')
  expect_equal(m5, csid)

  m6 <- cs_convert(inchi, from = 'inchi', to = 'inchikey')
  expect_equal(m6, inchikey)

  m7 <- cs_convert(inchi, from = 'inchi', to = 'mol')
  expect_is(m7, 'list')
  expect_equal(length(m7), 4)
  expect_is(m7$ab, 'data.frame')
  expect_is(m7$bb, 'data.frame')
  expect_equal(unname(m7$cl[1]), "25")
  expect_equal(unname(m7$cl[2]), "29")

  m8 <- cs_convert(inchi, from = 'inchi', to = 'smiles')
  expect_equal(m8, smiles)

  m9 <- cs_convert(smiles, from = 'smiles', to = 'inchi')
  expect_equal(m9, inchi)
})