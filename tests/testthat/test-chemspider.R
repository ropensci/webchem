context("chemspider")
token <- '37bf5e57-9091-42f5-9274-650a64398aaf'


test_that("get_csid()", {
  skip_on_cran()

  comps <- c("Triclosan", "50-00-0", "xxxxxx")
  o1 <- get_csid(comps, token = token, verbose = TRUE)
  o2 <- get_csid(comps, token = token, verbose = TRUE, first = FALSE)
  o3 <- get_csid(c("picoxystrobin", "mandipropamid"), token = token, verbose = TRUE, first = FALSE)

  b1 <- get_csid('acetic acid', token = token, verbose = TRUE)
  expect_equal(b1, c(`acetic acid` = '171'))

  expect_is(o1, 'character')
  expect_is(o2, 'list')
  expect_equal(length(o1),3)
  expect_equal(o3, structure(list(picoxystrobin = "9460644", mandipropamid = "9467809"), .Names = c("picoxystrobin",
      "mandipropamid")))
  expect_equal(length(o2), 3)
  expect_true(is.na(o1[[3]]))
  expect_true(is.na(o2[[3]]))
  expect_equal(o1[[1]], '5363')
  expect_equal(o2[[2]], '692')
  expect_true(is.na(get_csid(NA, token = token, verbose = TRUE)))

})


test_that("cs_compinfo()", {
  skip_on_cran()

  comps <- c("2157", "5363" )
  o1 <- cs_compinfo(comps, token)
  expect_is(o1, 'data.frame')
  expect_equal(dim(o1), c(2, 6))
  expect_equal(o1$csid[1], '2157')
  expect_true(all(is.na(cs_compinfo(NA, token)[ 1, 1:5])))
})


test_that("cs_extcompinfo()", {
  skip_on_cran()

  comps <- c("2157", "5363" )
  o1 <- cs_extcompinfo(comps, token)
  expect_is(o1, 'data.frame')
  expect_equal(dim(o1), c(2, 14))
  expect_equal(o1$csid[1], '2157')
  expect_true(all(is.na(cs_extcompinfo(c(2157, NA), token)[ 2, 1:5])))
})


test_that("cs_prop()", {
  skip_on_cran()

  id <- '5363'
  m1 <- cs_prop(id)

  expect_is(m1, 'list')
  expect_equal(length(m1), 1)
  expect_equal(length(m1[[1]]), 3)

  expect_is(m1[[1]]$epi, 'data.frame')
  expect_is(m1[[1]]$acd, 'data.frame')
  expect_equal(m1[[1]]$source_url,  "https://www.chemspider.com/Chemical-Structure.5363.html")
  expect_equal(names(m1[[1]]$epi), c("prop", "value_pred", "unit_pred",
                                     "source_pred", "value_exp",
                                     "unit_exp", "source_exp"))
  expect_equal(names(m1[[1]]$acd), c("variable", "value", "error", "unit"))

  # issue #127
  m2 <- cs_prop(16105)
  expect_is(m2, 'list')
  expect_equal(length(m2), 1)
  expect_equal(length(m2[[1]]), 3)
  expect_is(m2[[1]]$epi, 'data.frame')
  expect_is(m2[[1]]$acd, 'data.frame')
  expect_equal(m2[[1]]$epi$value_exp[2], 178.5)

  # issue #139 (no epi-suite data available)
  m3 <- cs_prop(21106900)
  expect_true(nrow(m3$`21106900`$epi) == 0)

  # issue #138 (invalid chemspider html)
  m3 <- cs_prop(8012)
  expect_is(m3, 'list')
  expect_equal(length(m3), 1)
  expect_equal(length(m3[[1]]), 3)
  expect_is(m3[[1]]$epi, 'data.frame')
  expect_is(m3[[1]]$acd, 'data.frame')

  # issue #142
  m4 <- cs_prop(391783)
  expect_is(m4, 'list')
  expect_equal(length(m4), 1)
  expect_equal(length(m4[[1]]), 3)
  expect_is(m4[[1]]$epi, 'data.frame')
  expect_is(m4[[1]]$acd, 'data.frame')

  # issue #143
  r <- m4$`391783`$epi
  expect_equal(r$value_pred[r$prop == 'Water Solubility from KOW'], 13690)

})


# integration tests
test_that("csid_extcompinfo(get_cid())", {
  skip_on_cran()

  tt <- get_csid('Triclosan', token = token, verbose = FALSE)
  tt2 <- cs_extcompinfo(tt,
                        token = token, verbose = FALSE)
  expect_equal(tt2[['average_mass']],
               "289.5418")
  expect_equal(ncol(tt2), 14)
})


# converters
test_that("cs_csid_mol()", {
  skip_on_cran()

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


test_that("cs_inchikey_csid()", {
  skip_on_cran()

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
  skip_on_cran()

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
  skip_on_cran()

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
  skip_on_cran()

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
  skip_on_cran()

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
  skip_on_cran()

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
  skip_on_cran()

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
  skip_on_cran()

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
  skip_on_cran()

  inchikey <- 'BQJCRHHNABKAKU-KBQPJGBKSA-N'
  csid <- "4450907"
  inchi <-  "InChI=1S/C17H19NO3/c1-18-7-6-17-10-3-5-13(20)16(17)21-15-12(19)4-2-9(14(15)17)8-11(10)18/h2-5,10-11,13,16,19-20H,6-8H2,1H3/t10-,11+,13-,16-,17-/m0/s1"
  smiles <- "CN1CC[C@]23[C@H]4C=C[C@@H]([C@@H]3Oc3c(ccc(C[C@@H]14)c23)O)O"

  expect_error(cs_convert(csid, from = 'csid', to = 'mol'))
  expect_error(cs_convert(csid, from = 'csid', to = 'inchikey'))

  m1 <- cs_convert(csid, from = 'csid', to = 'mol', token = token)
  m1r <- cs_convert(csid, from = 'csid', to = 'mol', token = token, parse = FALSE)
  expect_is(m1, 'list')
  expect_equal(length(m1[[1]]), 4)
  expect_is(m1[[1]]$ab, 'data.frame')
  expect_is(m1[[1]]$bb, 'data.frame')
  expect_equal(unname(m1[[1]]$cl[1]), "21")
  expect_equal(unname(m1[[1]]$cl[2]), "25")
  expect_is(m1r[[1]], 'character')
  expect_equal(length(m1r[[1]]), 1)

  m2 <- cs_convert(inchikey, from = 'inchikey', to = 'csid')
  expect_equal(m2[[1]], csid)

  m3 <- cs_convert(inchikey, from = 'inchikey', to = 'inchi')
  expect_equal(m3[[1]], inchi)

  m4 <- cs_convert(inchikey, from = 'inchikey', to = 'mol')
  expect_is(m4[[1]], 'list')
  expect_equal(length(m4[[1]]), 4)
  expect_is(m4[[1]]$ab, 'data.frame')
  expect_is(m4[[1]]$bb, 'data.frame')
  expect_equal(unname(m4[[1]]$cl[1]), "21")
  expect_equal(unname(m4[[1]]$cl[2]), "25")

  m5 <- cs_convert(inchi, from = 'inchi', to = 'csid')
  expect_equal(m5[[1]], csid)

  m6 <- cs_convert(inchi, from = 'inchi', to = 'inchikey')
  expect_equal(m6[[1]], inchikey)

  m7 <- cs_convert(inchi, from = 'inchi', to = 'mol')
  expect_is(m7[[1]], 'list')
  expect_equal(length(m7[[1]]), 4)
  expect_is(m7[[1]]$ab, 'data.frame')
  expect_is(m7[[1]]$bb, 'data.frame')
  expect_equal(unname(m7[[1]]$cl[1]), "25")
  expect_equal(unname(m7[[1]]$cl[2]), "29")

  m8 <- cs_convert(inchi, from = 'inchi', to = 'smiles')
  expect_equal(m8[[1]], smiles)

  m9 <- cs_convert(smiles, from = 'smiles', to = 'inchi')
  expect_equal(m9[[1]], inchi)
})


