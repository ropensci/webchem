context("chemspider")
token <- '37bf5e57-9091-42f5-9274-650a64398aaf'
apikey <- 'tKAuFrPCLOboXIeGLs5I257Pl0FjNH3e'

test_that("cs_datasources()",{
  skip_on_cran()

  a <- cs_datasources(apikey = apikey)

  expect_is(a, "character")
})

test_that("cs_control()",{

  expect_is(cs_control(), "list")
  expect_true("orderBy" %in% names(cs_control()))
  expect_true("orderDirection" %in% names(cs_control()))
  expect_true("includeAll" %in% names(cs_control()))
  expect_true("complexity" %in% names(cs_control()))
  expect_true("isotopic" %in% names(cs_control()))
  expect_true(cs_control(orderBy = "recordId")$orderBy == "recordId")
  expect_true(cs_control(orderBy = "massDefect")$orderBy == "massDefect")
  expect_true(cs_control(orderBy = "molecularWeight")$orderBy == "molecularWeight")
  expect_true(cs_control(orderBy = "referenceCount")$orderBy == "referenceCount")
  expect_true(cs_control(orderBy = "dataSourceCount")$orderBy == "dataSourceCount")
  expect_true(cs_control(orderBy = "pubMedCount")$orderBy == "pudMedCount")
  expect_true(cs_control(orderBy = "rscCount")$orderBy == "rscCount")
  expect_true(cs_control(orderDirection = "ascending")$orderDirection == "ascending")
  expect_true(cs_control(orderDirection = "descending")$orderDirection == "descending")
  expect_true(cs_control(includeAll = TRUE)$includeAll == TRUE)
  expect_true(cs_control(includeAll = FALSE)$includeAll == FALSE)
  expect_true(cs_control(complexity = "any")$complexity == "any")
  expect_true(cs_control(complexity = "single")$complexity == "single")
  expect_true(cs_control(complexity = "multiple")$complexity == "multiple")
  expect_true(cs_control(isotopic = "any")$isotopic == "any")
  expect_true(cs_control(isotopic = "labeled")$isotopic == "labeled")
  expect_true(cs_control(isotopic = "unlabeled")$isotopic == "unlabeled")
})

test_that("cs_name_csid()",{
  skip_on_cran()

  a <- cs_name_csid("Triclosan", apikey = apikey)
  b <- cs_name_csid("Acetic Acid", apikey = apikey)
  c1 <- cs_name_csid("Oxygen", control = cs_control(orderBy = "recordId"),
                     apikey = apikey)
  c2 <- cs_name_csid("Oxygen", control = cs_control(orderBy = "massDefect"),
                     apikey = apikey)
  c3 <- cs_name_csid("Oxygen", control = cs_control(orderBy = "molecularWeight"),
                     apikey = apikey)
  c4 <- cs_name_csid("Oxygen", control = cs_control(orderBy = "referenceCount"),
                     apikey = apikey)
  c5 <- cs_name_csid("Oxygen", control = cs_control(orderBy = "dataSourceCount"),
                     apikey = apikey)
  c6 <- cs_name_csid("Oxygen", control = cs_control(orderBy = "pubMedCount"),
                     apikey = apikey)
  c7 <- cs_name_csid("Oxygen", control = cs_control(orderBy = "rscCount"),
                     apikey = apikey)
  c8 <- cs_name_csid("Oxygen", control = cs_control(orderDirection = "ascending"),
                     apikey = apikey)
  c9 <- cs_name_csid("Oxygen", control = cs_control(orderDirection = "descending"),
                     apikey = apikey)

  expect_is(a,"list")
  expect_equal(a$results,5363)
  expect_equal(b$results,171)
  expect_equal(c1$results,c(952,140526))
  #expect_equal(c2$results,c(952,140526)) does not work, seems to be an API error.
  expect_equal(c3$results, c(140526,952))
  expect_equal(c4$results,c(952,140526))
  expect_equal(c5$results, c(140526,952))
  expect_equal(c6$results,c(952,140526))
  expect_equal(c7$results,c(952,140526))
  expect_equal(c8$results,c(952,140526))
  expect_equal(c9$results, c(140526,952))
})

test_that("cs_element_csid()",{
  skip_on_cran()

  a <- cs_element_csid(includeElements = c("Na","O","H"), excludeElements="C",
                       apikey = apikey)

  expect_is(a, "list")
  expect_equal(length(a$results),0)
})

test_that("cs_smiles_csid()",{
  skip_on_cran()

  a <- cs_smiles_csid("CC(O)=O", apikey = apikey)

  expect_is(a, "list")
  expect_equal(a$results, 171)
})

test_that("cs_inchi_csid()",{
  skip_on_cran()

  a <- cs_inchi_csid(inchi = "InChI=1S/C2H4O2/c1-2(3)4/h1H3,(H,3,4)", apikey = apikey)

  expect_is(a, "list")
  expect_equal(a$results, 171)
})

test_that("cs_inchikey_csid()",{
  skip_on_cran()

  a <- cs_inchikey_csid("QTBSBXVTEAMEQO-UHFFFAOYSA-N", apikey = apikey)

  expect_is(a, "list")
  expect_equal(a$results, 171)
})

test_that("cs_convert_multiple()",{
  skip_on_cran()

  a <- cs_convert_multiple("CC(=O)O","smiles","inchi",apikey)

  b <- cs_convert_multiple("InChI=1S/C2H4O2/c1-2(3)4/h1H3,(H,3,4)","inchi","inchikey",
                           apikey)
  c <- cs_convert_multiple("QTBSBXVTEAMEQO-UHFFFAOYSA-N","inchikey","mol",apikey)

  expect_is(a, "character")
  expect_equal(a, "InChI=1S/C2H4O2/c1-2(3)4/h1H3,(H,3,4)")
  expect_is(b, "character")
  expect_equal(b, "QTBSBXVTEAMEQO-UHFFFAOYSA-N")
  expect_is(c, "character")
})

test_that("cs_convert()",{
  skip_on_cran()

  a <- cs_convert(171, "csid", "inchi", apikey)
  a_rev <- cs_convert(a, "inchi", "csid", apikey)
  b <- cs_convert(171, "csid", "inchikey", apikey)
  b_rev <- cs_convert(b, "inhikey", "csid", apikey)
  c <- cs_convert(171, "csid", "smiles", apikey)
  c_rev <- cs_convert(c, "smiles", "csid", apikey)
  d <- cs_convert(171, "csid", "mol", apikey)
  expect_error(cs_convert(d, "mol","csid", apikey))
  e <- cs_convert("InChI=1S/C2H4O2/c1-2(3)4/h1H3,(H,3,4)","inchi","inchikey",
                  apikey)
  e_rev <- cs_convert(e, "inchikey", "inchi", apikey)
  f <- cs_convert("InChI=1S/C2H4O2/c1-2(3)4/h1H3,(H,3,4)", "inchi", "smiles", apikey)
  f_rev <- cs_convert(f, "smiles", "inchi", apikey)
  g <- cs_convert("InChI=1S/C2H4O2/c1-2(3)4/h1H3,(H,3,4)", "inchi", "mol", apikey)
  g_rev <- cs_convert(g, "mol", "inchi", apikey)
  h <- cs_convert("QTBSBXVTEAMEQO-UHFFFAOYSA-N","inchikey","mol",apikey)
  h_rev <- cs_convert(h, "mol", "inchikey", apikey)

  expect_equal(a, "InChI=1/C2H4O2/c1-2(3)4/h1H3,(H,3,4)")
  expect_equal(a_rev, 171)
  expect_equal(b, "QTBSBXVTEAMEQO-UHFFFAOYAR")
  expect_equal(b_rev, 171)
  expect_equal(c, "CC(=O)O")
  expect_equal(c_rev, 171)
  expect_is(parse_mol(d), "list")
  expect_equal(e, "QTBSBXVTEAMEQO-UHFFFAOYSA-N")
  expect_equal(e_rev, "InChI=1S/C2H4O2/c1-2(3)4/h1H3,(H,3,4)")
  expect_equal(f, "CC(=O)O")
  expect_equal(f_rev, "InChI=1S/C2H4O2/c1-2(3)4/h1H3,(H,3,4)")
  expect_is(parse_mol(g), "list")
  expect_equal(g_rev, "InChI=1S/C2H4O2/c1-2(3)4/h1H3,(H,3,4)")
  expect_is(parse_mol(h), "list")
  expect-equal(h_rev, "QTBSBXVTEAMEQO-UHFFFAOYSA-N")
})

test_that("cs_compinfo()",{
  skip_on_cran()

  a <- cs_compinfo(171, c("SMILES", "Formula", "InChI", "InChIKey", "StdInChI",
                          "StdInChIKey", "AverageMass", "MolecularWeight",
                          "MonoisotopicMass", "NominalMass", "CommonName",
                          "ReferenceCount", "DataSourceCount", "PubMedCount",
                          "RSCCount", "Mol2D", "Mol3D"), apikey)
  b <- cs_compinfo(c(171, 172), c("SMILES", "Formula", "InChI", "InChIKey",
                                  "StdInChI", "StdInChIKey", "AverageMass",
                                  "MolecularWeight", "MonoisotopicMass",
                                  "NominalMass", "CommonName", "ReferenceCount",
                                  "DataSourceCount", "PubMedCount", "RSCCount",
                                  "Mol2D", "Mol3D"), apikey)

  expect_is(a, "data.frame")
  expect_equal(dim(a), c(1,18))
  expect_is(b, "data.frame")
  expect_equal(dim(b), c(2,18))
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

  # issue #148
  m5 <- cs_prop(7688)
  expect_is(m5, 'list')
  expect_equal(length(m5), 1)
  expect_equal(length(m5[[1]]), 3)
  expect_is(m5[[1]]$epi, 'data.frame')
  expect_is(m5[[1]]$acd, 'data.frame')

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
