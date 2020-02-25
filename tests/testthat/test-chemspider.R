context("chemspider")

test_that("cs_check_key() can find API key in my local .Renviron", {
  skip_on_cran()
  expect_type(cs_check_key(), "character")
})

test_that("cs_datasources()", {
  skip_on_cran()
  a <- cs_datasources()

  expect_is(a, "character")
})

test_that("cs_control()", {
  skip_on_cran()
  expect_is(cs_control(), "list")
  expect_true("datasources" %in% names(cs_control()))
  expect_true("order_by" %in% names(cs_control()))
  expect_true("order_direction" %in% names(cs_control()))
  expect_true("include_all" %in% names(cs_control()))
  expect_true("complexity" %in% names(cs_control()))
  expect_true("isotopic" %in% names(cs_control()))
  expect_true(cs_control(order_by = "recordId")$order_by == "recordId")
  expect_true(cs_control(order_by = "massDefect")$order_by == "massDefect")
  expect_true(cs_control(order_by = "molecularWeight")$order_by ==
                "molecularWeight")
  expect_true(cs_control(order_by = "referenceCount")$order_by ==
                "referenceCount")
  expect_true(cs_control(order_by = "dataSourceCount")$order_by ==
                "dataSourceCount")
  expect_true(cs_control(order_by = "pubMedCount")$order_by == "pubMedCount")
  expect_true(cs_control(order_by = "rscCount")$order_by == "rscCount")
  expect_true(cs_control(order_direction = "ascending")$order_direction ==
                "ascending")
  expect_true(cs_control(order_direction = "descending")$order_direction ==
                "descending")
  expect_true(cs_control(include_all = TRUE)$include_all == TRUE)
  expect_true(cs_control(include_all = FALSE)$include_all == FALSE)
  expect_true(cs_control(complexity = "any")$complexity == "any")
  expect_true(cs_control(complexity = "single")$complexity == "single")
  expect_true(cs_control(complexity = "multiple")$complexity == "multiple")
  expect_true(cs_control(isotopic = "any")$isotopic == "any")
  expect_true(cs_control(isotopic = "labeled")$isotopic == "labeled")
  expect_true(cs_control(isotopic = "unlabeled")$isotopic == "unlabeled")
})

test_that("get_csid()", {
  skip_on_cran()
  a <- get_csid("Triclosan")
  b <- get_csid("Naproxene")
  ab <- get_csid(c("Triclosan", "Naproxene"))
  c1 <- get_csid("Oxygen", control = cs_control(order_by = "recordId"))
  #c2 <- get_csid("Oxygen", control = cs_control(order_by = "massDefect"))
  c3 <- get_csid("Oxygen", control = cs_control(order_by = "molecularWeight"))
  c4 <- get_csid("Oxygen", control = cs_control(order_by = "referenceCount"))
  c5 <- get_csid("Oxygen", control = cs_control(order_by = "dataSourceCount"))
  c6 <- get_csid("Oxygen", control = cs_control(order_by = "pubMedCount"))
  c7 <- get_csid("Oxygen", control = cs_control(order_by = "rscCount"))
  c8 <- get_csid("Oxygen", control = cs_control(order_direction = "ascending"))
  c9 <- get_csid("Oxygen", control = cs_control(order_direction = "descending"))

  expect_is(a, "data.frame")
  expect_equal(a$csid, 5363)
  expect_equal(b$csid, 137720)
  expect_equal(ab$csid, c(5363, 137720))
  expect_equal(c1$csid, c(952, 140526))
  #expect_equal(c2$Oxygen,c(952,140526)) does not work.
  #seems to be an API error.
  expect_equal(c3$csid, c(140526, 952))
  expect_equal(c4$csid, c(952, 140526))
  expect_equal(c5$csid, c(140526, 952))
  expect_equal(c6$csid, c(952, 140526))
  expect_equal(c7$csid, c(952, 140526))
  expect_equal(c8$csid, c(952, 140526))
  expect_equal(c9$csid, c(140526, 952))
})

test_that("cs_smiles_csid()", {
  skip_on_cran()
  a <- cs_smiles_csid("CC(O)=O")

  expect_is(a, "integer")
  expect_equal(a, 171)
})

test_that("cs_inchi_csid()", {
  skip_on_cran()
  a <- cs_inchi_csid(inchi = "InChI=1S/C2H4O2/c1-2(3)4/h1H3,(H,3,4)")

  expect_is(a, "integer")
  expect_equal(a, 171)
})

test_that("cs_inchikey_csid()", {
  skip_on_cran()
  a <- cs_inchikey_csid("QTBSBXVTEAMEQO-UHFFFAOYSA-N")

  expect_is(a, "integer")
  expect_equal(a, 171)
})

test_that("cs_convert_multiple()", {
  skip_on_cran()
  a <- cs_convert_multiple("CC(=O)O", "smiles", "inchi")
  a_rev <- cs_convert_multiple(a, "inchi", "smiles")
  b <- cs_convert_multiple("InChI=1S/C2H4O2/c1-2(3)4/h1H3,(H,3,4)", "inchi",
                           "inchikey")
  b_rev <- cs_convert_multiple(b, "inchikey", "inchi")
  c <- cs_convert_multiple("QTBSBXVTEAMEQO-UHFFFAOYSA-N", "inchikey", "mol")
  c_rev <- cs_convert_multiple(c, "mol", "inchikey")
  d <- cs_convert_multiple("InChI=1S/C2H4O2/c1-2(3)4/h1H3,(H,3,4)", "inchi",
                           "mol")

  expect_is(a, "character")
  expect_equal(a, "InChI=1/C2H4O2/c1-2(3)4/h1H3,(H,3,4)")
  expect_is(a_rev, "character")
  expect_equal(a_rev, "CC(=O)O")
  expect_is(b, "character")
  expect_equal(b, "QTBSBXVTEAMEQO-UHFFFAOYSA-N")
  expect_is(c, "character")
  expect_is(c_rev, "character")
  expect_equal(c_rev, "QTBSBXVTEAMEQO-UHFFFAOYSA-N")
  expect_is(d, "character")
})

test_that("cs_convert()", {
  skip_on_cran()
  a <- cs_convert(171, "csid", "inchi")
  a_rev <- cs_convert(a, "inchi", "csid")
  a2 <- cs_convert(c(171, 172), "csid", "inchi")
  a2_rev <- cs_convert(a2, "inchi", "csid")
  b <- cs_convert(171, "csid", "inchikey")
  b_rev <- cs_convert(b, "inchikey", "csid")
  b2 <- cs_convert(c(171, 172), "csid", "inchikey")
  b2_rev <- cs_convert(b2, "inchikey", "csid")
  c <- cs_convert(171, "csid", "smiles")
  c_rev <- cs_convert(c, "smiles", "csid")
  c2 <- cs_convert(c(171, 172), "csid", "smiles")
  c2_rev <- cs_convert(c2, "smiles", "csid")
  d <- cs_convert(171, "csid", "mol")
  expect_error(cs_convert(d, "mol", "csid"))
  d2 <- cs_convert(c(171, 172), "csid", "mol")
  e <- cs_convert("InChI=1S/C2H4O2/c1-2(3)4/h1H3,(H,3,4)", "inchi", "inchikey")
  e_rev <- cs_convert(e, "inchikey", "inchi")
  e2 <- cs_convert(a2, "inchi", "inchikey")
  e2_rev <- cs_convert(e2, "inchikey", "inchi")
  f <- cs_convert("InChI=1S/C2H4O2/c1-2(3)4/h1H3,(H,3,4)", "inchi", "smiles")
  f_rev <- cs_convert(f, "smiles", "inchi")
  f2 <- cs_convert(a2, "inchi", "smiles")
  f2_rev <- cs_convert(f2, "smiles", "inchi")
  g <- cs_convert("InChI=1S/C2H4O2/c1-2(3)4/h1H3,(H,3,4)", "inchi", "mol")
  #g_rev <- cs_convert(g, "mol", "inchi") possible db error
  g2 <- cs_convert(a2, "inchi", "mol")
  h <- cs_convert("QTBSBXVTEAMEQO-UHFFFAOYSA-N", "inchikey", "mol")
  h_rev <- cs_convert(h, "mol", "inchikey")
  h2 <- cs_convert(b2, "inchikey", "mol")
  h2_rev <- cs_convert(h2, "mol", "inchikey")

  expect_equal(a, "InChI=1/C2H4O2/c1-2(3)4/h1H3,(H,3,4)")
  expect_equal(a_rev, 171)
  expect_length(a2, 2)
  expect_length(a2_rev, 2)
  expect_equal(b, "QTBSBXVTEAMEQO-UHFFFAOYAR")
  expect_equal(b_rev, 171)
  expect_length(b2, 2)
  expect_length(b2_rev, 2)
  expect_length(c2, 2)
  expect_length(c2_rev, 2)
  expect_equal(c, "CC(=O)O")
  expect_equal(c_rev, 171)
  expect_is(d, "character")
  expect_length(d2, 2)
  expect_equal(e, "QTBSBXVTEAMEQO-UHFFFAOYSA-N")
  expect_equal(e_rev, "InChI=1S/C2H4O2/c1-2(3)4/h1H3,(H,3,4)")
  expect_length(e2, 2)
  expect_length(e2_rev, 2)
  expect_equal(f, "CC(=O)O")
  expect_equal(f_rev, "InChI=1/C2H4O2/c1-2(3)4/h1H3,(H,3,4)")
  expect_length(f2, 2)
  expect_length(f2_rev, 2)
  expect_is(g, "character")
  #expect_equal(g_rev, "InChI=1S/C2H4O2/c1-2(3)4/h1H3,(H,3,4)")
  #possible db error
  expect_length(g2, 2)
  expect_is(h, "character")
  expect_equal(h_rev, "QTBSBXVTEAMEQO-UHFFFAOYSA-N")
  expect_length(h2, 2)
  expect_length(h2_rev, 2)
})

test_that("cs_compinfo()", {
  skip_on_cran()
  a <- cs_compinfo(171, c("SMILES", "Formula", "InChI", "InChIKey", "StdInChI",
                          "StdInChIKey", "AverageMass", "MolecularWeight",
                          "MonoisotopicMass", "NominalMass", "CommonName",
                          "ReferenceCount", "DataSourceCount", "PubMedCount",
                          "RSCCount", "Mol2D", "Mol3D"))
  b <- cs_compinfo(c(171, 172), c("SMILES", "Formula", "InChI", "InChIKey",
                                  "StdInChI", "StdInChIKey", "AverageMass",
                                  "MolecularWeight", "MonoisotopicMass",
                                  "NominalMass", "CommonName", "ReferenceCount",
                                  "DataSourceCount", "PubMedCount", "RSCCount",
                                  "Mol2D", "Mol3D"))

  expect_is(a, "data.frame")
  expect_equal(dim(a), c(1, 18))
  expect_is(b, "data.frame")
  expect_equal(dim(b), c(2, 18))
})

test_that("cs_extcompinfo()", {
  skip_on_cran()

  comps <- c("2157", "5363")
  o1 <- cs_extcompinfo(comps)
  expect_is(o1, "data.frame")
  expect_equal(dim(o1), c(2, 14))
  expect_equal(o1$csid[1], "2157")
  expect_true(all(is.na(cs_extcompinfo(c(2157, NA))[2, 1:5])))
})

# integration tests
test_that("csid_extcompinfo(get_cid())", {
  skip_on_cran()

  tt <- get_csid("Triclosan")
  tt2 <- cs_extcompinfo(tt, verbose = FALSE)
  expect_equal(tt2[["average_mass"]],
               "289.5418")
  expect_equal(ncol(tt2), 14)
})