test_that("examples in the article are unchanged", {

  #values come from test-pubchem
  smiles <- c("CC1=CC(=C(C=C1)O)C", "CC1=C(C=CC(=C1)Cl)O", NA,
              "CCNC1=NC(=NC(=N1)Cl)NC(C)C", "C1=CC=CC=C1",
              "CC(C)NC1=NC(=NC(=N1)N)Cl")

  vcr::use_cassette("cs-article-examples", {
    csids <- get_csid(smiles, from = "smiles")
    inchikeys <- cs_convert(csids$csid, from = "csid", to = "inchikey")
  })

  expect_equal(csids$csid, c(13839123, 14165, NA, 2169, 236, 21157))
  expect_equal(inchikeys,
               c("KUFFULVDNCHOFZ-UHFFFAOYAC", "RHPUJHQBPORFGV-UHFFFAOYAB", NA,
                 "MXWJVTOOROXGIU-UHFFFAOYAJ", "UHOVQNZJYSORNB-UHFFFAOYAH",
                 "DFWFIQKMSFGDCQ-UHFFFAOYAI"))
})

test_that("cs_check_key() can find API key in my local .Renviron", {

    expect_type(cs_check_key(), "character")
})

test_that("cs_datasources()", {

  vcr::use_cassette("cs_datasources()", {
    a <- cs_datasources()
  })

  expect_type(a, "character")
})

test_that("cs_control()", {
  expect_type(cs_control(), "list")
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
  expect_true(cs_control(include_all = TRUE)$include_all)
  expect_false(cs_control(include_all = FALSE)$include_all)
  expect_true(cs_control(complexity = "any")$complexity == "any")
  expect_true(cs_control(complexity = "single")$complexity == "single")
  expect_true(cs_control(complexity = "multiple")$complexity == "multiple")
  expect_true(cs_control(isotopic = "any")$isotopic == "any")
  expect_true(cs_control(isotopic = "labeled")$isotopic == "labeled")
  expect_true(cs_control(isotopic = "unlabeled")$isotopic == "unlabeled")
})

test_that("get_csid()", {

  vcr::use_cassette("get_csid()", {
    # get_csid() works with defaults
    a <- get_csid("Triclosan")
    b <- get_csid("Naproxene")
    ab <- get_csid(c("Triclosan", "Naproxene"))
    abcd <- get_csid(c("ethanol", "balloon", NA, "acetic acid"),
                     match = "first")

    # get_csid() works with cs_control()
    c1 <- head(get_csid("iron oxide", from = "name", order_by = "recordId"))
    c3 <- head(get_csid("iron oxide", from = "name",
                        order_by = "molecularWeight"))
    c4 <- head(get_csid("C6H12O6", from = "formula",
                        order_by = "referenceCount",
                        order_direction = "descending"))
    c5 <- head(get_csid("C6H12O6", from = "formula", order_by = "dataSourceCount",
                        order_direction = "descending"))
    c6 <- head(get_csid("C6H12O6", from = "formula", order_by = "pubMedCount",
                        order_direction = "descending"))
    c7 <- head(get_csid("C6H12O6", from = "formula", order_by = "rscCount",
                        order_direction = "descending"))
    c8 <- head(get_csid("iron oxide", from = "name", order_by = "molecularWeight",
                        order_direction = "descending"))

    # get_csid() handles special characters in SMILES
    d1 <- get_csid("C#C", from = "smiles")

    # get_csid() can query smiles
    e1 <- get_csid("CC(O)=O", from = "smiles")

    # get_csid() can query inchis
    f1 <- get_csid("InChI=1S/C2H4O2/c1-2(3)4/h1H3,(H,3,4)", from = "inchi")

    # get_csid() can query inchikeys
    g1 <- get_csid("QTBSBXVTEAMEQO-UHFFFAOYSA-N", from = "inchikey")
    })

  # get_csid() works with defaults
  expect_s3_class(a, "data.frame")
  expect_equal(a$csid, 5363)
  expect_equal(b$csid, 137720)
  expect_equal(ab$csid, c(5363, 137720))
  expect_equal(abcd$csid, c(682, NA, NA, 171))

  # get_csid() works with cs_control()
  expect_equal(c1$csid, c(14147, 14237, 55474, 82623, 392353, 396260))
  expect_equal(c3$csid, c(14237, 396260, 392353, 82623, 14147, 452497))
  expect_equal(c4$csid, c(23139, 1070, 868, 388747, 161434, 96749))
  expect_equal(c5$csid, c(10239179, 5764, 58238, 71358, 83142, 96749))
  expect_equal(c6$csid, c(96749, 5589, 71358, 58238, 9484839, 9312824))
  expect_equal(c7$csid, c(96749, 5589, 71358, 58238, 9312824, 9484839))
  expect_equal(c8$csid, c(4937312, 55474, 14147, 452497, 82623, 392353))

  # get_csid() handles special characters in SMILES
  expect_equal(d1$csid, 6086)

  # get_csid() can query smiles
  expect_type(e1$csid, "integer")
  expect_equal(e1$csid, 171)

  # get_csid() can query inchis
  expect_type(f1$csid, "integer")
  expect_equal(f1$csid, 171)

  # get_csid() can query inchikeys
  expect_type(g1$csid, "integer")
  expect_equal(g1$csid, 171)
})

test_that("cs_convert()", {

  vcr::use_cassette("cs_convert()", {
    a <- cs_convert(171, "csid", "inchi")
    a_rev <- cs_convert(a, "inchi", "csid")
    a2 <- cs_convert(c(171, 172), "csid", "inchi")
    a2_rev <- cs_convert(a2, "inchi", "csid")
    b_rev <- cs_convert("QTBSBXVTEAMEQO-UHFFFAOYAR", "inchikey", "csid")
    b2_rev <- cs_convert(
      c("QTBSBXVTEAMEQO-UHFFFAOYAR", "IKHGUXGNUITLKF-UHFFFAOYSA-N"),
      "inchikey", "csid")
    c <- cs_convert(171, "csid", "smiles")
    c_rev <- cs_convert(c, "smiles", "csid")
    c2 <- cs_convert(c(171, 172), "csid", "smiles")
    c2_rev <- cs_convert(c2, "smiles", "csid")
    d <- cs_convert(171, "csid", "mol")
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
    h2 <- cs_convert(
      c("QTBSBXVTEAMEQO-UHFFFAOYAR", "IKHGUXGNUITLKF-UHFFFAOYSA-N"),
      "inchikey", "mol")
    h2_rev <- cs_convert(h2, "mol", "inchikey")

    # cs_convert() handles special characters in SMILES
    i1 <- cs_convert("C#C", from = "smiles", to = "csid")
    i2 <- cs_convert("C#C", from = "smiles", to = "inchi")
  })

  expect_equal(a, "InChI=1/C2H4O2/c1-2(3)4/h1H3,(H,3,4)")
  expect_equal(a_rev, 171)
  expect_length(a2, 2)
  expect_length(a2_rev, 2)
  expect_equal(b_rev, 171)
  expect_length(b2_rev, 2)
  expect_length(c2, 2)
  expect_length(c2_rev, 2)
  expect_equal(c, "CC(=O)O")
  expect_equal(c_rev, 171)
  expect_type(d, "character")
  expect_length(d2, 2)
  expect_equal(e, "QTBSBXVTEAMEQO-UHFFFAOYSA-N")
  expect_equal(e_rev, "InChI=1S/C2H4O2/c1-2(3)4/h1H3,(H,3,4)")
  expect_length(e2, 2)
  expect_length(e2_rev, 2)
  expect_equal(f, "CC(=O)O")
  expect_equal(f_rev, "InChI=1/C2H4O2/c1-2(3)4/h1H3,(H,3,4)")
  expect_length(f2, 2)
  expect_length(f2_rev, 2)
  expect_type(g, "character")
  #expect_equal(g_rev, "InChI=1S/C2H4O2/c1-2(3)4/h1H3,(H,3,4)")
  #possible db error
  expect_length(g2, 2)
  expect_type(h, "character")
  expect_equal(h_rev, "QTBSBXVTEAMEQO-UHFFFAOYSA-N")
  expect_length(h2, 2)
  expect_length(h2_rev, 2)

  # cs_convert() handles special characters in SMILES
  expect_equal(i1, 6086)
  expect_equal(i2, "InChI=1/C2H2/c1-2/h1-2H")
})

test_that("cs_compinfo()", {

  vcr::use_cassette("cs_compinfo()", {
    a <- cs_compinfo(
      171,
      c("SMILES", "Formula", "InChI", "InChIKey", "StdInChI", "StdInChIKey",
        "AverageMass", "MolecularWeight", "MonoisotopicMass", "NominalMass",
        "CommonName", "ReferenceCount", "DataSourceCount", "PubMedCount",
        "RSCCount", "Mol2D", "Mol3D"))
    b <- cs_compinfo(
      c(171, 172),
      c("SMILES", "Formula", "InChI", "InChIKey", "StdInChI", "StdInChIKey",
        "AverageMass", "MolecularWeight", "MonoisotopicMass", "NominalMass",
        "CommonName", "ReferenceCount", "DataSourceCount", "PubMedCount",
        "RSCCount", "Mol2D", "Mol3D"))
  })

  expect_s3_class(a, "data.frame")
  expect_equal(dim(a), c(1, 18))
  expect_s3_class(b, "data.frame")
  expect_equal(dim(b), c(2, 18))
})

#need to figure out how to test images with vcr
#test_that("cs_img()", {

  #imgs <- cs_img(c(682, 5363, "balloon", NA), dir = tempdir())

  #expect_true(file.exists(paste0(tempdir(), "/", "682.png")))
  #expect_true(file.exists(paste0(tempdir(), "/", "5363.png")))
#})
