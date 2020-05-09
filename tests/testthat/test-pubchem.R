up <- ping_service("pc")
test_that("get_cid()", {
  skip_on_cran()
  skip_if_not(up, "PubChem service is down")


  expect_equal(get_cid("Triclosan")$cid[1], "5564")
  expect_true(nrow(get_cid("Triclosan", arg = "name_type=word")) > 1)
  expect_true(nrow(get_cid("Triclosan", arg = "name_type=word",
                             match = "first")) == 1)
  expect_true(nrow(get_cid(c("Triclosan", "Aspirin"))) == 2)
  expect_true(is.na(suppressWarnings(get_cid("xxxx", verbose = FALSE))$cid[1]))
  expect_warning(
    get_cid("xxxx", verbose = FALSE),
    "No CID found that matches the given name. Returning NA."
  )
  expect_true(is.na(get_cid(NA)$cid[1]))
  expect_equal(get_cid("BPGDAMSIGCZZLK-UHFFFAOYSA-N", from = "inchikey")$cid[1],
               "12345")
})


test_that("pc_prop", {
  skip_on_cran()
  skip_if_not(up, "PubChem service is down")

  a <- pc_prop("5564", properties = "CanonicalSmiles", verbose = FALSE)
  b <- suppressWarnings(pc_prop("xxx", properties = "CanonicalSmiles", verbose = FALSE))
  c <- pc_prop("5564", properties = c("CanonicalSmiles", "InChiKey"),
               verbose = FALSE)
  expect_equal(a$CanonicalSMILES, "C1=CC(=C(C=C1Cl)O)OC2=C(C=C(C=C2)Cl)Cl")
  expect_true(is.na(b))
  expect_s3_class(a, "data.frame")
  expect_equal(ncol(c), 3)
})

test_that("pc_synonyms", {
  skip_on_cran()
  skip_if_not(up, "PubChem service is down")

  expect_equal(pc_synonyms("Triclosan")[[1]][1], "5564")
  expect_equal(length(pc_synonyms(c("Triclosan", "Aspirin"))), 2)
  expect_equal(pc_synonyms("BPGDAMSIGCZZLK-UHFFFAOYSA-N",
                           from = "inchikey")[[1]][1], "12345")
  expect_true(is.na(suppressWarnings(pc_synonyms("xxxx"))[[1]]))
})

test_that("cid integration tests", {
  skip_on_cran()
  skip_if_not(ping_pubchem(), "PubChem service is down")

  expect_equal(pc_prop(get_cid("Triclosan")$cid[1],
                       properties = "CanonicalSmiles")$CanonicalSMILES,
               "C1=CC(=C(C=C1Cl)O)OC2=C(C=C(C=C2)Cl)Cl")
  expect_true(is.na(suppressWarnings(pc_prop(NA, properties = "CanonicalSmiles",
                            verbose = FALSE))))
})

test_that("pc_page()", {
  skip_on_cran()
  skip_if_not(up, "PubChem service is down")

  a <- pc_page(c(311, 176, 1118, "balloon", NA), "pKa")

  expect_type(a, "list")
  expect_length(a, 5)
  expect_is(a[[1]], c("Node", "R6"))
  expect_is(a[[2]], c("Node", "R6"))
  expect_equal(a[[3]], NA)
  expect_equal(a[[4]], NA)
  expect_equal(a[[5]], NA)
})

test_that("pc_extract() chemical and physical properties", {
  skip_on_cran()
  skip_if_not(up, "PubChem service is down")

  s <- pc_page(c(NA, 176, 311, "balloon"), "chemical and physical properties")
  mw <- pc_extract(s, "molecular weight") # example for a computed property
  pd <- pc_extract(s, "physical description") # textual description
  bp <- pc_extract(s, "boiling point")
  mp <- pc_extract(s, "melting point")
  fp <- pc_extract(s, "flash point")
  so <- pc_extract(s, "solubility") # data with headers
  ow <- pc_extract(s, "octanol/water partition coefficient") #negative numbers
  logs <- pc_extract(s, "logs")
  logkoa <- pc_extract(s, "logkoa")
})

test_that("pc_sect()", {
  skip_on_cran()
  skip_if_not(up, "PubChem service is down")

  a <- pc_sect(c(311, 176, 1118, "balloon", NA), "pKa")
  expect_s3_class(a, c("tbl_df", "tbl", "data.frame"))
  expect_equal(names(a), c("CID", "Name", "Result", "SourceName", "SourceID"))
  expect_equal(a$CID, c("311", "176", "1118", "balloon", NA))
  expect_equal(a$Name, c("Citric acid", "Acetic acid", NA, NA, NA))
  expect_equal(a$Result, c("2.79", "4.76 (at 25 °C)", NA, NA, NA))
  expect_equal(a$SourceName, c("DrugBank", "DrugBank", NA, NA, NA))
  expect_equal(a$SourceID, c("DB04272", "DB03166", NA, NA, NA))

  b <- pc_sect(2231, "depositor-supplied synonyms", "substance")
  expect_s3_class(b, c("tbl_df", "tbl", "data.frame"))
  expect_equal(names(b), c("SID", "Name", "Result", "SourceName", "SourceID"))
  expect_equivalent(b$Result, c("cholesterol", "57-88-5", "5-cholestene-3beta-ol"))

  c <- pc_sect(780286, "modify date", "assay")
  expect_s3_class(c, c("tbl_df", "tbl", "data.frame"))
  expect_equal(names(c), c("AID", "Name", "Result", "SourceName", "SourceID"))
  expect_equal(c$Result, c("2014-05-03", "2018-09-28"))

  d <- pc_sect("1ZHY_A", "Sequence", "protein")
  expect_s3_class(d, c("tbl_df", "tbl", "data.frame"))
  expect_equal(names(d), c("pdbID", "Name", "Result", "SourceName", "SourceID"))
  expect_equivalent(d$Result[1], ">pdb|1ZHY|A Chain A, 1 Kes1 Protein (Run BLAST)")

  e <- pc_sect("US2013040379", "Patent Identifier Synonyms", "patent")
  expect_s3_class(e, c("tbl_df", "tbl", "data.frame"))
  expect_equal(names(e), c("PatentID", "Name", "Result", "SourceName",
                           "SourceID"))
  expect_equivalent(e$Result, c("US20130040379", "US20130040379A1",
                           "US2013040379A1"))
})
