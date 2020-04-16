context("pubchem")

test_that("get_cid()", {
  skip_on_cran()

  expect_equal(get_cid("Triclosan")[[1]], 5564)
  expect_true(length(get_cid("Triclosan", arg = "name_type=word")[[1]]) > 1)
  expect_true(length(get_cid("Triclosan", arg = "name_type=word",
                             first = TRUE)[[1]]) == 1)
  expect_true(length(get_cid(c("Triclosan", "Aspirin"))) == 2)
  expect_true(is.na(get_cid("xxxx", verbose = FALSE)[[1]]))
  expect_true(is.na(get_cid(NA)))
  expect_equal(get_cid("BPGDAMSIGCZZLK-UHFFFAOYSA-N", from = "inchikey")[[1]],
               12345)
})


test_that("pc_prop", {
  skip_on_cran()

  a <- pc_prop("5564", properties = "CanonicalSmiles", verbose = FALSE)
  b <- pc_prop("xxx", properties = "CanonicalSmiles", verbose = FALSE)
  c <- pc_prop("5564", properties = c("CanonicalSmiles", "InChiKey"),
               verbose = FALSE)
  expect_equal(a$CanonicalSMILES, "C1=CC(=C(C=C1Cl)O)OC2=C(C=C(C=C2)Cl)Cl")
  expect_true(is.na(b))
  expect_is(a, "data.frame")
  expect_equal(ncol(c), 3)
})

test_that("pc_synonyms", {
  skip_on_cran()

  expect_equal(pc_synonyms("Triclosan")[[1]][1], "5564")
  expect_equal(length(pc_synonyms(c("Triclosan", "Aspirin"))), 2)
  expect_equal(pc_synonyms("BPGDAMSIGCZZLK-UHFFFAOYSA-N",
                           from = "inchikey")[[1]][1], "12345")
  expect_true(is.na(pc_synonyms("xxxx")[[1]]))
})

test_that("cid integration tests", {
  skip_on_cran()

  expect_equal(pc_prop(get_cid("Triclosan")[[1]],
                       properties = "CanonicalSmiles")$CanonicalSMILES,
               "C1=CC(=C(C=C1Cl)O)OC2=C(C=C(C=C2)Cl)Cl")
  expect_true(is.na(pc_prop(NA, properties = "CanonicalSmiles",
                            verbose = FALSE)))
})

test_that("pc_prurl", {
  #name/names to cid

  a <- pc_prurl(2244, domain = "compound", from = "cid",
                to = "property/MolecularFormula", output = "JSON")
  expect_equal(strsplit(a$qurl, "pug/")[[1]][2],
               "compound/cid/property/molecularformula/json")
  expect_equal(a$body, "cid=2244")
  b <- pc_prurl(c(1234,2244), domain = "compound", from = "cid",
                to = "property/MolecularFormula", output = "JSON")
  expect_equal(strsplit(b$qurl, "pug/")[[1]][2],
               "compound/cid/property/molecularformula/json")
  expect_equal(b$body, "cid=1234,2244")
  c <- pc_prurl("5F1CA2B314D35F28C7F94168627B29E3", domain = "substance",
                from = "sourceid/ibm", output = "ASNT")
  expect_equal(strsplit(c$qurl, "pug/")[[1]][2],
               "substance/sourceid/ibm/asnt")
  expect_equal(c$body, "sourceid=5F1CA2B314D35F28C7F94168627B29E3")
  d <- pc_prurl(747285, domain = "substance",
                from = "sourceid/dtp/nci", output = "sdf")
  expect_equal(strsplit(d$qurl, "pug/")[[1]][2],
               "substance/sourceid/dtp.nci/sdf")
  expect_equal(d$body, "sourceid=747285")
  e <- pc_prurl(747285, domain = "substance",
                from = "sourceid/dtp/nci", output = "png")
  expect_equal(strsplit(e$qurl, "pug/")[[1]][2],
               "substance/sourceid/dtp.nci/png")
  expect_equal(e$body, "sourceid=747285")
  f <- pc_prurl(2244, domain = "compound",
                from = "cid", output = "sdf")
  expect_equal(strsplit(f$qurl, "pug/")[[1]][2],
               "compound/cid/sdf")
  expect_equal(f$body, "cid = 2244")
  g <- pc_prurl("aspirin", domain = "compound",
                from = "name", output = "json")
  expect_equal(strsplit(g$qurl, "pug/")[[1]][2],
               "compound/name/json")
  expect_equal(g$body, "name = aspirin")
  h <- pc_prurl(c(1,2,3,4,5), domain = "compound", from = "cid",
                to = "property/molecularformula,molecularweight",
                output = "xml")
  expect_equal(strsplit(h$qurl, "pug/")[[1]][2],
               "compound/cid/property/molecularformula,molecularweight/xml")
  expect_equal(h$body, "cid=1,2,3,4,5")
  i <- pc_prurl("acetic acid", domain = "compound", from = "name",
                to = "cids",
                output = "json")
  expect_equal(strsplit(i$qurl, "pug/")[[1]][2],
               "compound/name/cids/json")
  expect_equal(h$body, "name=acetic acid")



})

test_that("pc_pugrest", {
  #pc_pugrest(176, "compound", "cid",  "synonyms", "JSON")
  #pc_pugrest("EU REGULATION (EC) No 1272/2008", substance, )
  #pc_pugrest("DC Chemicals", "substance", "sourceall",  "sids", "JSON")
})
