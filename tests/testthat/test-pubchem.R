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

test_that("pc_page()", {
  a <- pc_page(c(311, 176, 1118, "balloon", NA), "pKa")
  b <- pc_page(c(311, 176, 1118, "balloon", NA), "therapeutic uses")
  c <- pc_page(c(311, 176, 1118, "balloon", NA), "spectral information")
  d <- pc_page(1, "external id", type = "substance")
  e <- pc_page(1, "description", type = "assay")
  f <- pc_page("US5837728", "patent title", type = "patent")
  g <- pc_page(1, "synonyms", type = "gene")

  expect_is(a, "list")
  expect_length(a, 5)
  expect_is(a[[1]], "list")
  expect_is(a[[2]], "list")
  expect_equal(a[[3]], NA)
  expect_equal(a[[4]], NA)
  expect_equal(a[[5]], NA)

  expect_is(b, "list")
  expect_length(b, 5)
  expect_is(b[[1]], "list")
  expect_is(b[[2]], "list")
  expect_is(b[[3]], "list")
  expect_equal(a[[4]], NA)
  expect_equal(a[[5]], NA)

  expect_is(c, "list")
  expect_length(c, 5)
  expect_is(c[[1]], "list")
  expect_is(c[[2]], "list")
  expect_is(c[[3]], "list")
  expect_equal(a[[4]], NA)
  expect_equal(a[[5]], NA)
})

test_that("pc_extract() chemical and physical properties", {
  s <- pc_page(c(NA,176,311,"balloon"), "chemical and physical properties")
  mw <- pc_extract(s, "molecular weight") # example for a computed property
  pd <- pc_extract(s, "physical description") # textual description
  bp <- pc_extract(s, "boiling point")
  mp <- pc_extract(s, "melting point")
  fp <- pc_extract(s, "flash point")
  so <- pc_extract(s, "solubility") # data with headers
  ow <- pc_extract(s, "octanol/water partition coefficient") #negative numbers
  logs <- pc_extract(s, "logs")
  logkoa <- pc_extract(s, "logkoa")
  ri <- pc_extract(s, "kovats retention index")

})
