context("pubchem")

test_that("examples in the article are unchanged", {
  #values come from test-etox
  cas <- c("105-67-9", "1570-64-5", NA, "1912-24-9", "71-43-2", "6190-65-4")
  cids <- get_cid(cas)
  pc_data <- pc_prop(cids, properties = "CanonicalSMILES")
  #values go to test-chemspider
  pc_smiles <- smiles(pc_data)

  expect_equal(cids, c(7771, 14855, NA, 2256, 241, 22563))
  expect_equal(pc_smiles, c("CC1=CC(=C(C=C1)O)C", "CC1=C(C=CC(=C1)Cl)O", NA,
                            "CCNC1=NC(=NC(=N1)Cl)NC(C)C", "C1=CC=CC=C1",
                            "CC(C)NC1=NC(=NC(=N1)N)Cl"))
})

test_that("get_cid()", {
  skip_on_cran()

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

  a <- pc_prop("5564", properties = "CanonicalSmiles", verbose = FALSE)
  b <- suppressWarnings(pc_prop("xxx", properties = "CanonicalSmiles", verbose = FALSE))
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
  expect_true(is.na(suppressWarnings(pc_synonyms("xxxx"))[[1]]))
})

test_that("cid integration tests", {
  skip_on_cran()

  expect_equal(pc_prop(get_cid("Triclosan")$cid[1],
                       properties = "CanonicalSmiles")$CanonicalSMILES,
               "C1=CC(=C(C=C1Cl)O)OC2=C(C=C(C=C2)Cl)Cl")
  expect_true(is.na(suppressWarnings(pc_prop(NA, properties = "CanonicalSmiles",
                            verbose = FALSE))))
})
