context("pubchem")

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
  # issue 206, some queries first return a listkey.
  expect_equal(get_cid(5564, from="fastsimilarity_2d/cid")$cid[1], "5564")
  expect_equal(get_cid("C26H52NO6P", from = "formula")$cid[1], "10864091")
  expect_equal(capture_messages(get_cid("balloon")),
               c("Querying balloon. ", "Not Found (HTTP 404).", "\n"))
})

test_that("get_sid()", {
  skip_on_cran()

  a <- get_sid("2-(Acetyloxy)benzoic acid", from = "name")
  a2 <- get_sid(c("balloon", NA, "aspirin"), from = "name")
  b <- get_sid("57-27-2", from = "xref/rn")
  c <- get_sid(c("VCC957895", "VCC883692"), from = "sourceid/23706")

  expect_true("tbl" %in% class(a))
  expect_true(87798 %in% a$sid)
  expect_equal(is.na(a2$sid[1:2]), c(TRUE, TRUE))
  expect_true(4681 %in% b$sid)
  expect_equal(c$sid, c("385515341", "385515340"))
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
