up <- ping_service("cir")
test_that("cir_query()", {
  skip_on_cran()
  skip_if_not(up, "CIR server is down")

  expect_equal(cir_query('Triclosan', 'mw')$mw[1], 289.5451)
  expect_equal(cir_query('xxxxxxx', 'mw')$mw[1], NA)
  expect_equal(cir_query("3380-34-5", 'stdinchikey', resolver = 'cas_number')$stdinchikey[1],
            "InChIKey=XEFQLINVKFYRCS-UHFFFAOYSA-N")
  expect_true(nrow(cir_query('Triclosan', 'cas')) > 1)
  expect_true(nrow(cir_query('Triclosan', 'cas', match = "first")) == 1)
  expect_true(nrow(cir_query(c('Triclosan', 'Aspirin'), 'cas', match = "first")) == 2)

  expect_equal(cir_query('acetic acid', 'mw', match = "first"),
               tibble(query = "acetic acid", mw = 60.0524))

})

test_that("cir_query() doesn't mistake NA for sodium", {
  skip_on_cran()
  skip_if_not(up, "CIR server is down")

  expect_true(is.na(cir_query(as.character(NA), 'cas')$cas))
})

test_that("cir_query() handles special characters in SMILES", {
  skip_on_cran()
  skip_if_not(up, "CIR server is down")

  expect_equal(cir_query("C#C", representation = "inchikey")$inchikey,
               "InChIKey=HSFWRNGVRCDJHI-UHFFFAOYNA-N")
})

test_that("cir_query() handles NA queries and queries that return NA", {
  skip_on_cran()
  skip_if_not(up, "CIR server is down")

  expect_identical(
    cir_query(c("Triclosan", "pumpkin", NA), representation = "cas",match = "first"),
    tibble(query = c("Triclosan", "pumpkin", NA_character_),
           cas = c("3380-34-5", NA_character_, NA_character_))
  )
})

test_that("cir_img()", {
  skip_on_cran()
  skip_if_not(up, "CIR server is down")

  expect_true(is.null(cir_img('CCO', tempdir())))
  fl <- file.path(tempdir(), 'CCO.png')
  expect_true(file.exists(fl))
  fl2 <- file.path(tempdir(), 'abcdefghijk.png')
  cir_img('abcdefghijk', tempdir())
  expect_true(!file.exists(fl2))
})
