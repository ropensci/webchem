up <- ping_service("cir")
test_that("cir_query()", {
  skip_on_cran()
  skip_if_not(up, "CIR server is down")

  expect_equal(cir_query('Triclosan', 'mw')[[1]], 289.5451)
  expect_equal(cir_query('xxxxxxx', 'mw')[[1]], NA)
  expect_equal(cir_query("3380-34-5", 'stdinchikey', resolver = 'cas_number')[[1]],
            "InChIKey=XEFQLINVKFYRCS-UHFFFAOYSA-N")
  expect_true(length(cir_query('Triclosan', 'cas')[[1]]) > 1)
  expect_length(cir_query('Triclosan', 'cas', match = "first")[[1]], 1)
  expect_length(cir_query(c('Triclosan', 'Aspirin'), 'cas'), 2)

  expect_equal(cir_query('acetic acid', 'mw', match = "first"),
               list(`acetic acid` = 60.0524))

})

test_that("cir_query() doesn't mistake NA for sodium", {
  skip_on_cran()
  skip_if_not(up, "CIR server is down")

  expect_true(is.na(cir_query(as.character(NA), 'cas')))
})

test_that("cir_query() handles special characters in SMILES", {
  skip_on_cran()
  skip_if_not(up, "CIR server is down")

  expect_equal(cir_query("C#C", representation = "inchikey")[[1]],
               "InChIKey=HSFWRNGVRCDJHI-UHFFFAOYNA-N")
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
