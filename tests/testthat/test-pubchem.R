context("pubchem")

test_that("get_cid()", {
  expect_equal(get_cid('Triclosan')[1], '5564', verbose = FALSE)
  expect_equal(length(get_cid('Triclosan', first = TRUE, verbose = FALSE)), 1)
  expect_equal(get_cid('xxxxx', verbose = FALSE), NA)
  expect_error(get_cid(c('xxxxx', 'aaaaaaa')))
  expect_warning(get_cid(NA))
})


test_that("cid_compinfo", {
  expect_error(cid_compinfo(c('xxxxx', 'aaaaaaa')))
  expect_equal(cid_compinfo('5564', verbose = FALSE)$CanonicalSmiles, "C1=CC(=C(C=C1Cl)O)OC2=C(C=C(C=C2)Cl)Cl")
  expect_equal(length(cid_compinfo('5564', verbose = FALSE)), 25)
  expect_equal(length(cid_compinfo('5564', first = TRUE, verbose = FALSE)$synonyms), 1)
  expect_equal(get_cid('xxxxx', verbose = FALSE), NA)
})