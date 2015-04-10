context("pubchem")

test_that("get_cid()", {
  expect_equal(get_cid('Triclosan')[1], '4093', verbose = FALSE)
  expect_equal(length(get_cid('Triclosan', first = TRUE, verbose = FALSE)), 1)
  expect_equal(get_cid('xxxxx', verbose = FALSE), NA)
  expect_error(get_cid(c('xxxxx', 'aaaaaaa')))
  expect_warning(get_cid(NA))
})


test_that("cid_compinfo", {
  expect_error(cid_compinfo(c('xxxxx', 'aaaaaaa')))
  expect_equal(cid_compinfo('4093', verbose = FALSE)$CanonicalSmiles, "CC(=C)C(=O)O")
  expect_equal(length(cid_compinfo('4093', verbose = FALSE)), 25)
  expect_equal(length(cid_compinfo('4093', first = TRUE, verbose = FALSE)$synonyms), 1)
  expect_equal(get_cid('xxxxx', verbose = FALSE), NA)
})