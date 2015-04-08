context("pubchem")

test_that("get_cid()", {
  expect_equal(get_cid('Triclosan')[1], '4093')
  expect_equal(length(get_cid('Triclosan', first = TRUE)), 1)
  expect_equal(get_cid('xxxxx'), NA)
  expect_error(get_cid(c('xxxxx', 'aaaaaaa')))
})


test_that("cid_compinfo", {
  expect_error(cid_compinfo(c('xxxxx', 'aaaaaaa')))
  expect_equal(cid_compinfo('4093')$CanonicalSmiles, "CC(=C)C(=O)O")
  expect_equal(length(cid_compinfo('4093')), 25)
  expect_equal(length(cid_compinfo('4093', first = TRUE)$synonyms), 1)
  expect_equal(get_cid('xxxxx'), NA)
})