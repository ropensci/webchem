context("pubchem")

test_that("get_cid()", {
  expect_equal(get_cid('Triclosan')[[1]], '5564')
  expect_true(length(get_cid('Triclosan', arg = 'name_type=word')[[1]]) > 1)
  expect_true(length(get_cid('Triclosan', arg = 'name_type=word', first = TRUE)[[1]]) == 1)
  expect_true(length(get_cid(c('Triclosan', 'Aspirin'))) == 2)
  expect_equal(get_cid('xxxxx', verbose = FALSE)[[1]], NA)
  expect_equal(get_cid("BPGDAMSIGCZZLK-UHFFFAOYSA-N", from = 'inchikey')[[1]], 12345)
})


test_that("pc_compinfo", {
  chk_pubchem()

  expect_error(pc_compinfo(c('xxxxx', 'aaaaaaa')))
  chk_pubchem()
  a <- pc_compinfo('5564', verbose = FALSE)
  expect_equal(a$CanonicalSmiles, "C1=CC(=C(C=C1Cl)O)OC2=C(C=C(C=C2)Cl)Cl")
  expect_equal(length(a), 26)
  chk_pubchem()
  expect_equal(pc_compinfo('xxxxx', verbose = FALSE), NA)
  expect_warning(pc_compinfo('xxxxx', verbose = TRUE))
})


test_that("cid integration tests", {
  chk_pubchem()
  expect_equal(pc_compinfo(get_pcid('Triclosan')[1])$CanonicalSmiles, "C1=CC(=C(C=C1Cl)O)OC2=C(C=C(C=C2)Cl)Cl")
  expect_true(is.na(pc_compinfo(get_pcid('xxxxxx')[1])))
  expect_warning(pc_compinfo(get_pcid('xxxxxx')[1]))
})