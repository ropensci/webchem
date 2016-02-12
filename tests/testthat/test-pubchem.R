context("pubchem")

test_that("get_cid()", {
  expect_equal(get_cid('Triclosan')[[1]], 5564)
  expect_true(length(get_cid('Triclosan', arg = 'name_type=word')[[1]]) > 1)
  expect_true(length(get_cid('Triclosan', arg = 'name_type=word', first = TRUE)[[1]]) == 1)
  expect_true(length(get_cid(c('Triclosan', 'Aspirin'))) == 2)
  expect_equal(get_cid('xxxxx', verbose = FALSE)[[1]], NA)
  expect_equal(get_cid("BPGDAMSIGCZZLK-UHFFFAOYSA-N", from = 'inchikey')[[1]], 12345)
})


test_that("pc_prop", {
  a <- pc_prop('5564', properties = 'CanonicalSmiles', verbose = FALSE)
  b <- pc_prop('xxx', properties = 'CanonicalSmiles', verbose = FALSE)
  c <- pc_prop('5564', properties = c('CanonicalSmiles', 'InChiKey'), verbose = FALSE)
  expect_equal(a$CanonicalSMILES, "C1=CC(=C(C=C1Cl)O)OC2=C(C=C(C=C2)Cl)Cl")
  expect_true(is.na(b))
  expect_is(a, 'data.frame')
  expect_equal(ncol(c), 3)
})

test_that("pc_synonyms", {
  expect_equal(pc_synonyms('Triclosan')[[1]][1], '5564')
  expect_equal(length(pc_synonyms(c('Triclosan', 'Aspirin'))), 2)
  expect_equal(pc_synonyms("BPGDAMSIGCZZLK-UHFFFAOYSA-N", from = 'inchikey')[[1]][1], "12345")
  expect_true(is.na(pc_synonyms('xxxxx')[[1]]))
})


test_that("cid integration tests", {
  expect_equal(pc_prop(get_cid('Triclosan')[[1]], properties = 'CanonicalSmiles')$CanonicalSMILES, "C1=CC(=C(C=C1Cl)O)OC2=C(C=C(C=C2)Cl)Cl")
  expect_true(is.na(pc_prop(NA, properties = 'CanonicalSmiles', verbose = FALSE)))
})