context("wikidata")


test_that("get_wdid returns correct results", {
  # test general
  comps <- c('DDT', 'Aspirin', 'xdewrwdcadsr4w')
  o1 <- get_wdid(comps, match = 'best')
  o2 <- get_wdid(comps, match = 'all')
  o3 <- get_wdid(comps[1], match = 'first')
  o4 <- get_wdid(comps[1], match = 'na')


  expect_is(o1, 'data.frame')
  expect_is(o2, 'list')
  expect_is(o3, 'data.frame')
  expect_is(o4, 'data.frame')

  expect_equal(o1$id, c("Q163648", "Q18216", NA))
  expect_equivalent(o2[[1]][1:2], c("Q163648", "Q949424"))
  expect_equal(o3$distance, 'first')
  expect_equal(o4$distance, NA)
})


test_that("wd_ident returns correct results", {
  id <- c( "Q163648", "Q18216", "asndalsr", NA)
  o1 <- wd_ident(id)
  expect_is(o1, 'data.frame')
  expect_equal(nrow(o1), 4)
  expect_true(is.na(o1$smiles[3]))
  expect_true(is.na(o1$smiles[4]))
  expect_equal(o1$cas[1], '50-29-3')
  expect_equal(names(o1), c('smiles', 'cas', 'cid', 'einecs', 'csid', 'inchi', 'inchikey',
                           'drugbank', 'zvg', 'chebi', 'chembl', 'unii', 'source_url', 'query'))
})


test_that("wd integration test", {
  d <- wd_ident(get_wdid('Glyphosate', 'en', 'best')$id)
  f <- wd_ident(get_wdid('xxxxx', 'en', 'best')$id)

  expect_equal(d$cas, "1071-83-6")
  expect_equal(ncol(d), 14)
  expect_is(d, 'data.frame')
  expect_true(all(is.na(f[1, ])))
})