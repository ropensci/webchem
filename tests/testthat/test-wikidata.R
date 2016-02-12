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
  id <- "Q407232" # glyphosate
  d <- wd_ident(id)
  d2 <- wd_ident('Q408646')
  f <- wd_ident('xxxxx')

  expect_error(wd_ident(c(id, id)))
  expect_equal(d$cas, "1071-83-6")
  expect_equal(length(d), 13)
  expect_equal(f, NA)
  expect_message(wd_ident('xxxxx'))

  expect_equal(d2$cas, "3380-34-5")
  expect_equal(length(d2), 13)
  expect_equal(names(d2), c('smiles', 'cas', 'cid', 'einecs', 'csid', 'inchi', 'inchikey',
                           'drugbank', 'zvg', 'chebi', 'chembl', 'unii', 'source_url'))
})


test_that("wd integration test", {
  d <- wd_ident(get_wdid('Glyphosate', 'en', TRUE))
  f <- wd_ident(get_wdid('xxxxx', 'en', TRUE))

  expect_equal(d$cas, "1071-83-6")
  expect_equal(length(d), 13)
  expect_equal(f, NA)
})