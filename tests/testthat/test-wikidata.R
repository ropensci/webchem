up <- ping_service("wd")
test_that("get_wdid returns correct results", {
  skip_on_cran()
  skip_if_not(up, "Wikidata service is down")
  # test general
  comps <- c('DDT', 'Aspirin', 'xdewrwdcadsr4w', 'acetic acid')
  o1 <- get_wdid(comps, match = 'best')
  o2 <- get_wdid(comps, match = 'all')
  o3 <- get_wdid(comps[1], match = 'first')
  o4 <- get_wdid(comps[1], match = 'na')


  expect_s3_class(o1, 'data.frame')
  expect_s3_class(o2, 'data.frame')
  expect_s3_class(o3, 'data.frame')
  expect_s3_class(o4, 'data.frame')

  expect_equal(o1$wdid, c("Q163648", "Q20987744", NA, 'Q47512'),
               ignore_attr = TRUE)
  expect_equal(o2$wdid[1:2], c("Q163648", "Q949424"), ignore_attr = TRUE)
})

test_that("get_wdid() handles NAs", {
  skip_on_cran()
  skip_if_not(up, "Wikidata service is down")

  expect_s3_class(get_wdid(NA), "data.frame")
  expect_s3_class(get_wdid(c("Triclosan", "Glyphosate", NA)), "data.frame")
})

test_that("wd_ident returns correct results", {
  skip_on_cran()
  skip_if_not(up, "Wikidata service is down")

  id <- c( "Q163648", "Q18216", "asndalsr", NA)
  o1 <- wd_ident(id)
  expect_s3_class(o1, 'data.frame')
  expect_equal(nrow(o1), 4)
  expect_true(is.na(o1$smiles[3]))
  expect_true(is.na(o1$smiles[4]))
  expect_equal(o1$cas[1], '50-29-3')
  expect_equal(names(o1), c('smiles', 'cas', 'cid', 'einecs', 'csid', 'inchi',
                            'inchikey', 'drugbank', 'zvg', 'chebi', 'chembl',
                            'unii', 'lipidmaps', 'swisslipids', 'source_url',
                            'query'))
})

test_that("wd_ident returns correct results for two lipids", {
  skip_on_cran()
  skip_if_not(up, "Wikidata service is down")

  id <- c( "Q27089367", "Q410888")
  o1 <- wd_ident(id)
  expect_s3_class(o1, 'data.frame')
  expect_equal(nrow(o1), 2)
  expect_equal(names(o1), c('smiles', 'cas', 'cid', 'einecs', 'csid', 'inchi',
                            'inchikey', 'drugbank', 'zvg', 'chebi', 'chembl',
                            'unii', 'lipidmaps', 'swisslipids', 'source_url',
                            'query'))
  expect_equal(o1$swisslipids[1], 'SLM:000000510')
  expect_equal(o1$lipidmaps[2], 'LMPR0102010003')
})


test_that("wd integration test", {
  skip_on_cran()
  skip_if_not(up, "Wikidata service is down")

  d <- wd_ident(get_wdid('hexane', language = 'en', match = 'best')$wdid)
  f <- wd_ident(get_wdid('xxxxxxxAX', language = 'en', match = 'best')$wdid)

  expect_equal(d$cas, "110-54-3")
  expect_equal(ncol(d), 16)
  expect_s3_class(d, 'data.frame')
  expect_true(all(is.na(f[1, ])))
})
