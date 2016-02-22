context("alanwood")


test_that("alanwood, commonname", {
  comps <- c('Fluazinam', "S-Metolachlor", "xxxxx")
  o1 <- aw_query(comps, type = 'commonname')

  expect_is(o1, 'list')
  expect_equal(length(o1), 3)
  expect_true(is.na(o1[[3]]))
  expect_equal(o1[['Fluazinam']]$cas, "79622-59-6")
  expect_equal(length(o1[["S-Metolachlor"]]$inchikey), 2)
  expect_equal(length(o1[["S-Metolachlor"]]$inchi), 2)
  expect_equal(length(o1[['Fluazinam']]), 11)
})


test_that("alanwood, cas", {
  comps <- c("79622-59-6", "87392-12-9", "xxxxx")
  o1 <- aw_query(comps, type = 'cas')

  expect_is(o1, 'list')
  expect_equal(length(o1), 3)
  expect_true(is.na(o1[[3]]))
  expect_equal(o1[[1]]$cas, "79622-59-6")
  expect_equal(length(o1[[2]]$inchikey), 2)
  expect_equal(length(o1[[2]]$inchi), 2)
  expect_equal(length(o1[[1]]), 11)
  expect_true(is.na(aw_query('12071-83-9', type = 'cas')[[1]]$inchi))
})

test_that("alanwood, build_index", {
  idx <- build_aw_idx()
  expect_is(idx, 'data.frame')
  expect_equal(ncol(idx), 4)
  expect_equal(names(idx), c("names", "links", "linknames", "source"))
  expect_equal(unique(idx$source), c("rn", "cn"))
  expect_equal(idx$names[1], '50-00-0')
})

test_that("alanwood index is up to date", {
  expect_true(Sys.Date() - attr(aw_idx, 'date') < 30)
})