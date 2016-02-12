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

})
