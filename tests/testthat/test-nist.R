context("nist")

a <- nist_ri("78-70-6", from = "cas")
b <- nist_ri("78-70-6", from = "cas", type = 'linear', polarity = 'non-polar')
c <- nist_ri("78-70-6", from = "cas", type = 'linear', temp_prog = 'custom')

test_that("nist returns correct results", {
  skip_on_cran()

  expect_is(a, 'data.frame')
  expect_is(b, 'data.frame')
  expect_is(c, 'data.frame')

  expect_equal(names(a)[1], 'CAS')
  expect_equal(names(b)[1], 'CAS')
  expect_equal(names(c)[1], 'CAS')

  expect_warning(nist_ri("78-70-6", type = 'linear', polarity = 'non-polar'))
})

test_that("can search by name", {
  expect_is(nist_ri("methyl salicylate", from = "name"), "data.frame")
})

test_that("can search by inchi", {
  expect_is(nist_ri("1S/C8H8O3/c1-11-8(10)6-4-2-3-5-7(6)9/h2-5,9H,1H3", from = "inchi"), "data.frame")
})

test_that("can serch by inchikey", {
  expect_is(nist_ri("OSWPMRLSEDHDFF-UHFFFAOYSA-N", from = "inchikey"), "data.frame")
})

test_that("can search by cas", {
  expect_is(nist_ri("78-70-6", from = "cas"), "data.frame")
})