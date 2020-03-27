context("nist")

test_that("nist_ri() works when only one row of data", {
  expect_silent(nist_ri("78-70-6"))
})

test_that("nist returns correct results", {
  skip_on_cran()

  a <- nist_ri("78-70-6")
  c <- nist_ri("78-70-6", type = 'linear', temp_prog = 'custom')

  expect_s3_class(a, 'data.frame')
  expect_s3_class(c, 'data.frame')

  expect_warning(nist_ri("78-70-6", type = 'linear', polarity = 'non-polar'))
})

test_that("nist_ri() works with inchikey query", {
  testdf <- nist_ri(
    "UHEPJGULSIKKTP-UHFFFAOYSA-N",
    from = "inchikey",
    type = "kovats",
    polarity = "non-polar",
    temp_prog = "isothermal"
  )
  expect_s3_class(testdf, "data.frame")
  expect_true(!anyNA(testdf$RI))
})

test_that("nist_ri() works with inchi query", {
  testdf <- nist_ri(
    "1S/C8H14O/c1-7(2)5-4-6-8(3)9/h5H,4,6H2,1-3H3",
    from = "inchi",
    type = "kovats",
    polarity = "non-polar",
    temp_prog = "isothermal"
  )
  expect_s3_class(testdf,"data.frame")
  expect_true(!anyNA(testdf$RI))
})
