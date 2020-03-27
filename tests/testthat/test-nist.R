context("nist")

test_that("nist_ri() warns when no results", {
  expect_warning(nist_ri("78-70-6", from = "cas", type = 'linear', polarity = 'non-polar'),
                 regexp = "There are no RIs for 78-70-6")
})

test_that("nist_ri() works when only one row of data", {
  testdf <- nist_ri("78-70-6")
  expect_s3_class(testdf, "data.frame")
  expect_true(!anyNA(testdf$RI))
  testdf2 <-
    nist_ri(
      "HICYDYJTCDBHMZ-QVHKTLOISA-N",
      from = "inchikey",
      type = "alkane",
      polarity = "non-polar",
      temp_prog = "ramp"
    )
  expect_s3_class(testdf2, "data.frame")
  expect_true(!anyNA(testdf2$RI))
})

test_that("nist_ri() returns results", {
  skip_on_cran()
  out <- nist_ri("78-70-6", type = 'linear', temp_prog = 'custom')
  expect_s3_class(out, 'data.frame')
  expect_true(!anyNA(out$RI))
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

test_that("nist_ri() works with name query", {
  testdf <- nist_ri(
    "myrcene",
    from = "name",
    type = "kovats",
    polarity = "non-polar",
    temp_prog = "isothermal"
  )
  expect_s3_class(testdf,"data.frame")
  expect_true(!anyNA(testdf$RI))
})

test_that("nist_ri() works with multiple queries", {
  myRIs <-
    nist_ri(
      c("78-70-6", "13474-59-4"),
      from = "cas",
      type = "linear",
      polarity = "non-polar",
      temp_prog = "ramp"
    )
  expect_equivalent(unique(myRIs$query), c("78-70-6", "13474-59-4"))
})

test_that("nist_ri() warns when multiple results", {
  expect_warning(
    nist_ri("Longipinene", from = "name"),
    "More than one match for 'Longipinene'. Returning NA.")
})

test_that("nist_ri() warns when no chromatography data", {
  expect_warning(
    nist_ri("methane", from = "name"),
    "There are no chromatography data for 'methane'. Returning NA."
  )
})

test_that("cas =  is deprecated gently", {
  expect_warning(nist_ri(cas = "78-70-6"),
                 "`cas` is deprecated.  Using `query` instead with `from = 'cas'`.")
})