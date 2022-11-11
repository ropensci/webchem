
library(robotstxt)
up <- ping_service("nist")
test_that("NIST webbook is still OK with being scraped", {
  skip_on_cran()
  skip_if_not(up, "NIST Web Book is down")
  expect_true(
    paths_allowed("https://webbook.nist.gov/cgi/cbook.cgi",
                  user_agent = 'webchem (https://github.com/ropensci/webchem)')
    )
})

test_that("nist_ri() works when only one row of data", {
  skip_on_cran()
  skip_if_not(up, "NIST Web Book is down")

  testdf <- nist_ri("78-70-6", from = "cas",
                    type = "alkane",
                    polarity = "non-polar",
                    temp_prog = "custom")
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
  skip_if_not(up, "NIST Web Book is down")

  out <- nist_ri("78-70-6", type = "linear", polarity="non-polar", temp_prog = "custom")
  expect_s3_class(out, "data.frame")
  expect_true(!anyNA(out$RI))
})

test_that("nist_ri() works with inchikey query", {
  skip_on_cran()
  skip_if_not(up, "NIST Web Book is down")

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
  skip_on_cran()
  skip_if_not(up, "NIST Web Book is down")

  testdf <- nist_ri(
    "1S/C8H14O/c1-7(2)5-4-6-8(3)9/h5H,4,6H2,1-3H3",
    from = "inchi",
    type = "kovats",
    polarity = "non-polar",
    temp_prog = "isothermal"
  )
  expect_s3_class(testdf, "data.frame")
  expect_true(!anyNA(testdf$RI))
})

test_that("nist_ri() works with name query", {
  skip_on_cran()
  skip_if_not(up, "NIST Web Book is down")

  testdf <- nist_ri(
    "myrcene",
    from = "name",
    type = "kovats",
    polarity = "non-polar",
    temp_prog = "isothermal"
  )
  expect_s3_class(testdf, "data.frame")
  expect_true(!anyNA(testdf$RI))
})

test_that("nist_ri() works with multiple queries", {
  skip_on_cran()
  skip_if_not(up, "NIST Web Book is down")

  myRIs <-
    nist_ri(
      c("78-70-6", "13474-59-4"),
      from = "cas",
      type = "linear",
      polarity = "non-polar",
      temp_prog = "ramp"
    )
  expect_equal(unique(myRIs$query), c("78-70-6", "13474-59-4"),
               ignore_attr = TRUE)
})

test_that("nist_ri() can return multiple types", {
  skip_on_cran()
  skip_if_not(up, "NIST Web Book is down")

  out <- nist_ri("78-70-6", type = c("kovats","linear"), polarity="non-polar",
                 temp_prog = "custom")
  expect_s3_class(out, "data.frame")
  expect_true(!anyNA(out$RI))
  expect_true(all(c("kovats", "linear") %in% out$type))
  expect_true(all(out$polarity == "non-polar"))
  expect_true(all(out$temp_prog == "custom"))
})


test_that("cas =  is deprecated gently", {
  skip_on_cran()
  skip_if_not(up, "NIST Web Book is down")

  expect_warning(
    nist_ri(cas = "78-70-6"),
    "`cas` is deprecated.  Using `query` instead with `from = 'cas'`."
  )
})


test_that("nist_ri works with NAs", {
  skip_on_cran()
  skip_if_not(up, "NIST Web Book is down")

  test <- nist_ri("107-86-8",
                  from = "cas",
                  type = "linear",
                  polarity = "non-polar",
                  temp_prog = "ramp")
  natest <- nist_ri(c(NA, "107-86-8"),
                    from = "cas",
                    type = "linear",
                    polarity = "non-polar",
                    temp_prog = "ramp")
  expect_identical(
    colnames(natest),
    colnames(test)
  )
  expect_equal(unique(natest$query), c(NA, "107-86-8"), ignore_attr = TRUE)
})

test_that("a name returns CAS", {
  test1 <- nist_ri("α-methyl-Benzenepropanamine", from = "name", type="kovats")
  expect_equal(test1[[1,"cas"]], "22374-89-6")
  test2 <- nist_ri("α-methyl-Benzenepropanamine", from = "name", type="lee")
  expect_equal(test2$query, "α-methyl-Benzenepropanamine")
  expect_equal(test2$cas, "22374-89-6")
})

test_that("a record with no CAS number returns expected output", {
  test <- nist_ri("(Z,Z)-alpha-Farnesene", from="name", type = "linear", polarity="polar")
  expect_equal(test$query, "(Z,Z)-α-Farnesene")
  expect_equal(test$cas, "U293031")
})

test_that("nist_ri() can deal appropriately with a mixture of queries", {
  test <- nist_ri(query=c(NA, "baloon", "methane", "hexanol"), from = "name",
                  type=c("kovats","linear"), polarity="polar",
                  temp_prog = "ramp", verbose=TRUE)

  expect_true(all(is.na(test[1,])))
  expect_true(is.na(test[[2,"cas"]]))
  expect_true(is.na(test[[2,"RI"]]))

  expect_equal(test[[3,"query"]], "methane")
  expect_equal(test[[3,"cas"]], "74-82-8")
  expect_true(is.na(test[[3,"RI"]]))

  expect_equal(test[[4,"query"]], "hexanol")
  expect_equal(test[[4,"cas"]], "111-27-3")
  expect_equal(test[[4,"RI"]], 1339)
})
