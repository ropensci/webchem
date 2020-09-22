up <- ping_service("etox")
test_that("examples in the article are unchanged", {
  skip_on_cran()
  skip_if_not(up, "ETOX service is down")

  data("jagst", package = "webchem")
  subs <- head(unique(jagst$substance))
  ids <- get_etoxid(subs, match = "best")
  etox_data <- etox_basic(ids$etoxid)
  #values go to test-pubchem
  etox_cas <- cas(etox_data)
  eqs <- etox_targets(c("8397", "7240", "8836", "7442", "7571", "8756"))
  macs <- suppressWarnings(sapply(eqs, function(y) {
    if (length(y) == 1 && is.na(y)) {
      return(NA)
    } else {
      res <- y$res
      min(res[res$Country_or_Region == "EEC / EU" &
                res$Designation == "MAC-EQS", "Value_Target_LR"])
    }
  }))

  expect_is(ids, "data.frame")
  expect_equal(names(ids), c("query", "match", "etoxid"))
  expect_equivalent(ids$etoxid,
               c("8668", "8494", NA, "8397", "7240", "7331"))
  expect_equal(
    ids$match,
    c("2,4-Xylenol", "4-Chlor-2-methylphenol", NA,
      "Atrazin", "Benzol", "Desethylatrazin"))
  expect_equal(ids$query,
               c("2,4-Dimethylphenol", "4-Chlor-2-methylphenol",
                 "4-para-nonylphenol", "Atrazin", "Benzol", "Desethylatrazin"))

  expect_is(etox_cas, "character")
  expect_equal(names(etox_cas),
               c("8668", "8494", NA, "8397", "7240", "7331"))
  expect_equal(unname(etox_cas),c("105-67-9", "1570-64-5", NA, "1912-24-9",
                                  "71-43-2", "6190-65-4"))
  expect_equal(unname(macs), c(2.000, 50.000, 0.016, 1.000, 4.000, 0.034),
               tolerance = 10^-4)
})

test_that("get_etoxid returns correct results", {
  skip_on_cran()
  skip_if_not(up, "ETOX service is down")

  # test general
  comps <- c("Triclosan", "Glyphosate")
  o1 <- suppressWarnings(get_etoxid(comps, match = "best"))
  o2 <- suppressWarnings(get_etoxid(comps, match = "all"))
  o3 <- get_etoxid("Triclosan", match = "first")
  o4 <- get_etoxid("Triclosan", match = "na")
  o5 <- get_etoxid("1071-83-6", from = 'cas', match = 'first')
  o6 <- get_etoxid("133483", from = "gsbl")
  o7 <- get_etoxid("203-157-5", from = "ec")
  do2 <- get_etoxid("Thiamethoxam")

  expect_s3_class(o1, "data.frame")
  expect_s3_class(o2, "data.frame")
  expect_s3_class(o3, "data.frame")
  expect_s3_class(o4, "data.frame")
  expect_s3_class(o5, "data.frame")
  expect_s3_class(o6, "data.frame")
  expect_s3_class(o7, "data.frame")
  expect_s3_class(do2, "data.frame")

  expect_equivalent(o1$etoxid, c("20179", "9051"))
  expect_equivalent(o2$etoxid, c("89236", "20179", "9051"))
})

test_that("examples from webchem article run", {
  skip_on_cran()
  skip_if_not(up, "ETOX service is down")

  # tests for the article
  data("jagst")
  ids <- get_etoxid(head(unique(jagst$substance),6), match = "best")

  expect_s3_class(ids, "data.frame")
  expect_equivalent(ids$etoxid, c("8668","8494",NA,"8397","7240","7331"))
  expect_equivalent(ids$match, c(
    "2,4-Xylenol",
    "4-Chlor-2-methylphenol",
    NA,
    "Atrazin",
    "Benzol",
    "Desethylatrazin"
  ))
  expect_equivalent(ids$query, c(
    "2,4-Dimethylphenol",
    "4-Chlor-2-methylphenol",
    "4-para-nonylphenol",
    "Atrazin",
    "Benzol",
    "Desethylatrazin"
  ))

})

test_that("etox_basic returns correct results", {
  skip_on_cran()
  skip_if_not(up, "ETOX service is down")

  ids <- c("20179", "9051", "xxxxx", NA)
  o1 <- etox_basic(ids)

  expect_s3_class(o1, 'list')
  expect_equal(length(o1), 4)
  expect_equal(o1[['20179']]$cas, "3380-34-5")
  expect_equal(length(o1[['20179']]), 5)
  expect_s3_class(o1[['20179']]$synonyms, 'data.frame')
  expect_true(is.na(o1[[3]]))
  expect_true(is.na(o1[[4]]))
})

test_that("etox_targets returns correct results", {
  skip_on_cran()
  skip_if_not(up, "ETOX service is down")

  ids <- c("20179", "9051", "xxxxx", NA)
  o1 <- etox_targets(ids)

  expect_type(o1, 'list')
  expect_equal(length(o1), 4)
  expect_equal(o1[['20179']]$res$Substance[1], "Triclosan")
  expect_equal(ncol(o1[['20179']]$res), 33)
  expect_s3_class(o1[['20179']]$res, 'data.frame')
  expect_true(is.na(o1[[3]]))
  expect_true(is.na(o1[[4]]))
})

test_that("etox_tests returns correct results", {
  skip_on_cran()
  skip_if_not(up, "ETOX service is down")

  ids <- c("20179", "9051", "xxxxx", NA)
  o1 <- etox_tests(ids)

  expect_type(o1, 'list')
  expect_equal(length(o1), 4)
  expect_equal(o1[['20179']]$res$Substance[1], "Triclosan")
  expect_equal(ncol(o1[['20179']]$res), 41)
  expect_s3_class(o1[['20179']]$res, 'data.frame')
  expect_true(is.na(o1[[3]]))
  expect_true(is.na(o1[[4]]))
})


test_that("etox integration tests", {
  skip_on_cran()
  skip_if_not(up, "ETOX service is down")

  comps <- c('Triclosan', 'Glyphosate', 'xxxx')
  ids_b <- get_etoxid(comps, match = 'best')
  ids_a <- get_etoxid(comps, match = 'all')

  int1 <- etox_basic(ids_b$etoxid)
  int2 <- etox_targets(ids_b$etoxid)
  int3 <- etox_tests(ids_b$etoxid)

  expect_type(int1, 'list')
  expect_equal(length(int1), 3)
  expect_equal(int1[['20179']]$cas, "3380-34-5")
  expect_equal(length(int1[['20179']]), 5)
  expect_s3_class(int1[['20179']]$synonyms, 'data.frame')
  expect_true(is.na(int1[[3]]))

  expect_type(int2, 'list')
  expect_equal(length(int2), 3)
  expect_equal(int2[['20179']]$res$Substance[1], "Triclosan")
  expect_equal(ncol(int2[['20179']]$res), 33)
  expect_s3_class(int2[['20179']]$res, 'data.frame')
  expect_true(is.na(int2[[3]]))

  expect_type(int3, 'list')
  expect_equal(length(int3), 3)
  expect_equal(int3[['20179']]$res$Substance[1], "Triclosan")
  expect_equal(ncol(int3[['20179']]$res), 41)
  expect_s3_class(int3[['20179']]$res, 'data.frame')
  expect_true(is.na(int3[[3]]))
})


test_that("etox functions handle NAs", {
  expect_equal(is.na(get_etoxid(NA)$match), TRUE)
})
