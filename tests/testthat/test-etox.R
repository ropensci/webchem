require(RCurl)
qurl <- 'https://webetox.uba.de/webETOX/public/search/stoff.do'
cont <- try(getURL(qurl, .encoding = 'UTF-8', .opts = list(timeout = 3)),
            silent = TRUE)
down <- inherits(cont, 'try-error')

test_that("get_etoxid returns correct results", {

  skip_on_cran()
  skip_if(down, "ETOX service is down")

  # test general
  comps <- c("Triclosan", "Glyphosate")
  o1 <- suppressWarnings(get_etoxid(comps, match = "best"))
  o2 <- suppressWarnings(get_etoxid(comps, match = "all"))
  o3 <- get_etoxid("Triclosan", match = "first")
  o4 <- get_etoxid("Triclosan", match = "na")
  o5 <- get_etoxid("1071-83-6", from = 'cas', match = 'best')
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

  expect_equal(o1$etoxid, c("20179", "9051"))
  expect_equal(o2$etoxid, c("89236", "20179", "9051"))
})

test_that("examples from webchem article run", {
  skip_on_cran()
  skip_if(down, "ETOX service is down")

  # tests for the article
  data("jagst")
  ids <- get_etoxid(head(unique(jagst$substance),6), match = "best")

  expect_s3_class(ids, "data.frame")
  expect_equal(ids$etoxid, c("8668","8494",NA,"8397","7240","7331"))
  expect_equal(ids$match, c(
    "2,4-Xylenol ( 8668 )",
    "4-Chlor-2-methylphenol ( 8494 )",
    NA,
    "Atrazin ( 8397 )",
    "Benzol ( 7240 )",
    "Desethylatrazin ( 7331 )"
  ))
  expect_equal(ids$query, c(
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
  skip_if(down, "Etox service is down")
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
  skip_if(down, "Etox service is down")

  ids <- c("20179", "9051", "xxxxx", NA)
  o1 <- etox_targets(ids)

  expect_s3_class(o1, 'list')
  expect_equal(length(o1), 4)
  expect_equal(o1[['20179']]$res$Substance[1], "Triclosan")
  expect_equal(ncol(o1[['20179']]$res), 33)
  expect_s3_class(o1[['20179']]$res, 'data.frame')
  expect_true(is.na(o1[[3]]))
  expect_true(is.na(o1[[4]]))
})

test_that("etox_tests returns correct results", {
  skip_on_cran()
  skip_if(down, "Etox service is down")

  ids <- c("20179", "9051", "xxxxx", NA)
  o1 <- etox_tests(ids)

  expect_s3_class(o1, 'list')
  expect_equal(length(o1), 4)
  expect_equal(o1[['20179']]$res$Substance[1], "Triclosan")
  expect_equal(ncol(o1[['20179']]$res), 41)
  expect_s3_class(o1[['20179']]$res, 'data.frame')
  expect_true(is.na(o1[[3]]))
  expect_true(is.na(o1[[4]]))
  expect_
})


test_that("etox integration tests", {
  skip_on_cran()
  skip_if(down, "Etox service is down")

  comps <- c('Triclosan', 'Glyphosate', 'xxxx')
  ids_b <- get_etoxid(comps, match = 'best')
  ids_a <- get_etoxid(comps, match = 'all')

  # etox_*() can handle only vector inputs (so using match = 'all' does not work)
  expect_error(etox_basic(ids_a))
  expect_error(etox_targets(ids_a))
  expect_error(etox_tests(ids_a))


  int1 <- etox_basic(ids_b$etoxid)
  int2 <- etox_targets(ids_b$etoxid)
  int3 <- etox_tests(ids_b$etoxid)

  expect_s3_class(int1, 'list')
  expect_equal(length(int1), 3)
  expect_equal(int1[['20179']]$cas, "3380-34-5")
  expect_equal(length(int1[['20179']]), 5)
  expect_s3_class(int1[['20179']]$synonyms, 'data.frame')
  expect_true(is.na(int1[[3]]))

  expect_s3_class(int2, 'list')
  expect_equal(length(int2), 3)
  expect_equal(int2[['20179']]$res$Substance[1], "Triclosan")
  expect_equal(ncol(int2[['20179']]$res), 33)
  expect_is(int2[['20179']]$res, 'data.frame')
  expect_true(is.na(int2[[3]]))

  expect_s3_class(int3, 'list')
  expect_equal(length(int3), 3)
  expect_equal(int3[['20179']]$res$Substance[1], "Triclosan")
  expect_equal(ncol(int3[['20179']]$res), 41)
  expect_s3_class(int3[['20179']]$res, 'data.frame')
  expect_true(is.na(int3[[3]]))
})