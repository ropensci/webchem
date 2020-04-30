context("etox")

test_that("examples in the article are unchanged", {
  skip_on_cran()

  data("jagst", package = "webchem")
  subs <- head(unique(jagst$substance))
  ids <- get_etoxid(subs, match = "best")
  etox_data <- etox_basic(ids$etoxid)
  #values go to test-pubchem
  etox_cas <- cas(etox_data)
  eqs <- etox_targets(c("8397", "7240", "8836", "7442", "7571", "8756"))
  # might not work.
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
  expect_equal(names(ids), c("etoxid", "match", "distance", "query"))
  expect_equal(ids$etoxid,
               c("8668", "8494", NA, "8397", "7240", "7331"))
  expect_equal(
    ids$match,
    c("2,4-Xylenol ( 8668 )", "4-Chlor-2-methylphenol ( 8494 )", NA,
      "Atrazin ( 8397 )", "Benzol ( 7240 )", "Desethylatrazin ( 7331 )"))
  expect_equal(ids$distance, c(0, 0, NA, 0, 0, 0))
  expect_equal(ids$query,
               c("2,4-Dimethylphenol", "4-Chlor-2-methylphenol",
                 "4-para-nonylphenol", "Atrazin", "Benzol", "Desethylatrazin"))

  expect_is(etox_cas, "character")
  expect_equal(names(etox_cas),
               c("8668", "8494", NA, "8397", "7240", "7331"))
  expect_equal(unname(etox_cas),c("105-67-9", "1570-64-5", NA, "1912-24-9",
                                  "71-43-2", "6190-65-4"))
  expect_equal(macs, c(2.000, 50.000, 0.016, 1.000, 4.000, 0.034),
               tolerance = 10^-4)
  })

test_that("get_etoxid returns correct results", {
  skip_on_cran()

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

  expect_is(o1, "data.frame")
  expect_is(o2, "data.frame")
  expect_is(o3, "data.frame")
  expect_is(o4, "data.frame")
  expect_is(o5, "data.frame")
  expect_is(o6, "data.frame")
  expect_is(o7, "data.frame")
  expect_is(do2, "data.frame")

  expect_equal(o1$etoxid, c("20179", "9051"))
  expect_equal(o2$etoxid, c("89236", "20179", "9051"))

  # tests for the article
  data("jagst")
  ids <- get_etoxid(head(unique(jagst$substance),6), match = "best")

  expect_is(ids, "data.frame")
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

# test_that("etox_basic returns correct results", {
#   skip_on_cran()
#
#   ids <- c("20179", "9051", "xxxxx", NA)
#   o1 <- etox_basic(ids)
#
#   expect_is(o1, 'list')
#   expect_equal(length(o1), 4)
#   expect_equal(o1[['20179']]$cas, "3380-34-5")
#   expect_equal(length(o1[['20179']]), 5)
#   expect_is(o1[['20179']]$synonyms, 'data.frame')
#   expect_true(is.na(o1[[3]]))
#   expect_true(is.na(o1[[4]]))
#})
#
#
# test_that("etox_targets returns correct results", {
#   skip_on_cran()
#
#   ids <- c("20179", "9051", "xxxxx", NA)
#   o1 <- etox_targets(ids)
#
#   expect_is(o1, 'list')
#   expect_equal(length(o1), 4)
#   expect_equal(o1[['20179']]$res$Substance[1], "Triclosan")
#   expect_equal(ncol(o1[['20179']]$res), 33)
#   expect_is(o1[['20179']]$res, 'data.frame')
#   expect_true(is.na(o1[[3]]))
#   expect_true(is.na(o1[[4]]))
# })

# test_that("etox_tests returns correct results", {
#   skip_on_cran()
#
#   ids <- c("20179", "9051", "xxxxx", NA)
#   o1 <- etox_tests(ids)
#
#   expect_is(o1, 'list')
#   expect_equal(length(o1), 4)
#   expect_equal(o1[['20179']]$res$Substance[1], "Triclosan")
#   expect_equal(ncol(o1[['20179']]$res), 41)
#   expect_is(o1[['20179']]$res, 'data.frame')
#   expect_true(is.na(o1[[3]]))
#   expect_true(is.na(o1[[4]]))
# })
#
#
# test_that("etox integration tests", {
#   skip_on_cran()
#
#   comps <- c('Triclosan', 'Glyphosate', 'xxxx')
#   ids_b <- get_etoxid(comps, match = 'best')
#   ids_a <- get_etoxid(comps, match = 'all')
#
#   # etox_*() can handle only vector inputs (so using match = 'all' does not work)
#   expect_error(etox_basic(ids_a))
#   expect_error(etox_targets(ids_a))
#   expect_error(etox_tests(ids_a))
#
#
#   int1 <- etox_basic(ids_b$etoxid)
#   int2 <- etox_targets(ids_b$etoxid)
#   int3 <- etox_tests(ids_b$etoxid)
#
#   expect_is(int1, 'list')
#   expect_equal(length(int1), 3)
#   expect_equal(int1[['20179']]$cas, "3380-34-5")
#   expect_equal(length(int1[['20179']]), 5)
#   expect_is(int1[['20179']]$synonyms, 'data.frame')
#   expect_true(is.na(int1[[3]]))
#
#   expect_is(int2, 'list')
#   expect_equal(length(int2), 3)
#   expect_equal(int2[['20179']]$res$Substance[1], "Triclosan")
#   expect_equal(ncol(int2[['20179']]$res), 33)
#   expect_is(int2[['20179']]$res, 'data.frame')
#   expect_true(is.na(int2[[3]]))
#
#   expect_is(int3, 'list')
#   expect_equal(length(int3), 3)
#   expect_equal(int3[['20179']]$res$Substance[1], "Triclosan")
#   expect_equal(ncol(int3[['20179']]$res), 41)
#   expect_is(int3[['20179']]$res, 'data.frame')
#   expect_true(is.na(int3[[3]]))
# })