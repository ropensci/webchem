up <- ping_service("aw")
test_that("alanwood, commonname", {
  skip_on_cran()
  skip_if_not(up, "Alanwood service is down")

  comps <- c("Fluazinam", "S-Metolachlor", "balloon", NA)
  o1 <- aw_query(comps, type = "commonname")

  expect_type(o1, "list")
  expect_equal(length(o1), 4)
  expect_type(o1[[1]], "list")
  expect_type(o1[[2]], "list")
  expect_equal(o1[[3]], NA)
  expect_equal(o1[[4]], NA)
  expect_equal(o1[["Fluazinam"]]$cas, "79622-59-6")
  expect_equal(length(o1[["S-Metolachlor"]]$inchikey), 2)
  expect_equal(length(o1[["S-Metolachlor"]]$inchi), 2)
  expect_equal(length(o1[["Fluazinam"]]), 11)
})


test_that("alanwood, cas", {
  skip_on_cran()
  skip_if_not(up, "Alanwood service is down")

  comps <- c("79622-59-6", "87392-12-9", "balloon", NA)
  o1 <- aw_query(comps, type = "cas")

  expect_type(o1, "list")
  expect_equal(length(o1), 4)
  expect_type(o1[[1]], "list")
  expect_type(o1[[2]], "list")
  expect_equal(o1[[3]], NA)
  expect_equal(o1[[4]], NA)
  expect_equal(o1[[1]]$cas, "79622-59-6")
  expect_equal(length(o1[[2]]$inchikey), 2)
  expect_equal(length(o1[[2]]$inchi), 2)
  expect_equal(length(o1[[1]]), 11)
  expect_true(is.na(aw_query("12071-83-9", type = "cas")[[1]]$inchi))
})

test_that("alanwood, build_index", {
  skip_on_cran()
  skip_if_not(up, "Alanwood service is down")

  idx <- suppressWarnings(build_aw_idx(verbose = FALSE, force_build = TRUE))
  expect_s3_class(idx, "data.frame")
  expect_equal(ncol(idx), 4)
  expect_equal(names(idx), c("names", "links", "linknames", "source"))
  expect_equal(unique(idx$source), c("rn", "cn"))
  expect_equal(idx$names[1], "50-00-0")
})
