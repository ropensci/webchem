up <- ping_service("bcpc")
test_that("examples in the article are unchanged", {
  skip_on_cran()
  skip_if_not(up, "BCPC pesticide compendium is down")

  utils::data("lc50", package = "webchem")
  expect_warning(
    aw_data <- aw_query(lc50$cas[1:3], from = "cas"),
    "deprecated"
  )
  igroup <- sapply(aw_data, function(y) y$subactivity[1])

  expect_type(igroup, "character")
  expect_equal(names(igroup), c("50-29-3", "52-68-6", "55-38-9"))
  expect_equal(unname(igroup), c("organochlorine insecticides",
                                 "phosphonate insecticides",
                                 "phenyl organothiophosphate insecticides"))
})

test_that("BCPC pesticide compendium, name", {
  skip_on_cran()
  skip_if_not(up, "BCPC pesticide compendium is down")

  comps <- c("Fluazinam", "S-Metolachlor", "balloon", NA)
  o1 <- bcpc_query(comps, from = "name")

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


test_that("BCPC pesticide compendium, invalid input", {
  skip_on_cran()
  skip_if_not(up, "BCPC pesticide compendium is down")

  comps <- c("balloon", NA)
  o1 <- bcpc_query(comps)
  expect_type(o1, "list")
  expect_equal(o1[[1]], NA)
  expect_equal(o1[[2]], NA)
})

test_that("BCPC pesticide compendium, build_index", {
  skip_on_cran()
  skip_if_not(up, "BCPC pesticide compendium is down")

  idx <- suppressWarnings(webchem:::build_bcpc_idx(force_build = TRUE))
  expect_s3_class(idx, "data.frame")
  expect_equal(ncol(idx), 4)
  expect_equal(names(idx), c("names", "links", "linknames", "source"))
  expect_equal(unique(idx$source), c("rn", "cn"))
  expect_equal(idx$names[1], "50-00-0")
})
