up <- ping_service("bcpc")
test_that("examples in the article are unchanged as far as it can be reasonably expected", {
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

  # After fixing issue #342, only the subactivity for the third element
  # is returned unchanged, because 1 (DDT) and 2 (trichloron) previously
  # returned only one subactivity (organochlorine insecticides and phosphonate
  # insecticides), where in fact BCPC lists two activities with corresponding
  # subactivities
  expect_equal(unname(igroup)[3], "phenyl organothiophosphate insecticides")

  # The subactivities for DDT and trichloron originally returned can still be
  # checked, as they are returned as the second element of the subactivities
  expect_equal(aw_data[[1]]$subactivity[2], "organochlorine insecticides")
  expect_equal(aw_data[[2]]$subactivity[2], "phosphonate insecticides")
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

test_that("BCPC pesticide compendium, activity", {
  skip_on_cran()
  skip_if_not(up, "BCPC pesticide compendium is down")

  comps <- c("atrazine", "2,4-D", "Copper hydroxide", "ziram")
  o1 <- bcpc_query(comps)
  expect_equal(o1[[1]]$activity, "herbicides")
  expect_equal(o1[[1]]$subactivity, "chlorotriazine herbicides")
  expect_equal(o1[[2]]$activity, c("herbicides", "plant growth regulators"))
  expect_equal(o1[[2]]$subactivity, c("phenoxyacetic herbicides", "auxins"))
  expect_equal(o1[[3]]$activity, c("bactericides", "fungicides"))
  expect_equal(o1[[3]]$subactivity, c(NA, "copper fungicides"))
  expect_equal(o1[[4]]$activity, c("bird repellents", "fungicides", "mammal repellents"))
  expect_equal(o1[[4]]$subactivity, c(NA, "dithiocarbamate fungicides; zinc fungicides", NA))
})
