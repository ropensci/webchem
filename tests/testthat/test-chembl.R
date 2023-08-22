up <- ping_service("chembl")

test_that("chembl_query()", {
  # skip_on_cran()
  # skip_if_not(up, "ChEMBL service is down")

  #examples
  with_mock_dir("mocks/chembl-query", {
    o1 <- chembl_query("CHEMBL1082", resource = "molecule")
    o1m <- capture_messages(
      chembl_query("CHEMBL1082", resource = "molecule", verbose = TRUE))

    o2 <- chembl_query(c("CHEMBL25", "CHEMBL1082"), resource = "molecule")
    o3 <- chembl_query("CHEMBL771355", "chembl_id_lookup")
    o4 <- chembl_query("CHEMBL771355", resource = "assay")
  })

  expect_type(o1, "list")
  expect_equal(length(o1[[1]]), 35)
  expect_equal(o1m[2], "OK (HTTP 200).")
  expect_equal(length(o2), 2)
  expect_equal(o3[[1]]$entity_type, "ASSAY")
  expect_equal(o4[[1]]$document_chembl_id, "CHEMBL1121620")
})

test_that("chembl_query() handles invalid inputs", {
  with_mock_dir("mocks/chembl-invalid", {
    o5 <- chembl_query(c("CHEMBL1082", NA, "pumpkin", "CHEMBL25"))
    o5m <- capture_messages(
      chembl_query(c("CHEMBL1082", NA, "pumpkin", "CHEMBL25"), verbose = TRUE)
    )
  })

  expect_equal(length(o5), 4)
  expect_equal(o5m[4], capture_messages(webchem_message("na"))[1])
  expect_equal(o5m[6], "Query is not a ChEMBL ID. Returning NA.\n")

})

test_that("caching works with chembl_query()", {
  with_mock_dir("mocks/chembl-cache", {
    o6 <- chembl_query("CHEMBL1082", resource = "molecule", cache_file = "test")
    o7m <- capture_messages(chembl_query("CHEMBL1082", resource = "molecule",
                                         cache_file = "test", verbose = TRUE)
    )
    o8 <- chembl_query(NA, resource = "molecule", cache_file = "test")
  })

  expect_equal(o7m[1], "Querying CHEMBL1082. ")
  expect_equal(o7m[2], "Already retrieved.\n")
  expect_equal(o8[[1]], NA)

  if (file.exists("./cache/test.rds")) file.remove("./cache/test.rds")
  if (dir.exists("cache")) unlink("cache", recursive = TRUE)
})

test_that("chembl_query() handles 404 and service down errors", {
  #messages
  with_mock_dir("mocks/chembl-404", {
    o9 <- capture_messages(
      chembl_query("CHEMBL12345678", resource = "molecule", verbose = TRUE))
  })
  expect_equal(o9[2], "Not Found (HTTP 404).")

  #service down
  without_internet({
    o10 <- chembl_query("CHEMBL1082")
    o10m <- capture_messages(
      chembl_query("CHEMBL1082", verbose = TRUE))
  })

  expect_equal(o10[[1]], NA)
  expect_equal(o10m[2], "Service not available. Returning NA.")
})

test_that("chembl_atc_classes()", {

  with_mock_dir("mocks/chembl-atc", {
    o1 <- chembl_atc_classes()
    o2 <- capture_messages(chembl_atc_classes(verbose = TRUE))
  })

  expect_s3_class(o1, c("tbl_df", "tbl", "data.frame"))
  expect_equal(o2[1], "Querying Page 1. ")
  expect_equal(o2[2], "OK (HTTP 200).")

  #service down
  without_internet({
    o3 <- chembl_atc_classes()
    o3m <- capture_messages(chembl_atc_classes(verbose = TRUE))
  })

  expect_equal(o3, NA)
  expect_equal(o3m[2], "Service not available. Returning NA.")
})
