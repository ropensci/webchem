# Skip on remote servers to avoid database downloads
skip_on_cran()
skip_on_ci()

# Download ChEMBL database if not already downloaded
db_download_chembl(version = "35", verbose = FALSE)

test_that("chembl_offline_chembl_id_lookup works", {
  a <- chembl_query_offline(query = "CHEMBL1", resource = "chembl_id_lookup")
  b <- chembl_query_offline(
    query = c("CHEMBL1", "CHEMBL1082"), resource = "chembl_id_lookup"
  )

  expect_s3_class(a, "data.frame")
  expect_equal(nrow(a), 1)
  expect_equal(ncol(a), 5)
  expect_s3_class(b, "data.frame")
  expect_equal(nrow(b), 2)
  expect_equal(ncol(b), 5)
})

test_that("informative error when query and resource do not match", {
  msg <- tryCatch(
    chembl_query_offline(
      query = c("CHEMBL1082", "CHEMBL3988026") , resource = "molecule"),
    error = function(e) e$message
  )
  expect_equal(msg, "CHEMBL3988026 is not a COMPOUND. It is a TISSUE.")
})