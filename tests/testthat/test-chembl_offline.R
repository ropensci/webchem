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

test_that("cell_lines work", {
  ws <- chembl_query(
    query = "CHEMBL3307241", resource = "cell_line", output = "raw")
  off <- chembl_query(
    query = "CHEMBL3307241",
    resource = "cell_line",
    mode = "offline",
    output = "raw"
  )
  res <- compare_service_lists(ws$CHEMBL3307241, off$CHEMBL3307241)
  testthat::expect_equal(res$status, "OK")
})

test_that("atc_class works", {
  off1 <- chembl_query(
    query = "A01AA01",
    resource = "atc_class",
    mode = "offline",
    output = "tidy"
  )
  off2 <- chembl_query(
    query = c("A01AA", "A01AB02"),
    resource = "atc_class",
    mode = "offline",
    output = "tidy"
  )
  off3 <- chembl_query(
    query = c("Q","A01AB02"),
    resource = "atc_class",
    mode = "offline",
    output = "tidy"
  )

  testthat::expect_s3_class(off1, "data.frame")
  testthat::expect_equal(nrow(off2), 7)
  testthat::expect_equal(nrow(off3), 2)
  testthat::expect_true(is.na(off3$level1[1]))

  off <- chembl_query(
    query = "A01AA01", resource = "atc_class", mode = "offline", output = "raw")
  ws <- chembl_query(query = "A01AA01", resource = "atc_class", output = "raw")
  res <- compare_service_lists(ws$A01AA01, off$A01AA01)
  testthat::expect_equal(res$status, "OK")
})

test_that("binding_site works", {
  ws <- chembl_query(query = 2, resource = "binding_site", output = "raw")
  off <- chembl_query(
    query = 2, resource = "binding_site", mode = "offline", output = "raw")
  res <- compare_service_lists(ws$`2`, off$`2`)
  testthat::expect_equal(res$status, "OK")
})

test_that("biotherapeutic works", {
  ids <- chembl_example_query("biotherapeutic")
  ws <- chembl_query(ids, resource = "biotherapeutic")
  off <- chembl_query(ids, resource = "biotherapeutic", mode = "offline")
  res1 <- identical(ws[[1]], off[[1]])
  res2 <- identical(ws[[2]], off[[2]])
  res3 <- identical(ws[[3]], off[[3]])

  expect_true(res1)
  expect_true(res2)
  #expect_trues(res3)
})
