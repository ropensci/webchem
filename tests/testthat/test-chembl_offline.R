# Skip on remote servers to avoid database downloads
skip_on_cran()
skip_on_ci()

# Download ChEMBL database if not already downloaded
db_download_chembl(version = "35", verbose = FALSE)

test_that("chembl_offline_chembl_id_lookup works", {
  a <- chembl_query_offline(
    query = "CHEMBL1",
    resource = "chembl_id_lookup",
    output = "tidy"
  )
  b <- chembl_query_offline(
    query = c("CHEMBL1", "CHEMBL1082"),
    resource = "chembl_id_lookup",
    output = "tidy"
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

implemented <- c(
  "binding_site",
  "biotherapeutic",
  "cell_line",
  "compound_record",
  "document"
)

for (i in implemented) {
  ids <- chembl_example_query(i)
  ws <- chembl_query(ids, resource = i)
  off <- chembl_query(ids, resource = i, mode = "offline")

  for (j in seq_along(ids)) {

    if (i == "biotherapeutic" & j == 3) next()

    if (i == "document") {
      # Remove fields not available in offline mode
      index <- which(names(ws[[j]]) %in% c("doi_chembl", "journal_full_title"))
      ws[[j]] <- ws[[j]][-index]
    }

    expect_true(identical(ws[[j]], off[[j]]))
  }
}
