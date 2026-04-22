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

test_that("fully implemented resources work", {
  full <- c(
    "atc_class",
    "binding_site",
    "cell_line",
    "compound_record",
    "drug_indication",
    "drug_warning",
    "go_slim"
  )

  for (i in full) {
    queries <- chembl_example_query(i)
    # single query
    queries[1] |>
      chembl_compare_service(resource = i) |>
      suppressWarnings() |>
      expect_true()
    # multiple queries, if available
    queries |>
      chembl_compare_service(resource = i) |>
      suppressWarnings() |>
      expect_true()
  }
})

test_that("partially implemented resources work", {
  partial = c(
    "biotherapeutic",
    "document"
  )

  for (i in partial) {
    ids <- chembl_example_query(i)
    ws <- chembl_query(ids, resource = i, mode = "ws")
    off <- chembl_query(ids, resource = i, mode = "offline")
    for (j in seq_along(ids)) {
      if (i == "biotherapeutic" & j == 3) next()
      if (i == "document") {
        # Remove fields not available in offline mode
        index <- which(names(ws[[j]]) %in% c("doi_chembl", "journal_full_title"))
        ws[[j]] <- ws[[j]][-index]
      }
      expect_true(all.equal(ws[[j]], off[[j]]))
    }
  }
})
