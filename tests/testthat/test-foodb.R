# Skip on remote servers to avoid database downloads
skip_on_cran()
skip_on_ci()

# Download database if not already downloaded
db_download_foodb(verbose = TRUE)

test_that("database ids are unique", {
  con <- connect_foodb()
  compounds <- dplyr::tbl(con, "Compound") |>
    dplyr::select(c(
      "id",
      "public_id",
      "name"
    )) |>
    dplyr::collect()
  DBI::dbDisconnect(con)

  for (i in names(compounds)) {
    ids <- compounds[[i]][which(!is.na(compounds[[i]]))]
    testthat::expect_equal(length(ids), length(unique(ids)))
  }
})

test_that("foodb_list_compounds() works", {
  harmonised <- foodb_list_compounds(idtype = "name", include_synonyms = FALSE)
  syns <- foodb_list_compounds(idtype = "name", include_synonyms = TRUE)

  expect_equal(length(harmonised), 70477)
  expect_equal(length(syns), 233129)
  expect_true(all(harmonised %in% syns))
})

test_that("foodb_convert() works", {
  # Single query
  qA <- foodb_convert("4", from = "id", to = "public_id")
  qB <- foodb_convert("FDB000004", from = "public_id", to = "name")
  qC <- foodb_convert("Biotin", from = "name", to = "cas_number")
  qD <- foodb_convert("64-17-5", from = "cas_number", to = "moldb_smiles")
  qE <- foodb_convert("[Fe]", from = "moldb_smiles", to = "moldb_inchi")
  qF <- foodb_convert(
    "InChI=1S/C4H7NO3/c1-3(6)5-2-4(7)8/h2H2,1H3,(H,5,6)(H,7,8)",
    from = "moldb_inchi",
    to = "moldb_inchikey"
  )
  qG <- foodb_convert(
    "OKJIRPAQVSHGFK-UHFFFAOYSA-N", from = "moldb_inchikey", to = "moldb_iupac"
  )
  qH <- foodb_convert("2-acetamidoacetic acid", from = "moldb_iupac", to = "id")
  # Multiple queries
  qI <- foodb_convert(c("xxx", "Biotin", "Folic acid"), from = "name", to = "id")

  expect_equal(qA$public_id, "FDB000004")
  expect_equal(qB$name, "Cyanidin 3-(6''-acetyl-galactoside)")
  expect_equal(qC$cas_number, "58-85-5")
  expect_equal(qD$moldb_smiles[1], "CCO")
  expect_equal(qD$moldb_smiles[2], "C\\C=C(/C)COC1OC(CO)C(O)C(O)C1O")
  expect_equal(qE$moldb_inchi, "InChI=1S/Fe")
  expect_equal(qF$moldb_inchikey, "OKJIRPAQVSHGFK-UHFFFAOYSA-N")
  expect_equal(qG$moldb_iupac, "2-acetamidoacetic acid")
  expect_equal(qI$id, c(NA_integer_, 14513, 14507))
})

test_that("foodb_query() works", {
  # single query
  qA <- foodb_query("Biotin")$Biotin
  qB <- foodb_query("3,7-Dimethylquercetin")$`3,7-Dimethylquerceti`
  qC <- foodb_query("Trimethylamine")$Trimethylamine

  # Atomic fields that are not NA
  expect_true(class(qA$id) == "integer" && !is.na(qA$id))
  expect_true(class(qA$public_id) == "character" && !is.na(qA$public_id))
  expect_true(class(qA$name) == "character" && !is.na(qA$name))
  expect_true(class(qA$state) == "character" && !is.na(qA$state))
  expect_true(class(qA$annotation_quality) == "character" && !is.na(qA$annotation_quality))
  expect_true(class(qA$description) == "character" && !is.na(qA$description))
  expect_true(class(qA$cas_number) == "character" && !is.na(qA$cas_number))
  expect_true(class(qA$moldb_smiles) == "character" && !is.na(qA$moldb_smiles))
  expect_true(class(qA$moldb_inchi) == "character" && !is.na(qA$moldb_inchi))
  expect_true(class(qA$moldb_mono_mass) == "character" && !is.na(qA$moldb_mono_mass))
  expect_true(class(qA$moldb_inchikey) == "character" && !is.na(qA$moldb_inchikey))
  expect_true(class(qA$moldb_iupac) == "character" && !is.na(qA$moldb_iupac))
  expect_true(class(qA$synonyms) == "character")
  expect_equal(length(qA$synonyms), 69L)

  # Atomic fields that are NA
  expect_true(class(qA$kingdom) == "character" && is.na(qA$kingdom))
  expect_true(class(qA$superklass) == "character" && is.na(qA$superklass))
  expect_true(class(qA$klass) == "character" && is.na(qA$klass))
  expect_true(class(qA$subklass) == "character" && is.na(qA$subklass))
  expect_true(class(qA$external_descriptors) == "character" && is.na(qA$external_descriptors))

  # Data frame fields that are not NA
  expect_true(inherits(qA$enzymes, "data.frame"))
  expect_equal(nrow(qA$enzymes), 6L)
  expect_equal(ncol(qA$enzymes), 5L)
  expect_true(inherits(qA$health_effect, "data.frame"))
  expect_equal(nrow(qA$health_effect), 4L)
  expect_equal(ncol(qA$health_effect), 8L)
  expect_true(inherits(qA$pathway, "data.frame"))
  expect_equal(nrow(qA$pathway), 1L)
  expect_equal(ncol(qA$pathway), 4L)
  expect_true(inherits(qA$content, "data.frame"))
  expect_equal(nrow(qA$content), 3036L)
  expect_equal(ncol(qA$content), 16L)

  # Data frame fields that are NA
  expect_true(inherits(qA$ontology_terms, "data.frame"))
  expect_equal(nrow(qA$ontology_terms), 1L)
  expect_equal(ncol(qA$ontology_terms), 8L)
  expect_equal(qA$ontology_terms$id, NA_integer_)
  expect_true(inherits(qA$flavor, "data.frame"))
  expect_equal(nrow(qA$flavor), 1L)
  expect_equal(ncol(qA$flavor), 6L)
  expect_equal(qA$ontology_terms$id, NA_integer_)

  # Test fields that are NA in qA
  expect_true(class(qB$kingdom) == "character" && !is.na(qB$kingdom))
  expect_true(class(qB$superklass) == "character" && !is.na(qB$superklass))
  expect_true(class(qB$klass) == "character" && !is.na(qB$klass))
  expect_true(class(qB$subklass) == "character" && !is.na(qB$subklass))
  expect_true(
    class(qB$external_descriptors) == "character" &&
    length(qB$external_descriptors) == 4
  )
  expect_equal(nrow(qB$ontology_terms), 13L)
  expect_equal(nrow(qC$flavor), 6L)

  # Multiple queries
  comps <- c("Biotin", "3,7-Dimethylquercetin","Trimethylamine")
  qC <- foodb_query(comps)
  expect_equal(class(qC), "list")
  expect_equal(names(qC), comps)
})
