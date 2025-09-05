up <- ping_service("chembl")

test_that("chembl_dir_url()", {
  # latest versions
  expect_equal(chembl_dir_url(), "https://ftp.ebi.ac.uk/pub/databases/chembl/ChEMBLdb/releases/chembl_35")
  expect_equal(chembl_dir_url("latest"), "https://ftp.ebi.ac.uk/pub/databases/chembl/ChEMBLdb/releases/chembl_35")
  expect_equal(chembl_dir_url("35"), "https://ftp.ebi.ac.uk/pub/databases/chembl/ChEMBLdb/releases/chembl_35")
  # previous versions
  expect_equal(chembl_dir_url("34"), "https://ftp.ebi.ac.uk/pub/databases/chembl/ChEMBLdb/releases/chembl_34")
  expect_equal(chembl_dir_url("24.1"), "https://ftp.ebi.ac.uk/pub/databases/chembl/ChEMBLdb/releases/chembl_24_1")
  # archived versions
  expect_equal(chembl_dir_url("24"), "https://ftp.ebi.ac.uk/pub/databases/chembl/ChEMBLdb/releases/chembl_24/archived")
  expect_equal(chembl_dir_url("22"), "https://ftp.ebi.ac.uk/pub/databases/chembl/ChEMBLdb/releases/chembl_22/archived")
  # invalid versions
  expect_error(chembl_dir_url("22.5"))
  expect_error(chembl_dir_url("19"))
  expect_error(chembl_dir_url(c("34", "35")))
  expect_error(chembl_dir_url("taxi"))
})

test_that("chembl_files()", {
  # latest versions
  o1 <- chembl_files()
  o2 <- chembl_files("latest")
  o3 <- chembl_files("35")
  # previous versions
  for (i in 20:34) {
    out <- i |> as.character() |> chembl_files() |> suppressWarnings()
    expect_true(all(out$url_exists[-1]))
  }
  o4 <- chembl_files("24.1")
  o5 <- suppressWarnings(chembl_files("22.1"))
  o5m <- capture_warnings(chembl_files("22.1"))
  # archived versions
  o6 <- chembl_files("24")
  o7 <- suppressWarnings(chembl_files("22"))
  o7m <- capture_warnings(chembl_files("22"))

  expect_true(all(o1$url_exists))
  expect_true(all(o2$url_exists))
  expect_true(all(o3$url_exists))
  expect_true(all(o4$url_exists))
  expect_true(all(o5$url_exists[-1]))
  expect_equal(o7m, "Checksum file not found. Data integrity cannot be checked.")
  expect_true(all(o6$url_exists))
  expect_true(all(o7$url_exists[-1]))
  expect_equal(o7m, "Checksum file not found. Data integrity cannot be checked.")
})

test_that("chembl_query() examples", {
  skip_on_cran()
  skip_if_not(up, "ChEMBL service is down")

  #examples
  # Resource: "activity" - requires activity ID
  o1 <- chembl_query("31863", resource = "activity")
  # Resource: "assay" - requires assay ChEMBL ID
  o2 <- chembl_query("CHEMBL615117", resource = "assay")
  # Resource: "atc_class" - requires ATC class ID
  o3 <- chembl_query("A01AA01", resource = "atc_class")
  # Resource: binding_site - requires site ID
  o4 <- chembl_query(2, resource = "binding_site")
  # Resource: biotherapeutic - requires ChEMBL ID
  o5 <- chembl_query("CHEMBL448105", resource = "biotherapeutic")
  # Resource: cell_line - requires ChEMBL ID
  o6 <- chembl_query("CHEMBL3307241", resource = "cell_line")
  # Resource: chembl_id_lookup - requires ChEMBL ID
  o7 <- chembl_query("CHEMBL1", resource = "chembl_id_lookup")
  # Resource: compound_record - requires record ID
  o8 <- chembl_query("1", resource = "compound_record")
  # Resource: compound_structural_alert - requires compound structural alert ID
  o9 <- chembl_query("79048021", resource = "compound_structural_alert")
  # Resource: document - requires document ChEMBL ID
  o10 <- chembl_query("CHEMBL1158643", resource = "document")
  # Resource: document_similarity - requires document 1 ChEMBL ID
  o11 <- chembl_query("CHEMBL1148466", resource = "document_similarity")
  # Resource: drug - requires ChEMBL ID
  o12 <- chembl_query("CHEMBL2", resource = "drug")
  # Resource: drug_indication - requires drug indication ID
  o13 <- chembl_query("22606", resource = "drug_indication")
  # Resource: drug_warning - requires warning ID
  o14 <- chembl_query("1", resource = "drug_warning")
  # Resource: go_slim - requires GO ID
  o15 <- chembl_query("GO:0000003", resource = "go_slim")
  # Resource: mechanism - requires mechanism ID
  o16 <- chembl_query("13", resource = "mechanism")
  # Resource: metabolism - requires metabolism ID
  o17 <- chembl_query("119", resource = "metabolism")
  # Resource: molecule - requires ChEMBL ID
  o18 <- chembl_query("CHEMBL1082", resource = "molecule")
  o19 <- chembl_query(c("CHEMBL25", "CHEMBL1082"), resource = "molecule")
  # Resource: molecule_form - requires ChEMBL ID
  o20 <- chembl_query("CHEMBL6329", resource = "molecule_form")
  # Resource: organism - requires organism class ID (not taxid)
  o21 <- chembl_query("1", resource = "organism")
  # Resource: protein_classification - requires protein class ID
  o22 <- chembl_query("1", resource = "protein_classification")
  # Resource: similarity - requires SMILES
  o23 <- chembl_query("CC(=O)Oc1ccccc1C(=O)O/70", resource = "similarity")
  # Resource: source - requires source ID
  o24 <- chembl_query("1", resource = "source")
  # Resource: substructure - requires SMILES
  o25 <- chembl_query("CN(CCCN)c1cccc2ccccc12", resource = "substructure")
  # Resource: target - requires target ChEMBL ID
  o26 <- chembl_query("CHEMBL2074", resource = "target")
  # Resource: target_component - requires target component ID
  o27 <- chembl_query("1", resource = "target_component")
  # Resource: target_relation - requires target ChEMBL ID
  o28 <- chembl_query("CHEMBL2251", resource = "target_relation")
  # Resource: tissue - requires tissue ChEMBL ID
  o29 <- chembl_query("CHEMBL3988026", resource = "tissue")
  # Resource: xref_source - requires the name of the resource
  o30 <- chembl_query("AlphaFoldDB", resource = "xref_source")

  # verbose message
  o18m <- capture_messages(
    chembl_query("CHEMBL1082", resource = "molecule", verbose = TRUE))

  expect_true(inherits(o1, "list") & length(o1[[1]]) == 46)
  expect_true(inherits(o2, "list") & length(o2[[1]]) == 29)
  expect_true(inherits(o3, "list") & length(o3[[1]]) == 10)
  expect_true(inherits(o4, "list") & length(o4[[1]]) == 3)
  expect_true(inherits(o5, "list") & length(o5[[1]]) == 4)
  expect_true(inherits(o6, "list") & length(o6[[1]]) == 11)
  expect_true(inherits(o7, "list") & length(o7[[1]]) == 5)
  expect_true(inherits(o8, "list") & length(o8[[1]]) == 6)
  expect_true(inherits(o9, "list") & length(o9[[1]]) == 3)
  expect_true(inherits(o10, "list") & length(o10[[1]]) == 19)
  expect_true(inherits(o11, "list") & length(o11[[1]]) == 4)
  expect_true(inherits(o12, "list") & length(o12[[1]]) == 31)
  expect_true(inherits(o13, "list") & length(o13[[1]]) == 9)
  expect_true(inherits(o14, "list") & length(o14[[1]]) == 12)
  expect_true(inherits(o15, "list") & length(o15[[1]]) == 6)
  expect_true(inherits(o16, "list") & length(o16[[1]]) == 17)
  expect_true(inherits(o17, "list") & length(o17[[1]]) == 15)
  expect_true(inherits(o18, "list") & length(o18[[1]]) == 36)
  expect_true(inherits(o19, "list") & length(o19[[1]]) == 36)
  expect_true(inherits(o20, "list") & length(o20[[1]]) == 2)
  expect_true(inherits(o21, "list") & length(o21[[1]]) == 5)
  expect_true(inherits(o22, "list") & length(o22[[1]]) == 9)
  expect_true(inherits(o23, "list") & length(o23[[1]]) == 2)
  expect_true(inherits(o24, "list") & length(o24[[1]]) == 5)
  expect_true(inherits(o25, "list") & length(o25[[1]]) == 2)
  expect_true(inherits(o26, "list") & length(o26[[1]]) == 8)
  expect_true(inherits(o27, "list") & length(o27[[1]]) == 12)
  expect_true(inherits(o28, "list") & length(o28[[1]]) == 3)
  expect_true(inherits(o29, "list") & length(o29[[1]]) == 6)
  expect_true(inherits(o30, "list") & length(o30[[1]]) == 4)

  expect_equal(o18m[2], "OK (HTTP 200).")
})

test_that("More chembl_query()", {
  skip_on_cran()
  skip_if_not(up, "ChEMBL service is down")

  #invalid inputs
  o5 <- chembl_query(c("CHEMBL1082", NA, "pumpkin", "CHEMBL25"))
  o5m <- capture_messages(
    chembl_query(c("CHEMBL1082", NA, "pumpkin", "CHEMBL25"), verbose = TRUE)
  )

  expect_equal(length(o5), 4)
  expect_equal(o5m[4], capture_messages(webchem_message("na"))[1])
  expect_equal(o5m[6], "Query must be a ChEMBL ID. Returning NA.\n")

  #caching
  o6 <- chembl_query("CHEMBL1082", resource = "molecule", cache_file = "test")
  o7m <- capture_messages(chembl_query("CHEMBL1082", resource = "molecule",
                                       cache_file = "test", verbose = TRUE)
  )
  o8 <- chembl_query(NA, resource = "molecule", cache_file = "test")

  expect_equal(o7m[1], "Querying CHEMBL1082. ")
  expect_equal(o7m[2], "Already retrieved.\n")
  expect_equal(o8[[1]], NA)

  if (file.exists("./cache/test.rds")) file.remove("./cache/test.rds")
  if (dir.exists("cache")) unlink("cache", recursive = TRUE)

  #messages
  o9 <- capture_messages(
    chembl_query("CHEMBL12345678", resource = "molecule", verbose = TRUE))

  expect_equal(o9[2], "Not Found (HTTP 404).")

  #service down
  o10 <- chembl_query("CHEMBL1082", test_service_down = TRUE)
  o10m <- capture_messages(
    chembl_query("CHEMBL1082", test_service_down = TRUE, verbose = TRUE))

  expect_equal(o10[[1]], NA)
  expect_equal(o10m[2], "Service not available. Returning NA.")
})

test_that("chembl_atc_classes()", {
  o1 <- chembl_atc_classes()
  o2 <- capture_messages(chembl_atc_classes(verbose = TRUE))

  expect_s3_class(o1, c("tbl_df", "tbl", "data.frame"))
  expect_equal(o2[1], "Querying Page 1. ")
  expect_equal(o2[2], "OK (HTTP 200).")

  #service down

  o3 <- chembl_atc_classes(test_service_down = TRUE)
  o3m <- capture_messages(
    chembl_atc_classes(test_service_down = TRUE, verbose = TRUE))

  expect_equal(o3, NA)
  expect_equal(o3m[2], "Service not available. Returning NA.")
})

test_that("validate_chembl_version()", {
  expect_equal(validate_chembl_version(), "35")
  expect_equal(validate_chembl_version("latest"), "35")
  expect_equal(validate_chembl_version("34"), "34")
  expect_equal(validate_chembl_version("24.1"), "24_1")
  expect_error(validate_chembl_version("19"))
  expect_error(validate_chembl_version(c("34", "35")))
  expect_error(validate_chembl_version(NA))
  expect_error(validate_chembl_version("thirtyfour"))
})
