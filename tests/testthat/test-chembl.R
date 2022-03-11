up <- ping_service("chembl")

test_that("chembl_query()", {
  skip_on_cran()
  skip_if_not(up, "ChEMBL service is down")

  #examples
  o1 <- chembl_query("CHEMBL1082", resource = "molecule")
  o2 <- chembl_query(c("CHEMBL25", "CHEMBL1082"), resource = "molecule")
  o3 <- chembl_query("CHEMBL771355", "chembl_id_lookup")
  o4 <- chembl_query("CHEMBL771355", resource = "assay")

  expect_type(o1, "list")

  #invalid inputs
  o5 <- chembl_query("CHEMBL1082", NA, "pumpkin", "CHEMBL25")


})