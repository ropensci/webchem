up <- ping_service("pc")
test_that("examples in the article are unchanged", {
  skip_on_cran()
  skip_if_not(up, "PubChem service is down")
  #values come from test-etox
  cas <- c("105-67-9", "1570-64-5", NA, "1912-24-9", "71-43-2", "6190-65-4")
  cids <- get_cid(cas, from = "xref/rn", match = "first")
  pc_data <- pc_prop(cids$cid, properties = "CanonicalSMILES")
  #values go to test-chemspider
  pc_smiles <- smiles(pc_data)

  expect_s3_class(pc_data, "data.frame")

  expect_equal(cids$cid, c("7771", "14855", NA, "2256", "241", "22563"))
  expect_equal(pc_smiles, c("CC1=CC(=C(C=C1)O)C", "CC1=C(C=CC(=C1)Cl)O", NA,
                            "CCNC1=NC(=NC(=N1)Cl)NC(C)C", "C1=CC=CC=C1",
                            "CC(C)NC1=NC(=NC(=N1)N)Cl"))
})

test_that("get_cid()", {
  skip_on_cran()
  skip_if_not(up, "PubChem service is down")

  #from name
  expect_true("5564" %in% get_cid("Triclosan")$cid)
  expect_true("5564" %in% get_cid("Triclosan", domain = "substance")$cid)
  #from smiles
  expect_equal(get_cid("CCCC", from = "smiles")$cid, "7843")
  #from inchi
  expect_equal(get_cid("InChI=1S/CH5N/c1-2/h2H2,1H3", from = "inchi")$cid,
               "6329")
  #from inchikey
  expect_equal(get_cid("BPGDAMSIGCZZLK-UHFFFAOYSA-N", from = "inchikey")$cid,
               "12345")
  #from formula, issue 206, some queries first return a listkey.
  expect_true("10864091" %in% get_cid("C26H52NO6P", from = "formula")$cid)
  # from CAS RN
  expect_true("750" %in% get_cid("56-40-6", from = "xref/rn")$cid)
  expect_true("5257127" %in%
                get_cid("56-40-6", from = "xref/rn", domain = "substance")$cid)
  #from cid, similarity
  expect_true("5564" %in% get_cid(5564, from = "similarity/cid")$cid)
  #from smiles, similarity
  expect_true("702" %in% get_cid("CCO", from = "similarity/smiles")$cid)
  #from SID
  expect_equal(get_cid("126534046", from = "sid", domain = "substance")$cid,
               "24971898")
  # sourceid
  expect_true(
    "19689584" %in%
      get_cid("VCC957895", from = "sourceid/23706", domain = "substance")$cid)
  #from aid
  expect_equal(get_cid(170004, from = "aid", domain = "assay")$cid, "68352")
  #from GeneID
  expect_true("11580958" %in%
                get_cid(25086, from = "target/geneid", domain = "assay")$cid)
  #arg
  expect_true(nrow(get_cid("Triclosan", arg = "name_type=word")) > 1)
  #match
  expect_true(nrow(get_cid("Triclosan", arg = "name_type=word",
                           match = "first")) == 1)
  #multiple compounds
  expect_true(nrow(get_cid(c("Triclosan", "Aspirin"))) == 2)
  #invalid input
  expect_true(is.na(get_cid(NA)$cid[1]))
  expect_true(is.na(suppressWarnings(get_cid("xxxx"))$cid[1]))
  expect_equal(capture_messages(get_cid("balloon", verbose = TRUE)),
               c("Querying balloon. ", "Not Found (HTTP 404).", "\n"))
  # sourceall
  opto <- get_cid("Optopharma Ltd", from = "sourceall", domain = "substance")
  expect_equal(min(opto$cid), "102361739")
})

test_that("get_cid() handles special characters in SMILES", {
  skip_on_cran()
  skip_if_not(up, "PubChem service is down")

  expect_equal(get_cid("C#C", from = "smiles")$cid, "6326")
})

test_that("pc_prop", {
  skip_on_cran()
  skip_if_not(up, "PubChem service is down")

  b <- suppressWarnings(pc_prop("xxx", properties = "CanonicalSmiles"))
  c <- pc_prop("5564", properties = c("CanonicalSmiles", "InChiKey"))
  expect_true(is.na(b))
  expect_equal(ncol(c), 3)
})

test_that("pc_synonyms", {
  skip_on_cran()
  skip_if_not(up, "PubChem service is down")
  expect_equal(pc_synonyms(NA), list(NA), ignore_attr = TRUE)
  expect_equal(pc_synonyms("Acetyl Salicylic Acid")[[1]][1], "aspirin")
  expect_equal(length(pc_synonyms(c("Triclosan", "Aspirin"))), 2)
  expect_equal(pc_synonyms("BPGDAMSIGCZZLK-UHFFFAOYSA-N",
                           from = "inchikey")[[1]][1], "Methylene diacetate")
  expect_true(is.na(suppressWarnings(pc_synonyms("xxxx"))[[1]]))
})

test_that("cid integration tests", {
  skip_on_cran()
  skip_if_not(ping_pubchem(), "PubChem service is down")

  expect_equal(pc_prop(get_cid("Triclosan")$cid[1],
                       properties = "CanonicalSmiles")$CanonicalSMILES,
               "C1=CC(=C(C=C1Cl)O)OC2=C(C=C(C=C2)Cl)Cl")
  expect_true(is.na(suppressWarnings(pc_prop(NA,
                                             properties = "CanonicalSmiles"))))
})

test_that("pc_page()", {
  skip_on_cran()
  skip_if_not(up, "PubChem service is down")

  a <- pc_page(c(311, 176, 1118, "balloon", NA), "Dissociation Constants")

  expect_type(a, "list")
  expect_length(a, 5)
  expect_s3_class(a[[1]], c("Node", "R6"))
  expect_s3_class(a[[2]], c("Node", "R6"))
  expect_equal(a[[4]], NA)
  expect_equal(a[[5]], NA)
})

test_that("pc_sect()", {
  skip_on_cran()
  skip_if_not(up, "PubChem service is down")

  a <- pc_sect(c(311, 176, 1118, "balloon", NA), "Dissociation Constants")
  expect_s3_class(a, c("tbl_df", "tbl", "data.frame"))
  expect_equal(mean(c("Citric acid", "Acetic acid", NA) %in% a$Name), 1)
  expect_equal(mean(c("2.79", "4.76 (at 25 °C)", NA) %in% a$Result), 1)
  expect_equal(mean(c("DrugBank", NA) %in% a$SourceName), 1)
  expect_equal(mean(c("DB04272", "DB03166", NA) %in% a$SourceID), 1)
  b <- pc_sect(2231, "depositor-supplied synonyms", "substance")
  expect_s3_class(b, c("tbl_df", "tbl", "data.frame"))
  expect_equal(names(b), c("SID", "Name", "Result", "SourceName", "SourceID"))
  expect_equal(b$Result, c("cholesterol", "57-88-5",
                                "5-cholestene-3beta-ol"), ignore_attr = TRUE)

  c <- pc_sect(780286, "modify date", "assay")
  expect_s3_class(c, c("tbl_df", "tbl", "data.frame"))
  expect_equal(names(c), c("AID", "Name", "Result", "SourceName", "SourceID"))
  expect_equal(c$Result, c("2014-05-03", "2018-09-28", "2020-06-30"))

  d <- pc_sect("1ZHY_A", "Sequence", "protein")
  expect_s3_class(d, c("tbl_df", "tbl", "data.frame"))
  expect_equal(names(d), c("pdbID", "Name", "Result", "SourceName", "SourceID"))
  expect_equal(d$Result[1], ">pdb|1ZHY|A Chain A, KES1 protein (Run BLAST)",
               ignore_attr = TRUE)
 })
