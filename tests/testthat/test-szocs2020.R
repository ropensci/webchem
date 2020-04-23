context("Szocs et al. 2020")

# unable to check:
# get_etoxid() match arguments can be "best", "all", "first", "ask", "na".
# figure 2 is unchanged
# figure 3 is unchanged

test_that("webchem functions and data sets in the article exist", {
  expect_true(exists("cs_compinfo"))
  expect_true(exists("get_csid"))
  expect_true(exists("jagst"))
  expect_true(exists("lc50"))
  expect_true(exists("get_etoxid"))
  expect_true(exists("etox_basic"))
  expect_true(exists("cas"))
  expect_true(exists("get_cid"))
  expect_true(exists("pc_prop"))
  expect_true(exists("smiles"))
  expect_true(exists("cs_convert"))
  expect_true(exists("aw_query"))
  expect_true(exists("get_chebiid"))
  expect_true(exists("chebi_comp_entity"))
  expect_true(exists("etox_targets"))
  expect_true(exists("pan_query"))
  expect_true(exists("is.inchikey"))
  expect_true(exists("is.cas"))
})

test_that("first six rows of jagst are unchanged", {
  data("jagst", package = "webchem")

  expect_is(jagst, "data.frame")
  expect_equal(names(jagst), c("date", "substance", "value", "qual"))
  expect_equal(head(jagst$date), as.Date(c("2013-01-04", "2013-01-29",
                                           "2013-02-26", "2013-03-26",
                                           "2013-04-23", "2013-05-22")))
  expect_equal(head(jagst$substance), rep("2,4-Dimethylphenol", times = 6))
  expect_equal(head(jagst$value), rep(0.006, times = 6))
  expect_equal(head(jagst$qual), rep("<", times = 6))
})

test_that("first six rows of lc50 are unchanged", {
  data("lc50", package = "webchem")

  expect_is(lc50, "data.frame")
  expect_equal(names(lc50), c("cas", "value"))
  expect_equal(head(lc50$cas), c("50-29-3", "52-68-6", "55-38-9", "56-23-5",
                                 "56-38-2", "57-74-9"))
  expect_equal(head(lc50$value), c(12.415277, 1.282980, 12.168138, 35000.000000,
                                   1.539119, 98.400000), tolerance = 0.001)
})

test_that("etoxid table is unchanged", {
  skip_on_cran()

  subs <<- unique(jagst$substance)
  ids <<- get_etoxid(subs, match = "best")
  ids_short <<- head(ids)

  expect_is(ids_short, "data.frame")
  expect_equal(names(ids_short), c("etoxid", "match", "distance", "query"))
  expect_equal(ids_short$etoxid, c("8668", "8494", NA_character_, "8397",
                                   "7240", "7331"))
  expect_equal(ids_short$match, c("2,4-Xylenol ( 8668 )",
                                  "4-Chlor-2-methylphenol ( 8494 )", NA,
                                  "Atrazin ( 8397 )", "Benzol ( 7240 )",
                                  "Desethylatrazin ( 7331 )"))
  expect_equal(ids_short$distance, c(0, 0, NA, 0, 0, 0))
  expect_equal(ids_short$query, c("2,4-Dimethylphenol",
                                  "4-Chlor-2-methylphenol",
                                  "4-para-nonylphenol", "Atrazin", "Benzol",
                                  "Desethylatrazin"))
})

test_that("etox cas numbers are unchanged", {
  skip_on_cran()

  etox_data <- etox_basic(ids_short$etoxid)
  etox_cas <<- cas(etox_data)

  expect_is(etox_cas, "character")
  expect_equal(names(etox_cas), c("8668", "8494", NA_character_, "8397", "7240",
                                  "7331"))
  expect_equal(unname(etox_cas), c("105-67-9", "1570-64-5", NA_character_,
                                   "1912-24-9", "71-43-2", "6190-65-4"))

})

test_that("compound table is unchanged", {
  skip_on_cran()

  subs <- head(subs)
  cids <- get_cid(etox_cas)
  pc_data <- pc_prop(cids, properties = "CanonicalSMILES")
  pc_smiles <- smiles(pc_data)
  csids <- get_csid(pc_smiles, from = "smiles")
  cs_inchikey <- cs_convert(csids$csid, from = "csid", to = "inchikey")
  res <- data.frame(name = subs, cas = etox_cas, smiles = pc_smiles,
                    cid = pc_data$CID, inchikey = cs_inchikey,
                    csid = csids$csid, stringsAsFactors = FALSE,
                    row.names = NULL)

  expect_is(res, "data.frame")
  expect_equal(names(res),
               c("name", "cas", "smiles", "cid", "inchikey", "csid"))
  expect_equal(res$name, c("2,4-Dimethylphenol", "4-Chlor-2-methylphenol",
                           "4-para-nonylphenol", "Atrazin", "Benzol",
                           "Desethylatrazin"))
  expect_equal(res$cas, c("105-67-9", "1570-64-5", NA_character_,
                          "1912-24-9", "71-43-2", "6190-65-4"))
  expect_equal(res$smiles, c("CC1=CC(=C(C=C1)O)C", "CC1=C(C=CC(=C1)Cl)O",
                             NA_character_, "CCNC1=NC(=NC(=N1)Cl)NC(C)C",
                             "C1=CC=CC=C1", "CC(C)NC1=NC(=NC(=N1)N)Cl"))
  expect_equal(res$cid, c(7771, 14855, NA, 2256, 241, 22563))
  expect_equal(res$inchikey, c("KUFFULVDNCHOFZ-UHFFFAOYAC",
                               "RHPUJHQBPORFGV-UHFFFAOYAB", NA_character_,
                               "MXWJVTOOROXGIU-UHFFFAOYAJ",
                               "UHOVQNZJYSORNB-UHFFFAOYAH",
                               "DFWFIQKMSFGDCQ-UHFFFAOYAI"))
  expect_equal(res$csid, c(13839123, 14165, NA, 2169, 236, 21157))
})

test_that("alanwood example is unchanged", {
  skip_on_cran()

  aw_data <- aw_query(lc50$cas[1:3], type = "cas")
  igroup <- sapply(aw_data, function(y) y$subactivity[1])

  expect_is(igroup, "character")
  expect_equal(names(igroup), c("50-29-3", "52-68-6", "55-38-9"))
  expect_equal(unname(igroup), c("organochlorine insecticides",
                                 "phosphonate insecticides",
                                 "phenyl organothiophosphate insecticides"))

})

test_that("chebi example is unchanged", {
  skip_on_cran()

  cas_rns <- lc50[order(lc50$value)[1:3], "cas"]
  chebiids <- get_chebiid(cas_rns)
  comp <- chebi_comp_entity(chebiids$chebiid)
  pars <- lapply(comp, function(x) {
    with(x, parents[parents$type == "has role", ])
  })

  expect_equal(cas_rns, c("563-12-2", "96182-53-5", "3383-96-8"))
  expect_equal(chebiids$chebiid, c("CHEBI:38663", "CHEBI:38951", "CHEBI:38954"))
  expect_equal(chebiids$chebiasciiname, c("ethion", "tebupirimfos", "temephos"))
  expect_equal(pars$`CHEBI:38663`$chebiName, c("insecticide",
  "environmental contaminant", "EC 3.1.1.7 (acetylcholinesterase) inhibitor",
  "acaricide", "agrochemical"))
  expect_equal(pars$`CHEBI:38951`$chebiName,
               "EC 3.1.1.7 (acetylcholinesterase) inhibitor")
  expect_equal(pars$`CHEBI:38954`$chebiName,
  c("EC 3.1.1.7 (acetylcholinesterase) inhibitor", "acaricide", "agrochemical",
    "ectoparasiticide"))
})

test_that("regulatory example is unchanged", {
  skip_on_cran()

  eqs <- suppressWarnings(etox_targets(ids$etoxid))
  ids$mac <- suppressWarnings(sapply(eqs, function(y) {
    if (length(y) == 1 && is.na(y)) {
      return(NA)
    } else {
      res <- y$res
      min(res[res$Country_or_Region == "EEC / EU" &
                res$Designation == "MAC-EQS", "Value_Target_LR"])
    }
  }))
  mac <- with(ids, ids[!is.na(mac) & is.finite(mac),c("etoxid", "query", "mac")])
  mac_short <- head(mac)

  expect_is(mac_short, "data.frame")
  expect_equal(names(mac_short), c("etoxid", "query", "mac"))
  expect_equal(mac_short$etoxid, c("8397", "7240", "8836", "7442", "7571",
                                   "8756"))
  expect_equal(mac_short$query, c("Atrazin", "Benzol", "Irgarol", "Isoproturon",
                                  "Simazin", "Terbutryn"))
  expect_equal(mac_short$mac, c(2.000, 50.000, 0.016, 1.000, 4.000, 0.034),
               tolerance = 10^-4)

  jagst_eqs <- merge(jagst, mac, by.x = "substance", by.y = "query")
  jagst_short <- head(jagst_eqs)

  expect_is(jagst_short, "data.frame")
  expect_equal(names(jagst_short), c("substance", "date", "value", "qual",
                                     "etoxid", "mac"))
  expect_equal(jagst_short$substance, rep("Atrazin", times = 6))
  expect_equal(jagst_short$date, as.Date(c("2013-09-10", "2013-10-08",
                                           "2013-03-26", "2013-04-23",
                                           "2013-06-18", "2013-07-16")))
  expect_equal(jagst_short$qual, rep("=", times = 6))
  expect_equal(jagst_short$etoxid, rep("8397", times = 6))
  expect_equal(jagst_short$mac, rep(2, times = 6))
})

test_that("utility examples work", {
  a <- is.inchikey("BQJCRHHNABKAKU-KBQPJGBKS-AN")
  a2 <- capture_messages(is.inchikey("BQJCRHHNABKAKU-KBQPJGBKS-AN"))

  expect_false(a)
  expect_equal(a2, "Hyphens not at position 15 and 26.\n")

  b <- is.cas("64-17-6")
  b2 <- capture_messages(is.cas("64-17-6"))

  expect_false(b)
  expect_equal(b2, "Checksum is not correct! 5 vs. 6\n")

  c <- is.inchikey("BQJCRHHNABKAKU-KBQPJGBKSA-5", type = "chemspider")

  expect_false(c)
})
