up <- ping_service("cs_web")
test_that("examples in the article are unchanged", {
  expect_false(is.inchikey("BQJCRHHNABKAKU-KBQPJGBKS-AN"))
  # The default value for verbose has changed and it now requires verbose = TRUE
  # to be added to the call to return the same output as the article example.
  expect_equal(capture_messages(is.inchikey("BQJCRHHNABKAKU-KBQPJGBKS-AN",
                                            verbose = TRUE)),
               "Hyphens not at position 15 and 26.\n")
  expect_false(is.cas('64-17-6'))
  # The default value for verbose has changed and it now requires verbose = TRUE
  # to be added to the call to return the same output as the article example.
  expect_equal(
    capture_messages(is.cas("64-17-6", verbose = TRUE)),
    "64-17-6: Checksum is not correct! 5 vs. 6\n")
  skip_if_not(up, "ChemSpider service is down, skipping tests")
  expect_false(is.inchikey("BQJCRHHNABKAKU-KBQPJGBKSA-5", type = "chemspider"))
})

test_that("is.cas() returns correct results", {

  expect_true(is.cas('64-17-5'))
  expect_false(is.cas('64175'))
  expect_false(is.cas('4-17-5'))
  expect_false(is.cas('64-177-6'))
  expect_false(is.cas('64-17-55'))
  expect_false(is.cas(" 64-17-5"))
  expect_false(is.cas("64-17-5 "))
})

test_that("as.cas() handles properly formatted CAS",{
  expect_equal(as.cas("64-17-5"), "64-17-5", ignore_attr=TRUE)
  expect_silent(as.cas("64-17-5"))
})

test_that("is.inchikey() returns correct results", {

  expect_true(is.inchikey('BQJCRHHNABKAKU-KBQPJGBKSA-N'))
  expect_false(is.inchikey('BQJCRHHNABKAKU-KBQPJGBKSA'))
  expect_false(is.inchikey('BQJCRHHNABKAKU-KBQPJGBKSA-5'))
  expect_false(is.inchikey('BQJCRHHNABKAKU-KBQPJGBKSA-n'))
  expect_false(is.inchikey('BQJCRHHNABKAKU/KBQPJGBKSA/N'))
  expect_false(is.inchikey('BQJCRHHNABKAKU-KBQPJGBKXA-N'))
  expect_false(is.inchikey('BQJCRHHNABKAKU-KBQPJGBKSB-N'))
  expect_error(is.inchikey(c('BQJCRHHNABKAKU-KBQPJGBKSA-N',
                             'BQJCRHHNABKAKU-KBQPJGBKSA-N')))

  skip_on_cran()
  skip_on_ci()
  skip_if_not(up, "ChemSpider service is down, skipping tests")

  g <- is.inchikey('BQJCRHHNABKAKU-KBQPJGBKSA-N', type = 'chemspider')
  b <- is.inchikey('BQJCRHHNABKAKU-KBQPJGBKSA', type = 'chemspider')

  expect_true(g)
  expect_false(b)

  expect_error(is.inchikey(c('BQJCRHHNABKAKU-KBQPJGBKSA',
                             'BQJCRHHNABKAKU-KBQPJGBKSA-N'),
                           type = 'chemspider'))
  expect_error(is.inchikey_cs(c('BQJCRHHNABKAKU-KBQPJGBKSA',
                                'BQJCRHHNABKAKU-KBQPJGBKSA-N')))
  expect_error(is.inchikey_format(c('BQJCRHHNABKAKU-KBQPJGBKSA',
                                    'BQJCRHHNABKAKU-KBQPJGBKSA-N')))
  expect_equal(length(g), 1)
})


test_that("is.smiles() returns correct results", {
  skip_if_not_installed("rcdk")
  expect_true(is.smiles('Clc1ccc(cc1)C(c2ccc(Cl)cc2)C(Cl)(Cl)Cl'))
  expect_false(suppressWarnings(
    is.smiles('Clc1ccc(cc1)C(c2ccc(Cl)cc2)C(Cl)(Cl)ClWWX')))
  expect_error(is.smiles(c('Clc1ccc(cc1)C(c2ccc(Cl)cc2)C(Cl)(Cl)Cl',
                           'Clc1ccc(cc1)C(c2ccc(Cl)cc2)C(Cl)(Cl)Cl')))
})

test_that("extr_num() returns correct results", {

  expect_equal(extr_num("Melting Pt : -44.6 deg C"), -44.6)
  expect_equal(extr_num("Melting Pt : 44.6 deg C"), 44.6)
  expect_equal(extr_num("Melting Pt : 446 deg C"), 446)
})

test_that("as.cas() returns correct reults", {

  expect_equal(as.cas(58082), "58-08-2", ignore_attr = TRUE)
  expect_equal(as.cas(123456789), NA,ignore_attr = TRUE)
  expect_equal(as.cas(c(761659, 123456789, "hexenol")),
                   c("761-65-9", NA, NA), ignore_attr = TRUE)
})

test_that("parse_mol()", {

  vcr::use_cassette("parse_mol()",{
    A <- cs_compinfo(2265, field = "Mol3D")
    B <- cs_compinfo(2265, field = "Mol2D")
    C <- cs_convert("BGEBZHIAGXMEMV-UHFFFAOYAX", "inchikey", "mol")
  })

  a <- parse_mol(A$mol3D)
  b <- parse_mol(B$mol2D)
  c <- parse_mol(C)

  # issue #294
  res <- POST("https://www.ebi.ac.uk/chembl/api/utils/smiles2ctab",
              body = "CC(O)=O",
              httr::user_agent(webchem:::webchem_url())
  )
  D <- rawToChar(res$content)
  d <- parse_mol(D)

  expect_type(a, "list")
  expect_type(a$eh, "character")
  expect_type(a$cl, "character")
  expect_s3_class(a$ab, "data.frame")
  expect_s3_class(a$bb, "data.frame")
  expect_type(b, "list")
  expect_type(c, "list")
  expect_type(d, "list")
})

test_that("write_mol()", {
  expect_error(write_mol(123), regex = "x is not a character string")
  expect_error(write_mol("hello world"), regex = "x is not a Mol string")

  # test a file output

})

test_that("matcher() warns when 'best' is used with chemical names", {
  expect_warning(
    matcher(x = c("formalin", "carbon monoxide"),
          query = "WSFSSNUMVMOOMR-UHFFFAOYSA-N",
          from = "inchikey",
          result = c("WSFSSNUMVMOOMR-UHFFFAOYSA-N","UGFAIRIUMAVXCW-UHFFFAOYSA-N "),
          match = "best"),
    "match = 'best' only makes sense for chemical name queries.")
})
