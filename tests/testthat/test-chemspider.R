context("chemspider")
token <- '37bf5e57-9091-42f5-9274-650a64398aaf'

test_that("get_csid()", {
  expect_equal(get_csid("Triclosan", token = token), '5363')
  expect_equal(get_csid("xxxxxxxxx", token = token), NA)
  expect_error(get_csid(c("a", "b"), token = token))
  expect_true(is.vector(get_csid("Triclosan", token = token)))
})

test_that("csid_compinfo()", {
  expect_equal(length(csid_compinfo('5363', token)), 4)
  expect_equal(csid_compinfo('5363', token)['SMILES'],
               c(SMILES = "c1cc(c(cc1Cl)O)Oc2ccc(cc2Cl)Cl"))
  expect_warning(csid_compinfo('aaaa', token))
  expect_error(csid_compinfo(c("a", "b"), token = token))
  })

test_that("csid_extcompinfo()", {
  expect_equal(csid_extcompinfo('5363', token = token)['AverageMass'],
               c(AverageMass = '289.5418'))
  expect_equal(length(csid_extcompinfo('5363', token = token)), 12)
  expect_warning(csid_extcompinfo('aaaa', token))
  expect_error(csid_extcompinfo(c("a", "b"), token = token))
})


test_that("csid_extcompinfo(get_cid())", {
  expect_equal(csid_extcompinfo(get_csid('Triclosan', token = token),
                                token = token)['AverageMass'],
               c(AverageMass = '289.5418'))
  expect_equal(length(csid_extcompinfo(get_csid('Triclosan', token = token),
                                       token = token)), 12)
})