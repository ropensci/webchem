test_that("InChI", {
  res <- pc_sect(1983, "InChI", parser = "table")
  expect_false(length(res$Result) == 1 && is.na(res$Result))
  expect_equal(
    res$Result,
    "InChI=1S/C8H9NO2/c1-6(10)9-7-2-4-8(11)5-3-7/h2-5,11H,1H3,(H,9,10)"
  )
})

test_that("Molecular Formula", {
  res <- pc_sect(1983, "Molecular Formula", parser = "table")
  expect_false(length(res$Result) == 1 && is.na(res$Result))
})

test_that("CAS", {
  res <- pc_sect(1983, "CAS", parser = "table")
  expect_false(length(res$Result) == 1 && is.na(res$Result))
})

test_that("MeSH Entry Terms", {
  res <- pc_sect(1983, "MeSH Entry Terms", parser = "table")
  expect_false(length(res$Result) == 1 && is.na(res$Result))
  expect_true(length(res$Result) > 1)
})

test_that("Depositor-Supplied Synonyms", {
  res <- pc_sect(1983, "Depositor-Supplied Synonyms", parser = "table")
  expect_false(length(res$Result) == 1 && is.na(res$Result))
  expect_true(length(res$Result) > 10)
})

test_that("Molecular Weight", {
  res <- pc_sect(1983, "Molecular Weight", parser = "table")
  expect_false(length(res$Result) == 1 && is.na(res$Result))
  expect_equal(res$Result, "151.16 g/mol")
})

test_that("XLogP3", {
  res <- pc_sect(1983, "XLogP3", parser = "table")
  expect_false(length(res$Result) == 1 && is.na(res$Result))
  expect_equal(res$Result, "0.5")
})

test_that("Hydrogen Bond Donor Count", {
  res <- pc_sect(1983, "Hydrogen Bond Donor Count", parser = "table")
  expect_false(length(res$Result) == 1 && is.na(res$Result))
  expect_equal(res$Result, "2")
})

test_that("Hydrogen Bond Acceptor Count", {
  res <- pc_sect(1983, "Hydrogen Bond Acceptor Count", parser = "table")
  expect_false(length(res$Result) == 1 && is.na(res$Result))
  expect_equal(res$Result, "2")
})

test_that("Rotatable Bond Count", {
  res <- pc_sect(1983, "Rotatable Bond Count", parser = "table")
  expect_false(length(res$Result) == 1 && is.na(res$Result))
  expect_equal(res$Result, "1")
})

test_that("Exact Mass", {
  res <- pc_sect(1983, "Exact Mass", parser = "table")
  expect_false(length(res$Result) == 1 && is.na(res$Result))
})

test_that("Monoisotopic Mass", {
  res <- pc_sect(1983, "Monoisotopic Mass", parser = "table")
  expect_false(length(res$Result) == 1 && is.na(res$Result))
})

test_that("Topological Polar Surface Area", {
  res <- pc_sect(1983, "Topological Polar Surface Area", parser = "table")
  expect_false(length(res$Result) == 1 && is.na(res$Result))
})

test_that("Heavy Atom Count", {
  res <- pc_sect(1983, "Heavy Atom Count", parser = "table")
  expect_false(length(res$Result) == 1 && is.na(res$Result))
  expect_equal(res$Result, "11")
})

test_that("Formal Charge", {
  res <- pc_sect(1983, "Formal Charge", parser = "table")
  expect_false(length(res$Result) == 1 && is.na(res$Result))
  expect_equal(res$Result, "0")
})

test_that("Complexity", {
  res <- pc_sect(1983, "Complexity", parser = "table")
  expect_false(length(res$Result) == 1 && is.na(res$Result))
  expect_equal(res$Result, "139")
})

test_that("Isotope Atom Count", {
  res <- pc_sect(1983, "Isotope Atom Count", parser = "table")
  expect_false(length(res$Result) == 1 && is.na(res$Result))
  expect_equal(res$Result, "0")
})

test_that("Defined Atom Stereocenter Count", {
  res <- pc_sect(1983, "Defined Atom Stereocenter Count", parser = "table")
  expect_false(length(res$Result) == 1 && is.na(res$Result))
  expect_equal(res$Result, "0")
})

test_that("Undefined Atom Stereocenter Count", {
  res <- pc_sect(1983, "Undefined Atom Stereocenter Count", parser = "table")
  expect_false(length(res$Result) == 1 && is.na(res$Result))
  expect_equal(res$Result, "0")
})

test_that("Defined Bond Stereocenter Count", {
  res <- pc_sect(1983, "Defined Bond Stereocenter Count", parser = "table")
  expect_false(length(res$Result) == 1 && is.na(res$Result))
  expect_equal(res$Result, "0")
})

test_that("Undefined Bond Stereocenter Count", {
  res <- pc_sect(1983, "Undefined Bond Stereocenter Count", parser = "table")
  expect_false(length(res$Result) == 1 && is.na(res$Result))
  expect_equal(res$Result, "0")
})

test_that("Covalently-Bonded Unit Count", {
  res <- pc_sect(1983, "Covalently-Bonded Unit Count", parser = "table")
  expect_false(length(res$Result) == 1 && is.na(res$Result))
  expect_equal(res$Result, "1")
})

test_that("Compound Is Canonicalized", {
  res <- pc_sect(1983, "Compound Is Canonicalized", parser = "table")
  expect_false(length(res$Result) == 1 && is.na(res$Result))
  expect_equal(res$Result, "Yes")
})

test_that("Physical Description", {
  res <- pc_sect(1983, "Physical Description", parser = "table")
  expect_false(length(res$Result) == 1 && is.na(res$Result))
})

test_that("Odor", {
  res <- pc_sect(1983, "Odor", parser = "table")
  expect_false(length(res$Result) == 1 && is.na(res$Result))
})

test_that("Taste", {
  res <- pc_sect(1983, "Taste", parser = "table")
  expect_false(length(res$Result) == 1 && is.na(res$Result))
})

test_that("Boiling Point", {
  res <- pc_sect(1983, "Boiling Point", parser = "table")
  expect_false(length(res$Result) == 1 && is.na(res$Result))
  expect_true(all(grepl("^>", res$Result)))
})

test_that("Melting Point", {
  res <- pc_sect(1983, "Melting Point", parser = "table")
  expect_false(length(res$Result) == 1 && is.na(res$Result))
})

test_that("Solubility", {
  res <- pc_sect(1983, "Solubility", parser = "table")
  expect_false(length(res$Result) == 1 && is.na(res$Result))
})

test_that("Density", {
  res <- pc_sect(1983, "Density", parser = "table")
  expect_false(length(res$Result) == 1 && is.na(res$Result))
})

test_that("Vapor Density", {
  res <- pc_sect(1983, "Vapor Density", parser = "table")
  expect_false(length(res$Result) == 1 && is.na(res$Result))
})

test_that("Vapor Pressure", {
  res <- pc_sect(1983, "Vapor Pressure", parser = "table")
  expect_false(length(res$Result) == 1 && is.na(res$Result))
})

test_that("LogP", {
  res <- pc_sect(1983, "LogP", parser = "table")
  expect_false(length(res$Result) == 1 && is.na(res$Result))
  expect_equal(res$Result, "0.5")
})

test_that("Autoignition Temperature", {
  res <- pc_sect(1983, "Autoignition Temperature", parser = "table")
  expect_false(length(res$Result) == 1 && is.na(res$Result))
})

test_that("pH", {
  res <- pc_sect(1983, "pH", parser = "table")
  expect_false(length(res$Result) == 1 && is.na(res$Result))
})

test_that("Dissociation Constants", {
  res <- pc_sect(1983, "Dissociation Constants", parser = "table")
  expect_false(length(res$Result) == 1 && is.na(res$Result))
})

test_that("Collision Cross Section", {
  res <- pc_sect(1983, "Collision Cross Section", parser = "table")
  expect_false(length(res$Result) == 1 && is.na(res$Result))
})