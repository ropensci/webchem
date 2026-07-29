test_that("InChI", {
  res <- pc_sect(1983, "InChI", parser = "string")
  expect_false(length(res$Result) == 1 && is.na(res$Result))
  expect_equal(
    res$Result,
    "InChI=1S/C8H9NO2/c1-6(10)9-7-2-4-8(11)5-3-7/h2-5,11H,1H3,(H,9,10)"
  )
})

test_that("Molecular Formula", {
  res <- pc_sect(1983, "Molecular Formula", parser = "string")
  expect_false(length(res$Result) == 1 && is.na(res$Result))
})

test_that("CAS", {
  res <- pc_sect(1983, "CAS", parser = "string")
  expect_false(length(res$Result) == 1 && is.na(res$Result))
})

test_that("MeSH Entry Terms", {
  res <- pc_sect(1983, "MeSH Entry Terms", parser = "string")
  expect_false(length(res$Result) == 1 && is.na(res$Result))
  expect_true(length(res$Result) > 1)
})

test_that("Depositor-Supplied Synonyms", {
  res <- pc_sect(1983, "Depositor-Supplied Synonyms", parser = "string")
  expect_false(length(res$Result) == 1 && is.na(res$Result))
  expect_true(length(res$Result) > 10)
})

test_that("Molecular Weight", {
  res <- pc_sect(1983, "Molecular Weight", parser = "string")
  expect_false(length(res$Result) == 1 && is.na(res$Result))
  expect_equal(res$Result, "151.16 g/mol")
})

test_that("XLogP3", {
  res <- pc_sect(1983, "XLogP3", parser = "string")
  expect_false(length(res$Result) == 1 && is.na(res$Result))
  expect_equal(res$Result, "0.5")
})

test_that("Hydrogen Bond Donor Count", {
  res <- pc_sect(1983, "Hydrogen Bond Donor Count", parser = "string")
  expect_false(length(res$Result) == 1 && is.na(res$Result))
  expect_equal(res$Result, "2")
})

test_that("Hydrogen Bond Acceptor Count", {
  res <- pc_sect(1983, "Hydrogen Bond Acceptor Count", parser = "string")
  expect_false(length(res$Result) == 1 && is.na(res$Result))
  expect_equal(res$Result, "2")
})

test_that("Rotatable Bond Count", {
  res <- pc_sect(1983, "Rotatable Bond Count", parser = "string")
  expect_false(length(res$Result) == 1 && is.na(res$Result))
  expect_equal(res$Result, "1")
})

test_that("Exact Mass", {
  res <- pc_sect(1983, "Exact Mass", parser = "string")
  expect_false(length(res$Result) == 1 && is.na(res$Result))
})

test_that("Monoisotopic Mass", {
  res <- pc_sect(1983, "Monoisotopic Mass", parser = "string")
  expect_false(length(res$Result) == 1 && is.na(res$Result))
})

test_that("Topological Polar Surface Area", {
  res <- pc_sect(1983, "Topological Polar Surface Area", parser = "string")
  expect_false(length(res$Result) == 1 && is.na(res$Result))
})

test_that("Heavy Atom Count", {
  res <- pc_sect(1983, "Heavy Atom Count", parser = "string")
  expect_false(length(res$Result) == 1 && is.na(res$Result))
  expect_equal(res$Result, "11")
})

test_that("Formal Charge", {
  res <- pc_sect(1983, "Formal Charge", parser = "string")
  expect_false(length(res$Result) == 1 && is.na(res$Result))
  expect_equal(res$Result, "0")
})

test_that("Complexity", {
  res <- pc_sect(1983, "Complexity", parser = "string")
  expect_false(length(res$Result) == 1 && is.na(res$Result))
  expect_equal(res$Result, "139")
})

test_that("Isotope Atom Count", {
  res <- pc_sect(1983, "Isotope Atom Count", parser = "string")
  expect_false(length(res$Result) == 1 && is.na(res$Result))
  expect_equal(res$Result, "0")
})

test_that("Defined Atom Stereocenter Count", {
  res <- pc_sect(1983, "Defined Atom Stereocenter Count", parser = "string")
  expect_false(length(res$Result) == 1 && is.na(res$Result))
  expect_equal(res$Result, "0")
})

test_that("Undefined Atom Stereocenter Count", {
  res <- pc_sect(1983, "Undefined Atom Stereocenter Count", parser = "string")
  expect_false(length(res$Result) == 1 && is.na(res$Result))
  expect_equal(res$Result, "0")
})

test_that("Defined Bond Stereocenter Count", {
  res <- pc_sect(1983, "Defined Bond Stereocenter Count", parser = "string")
  expect_false(length(res$Result) == 1 && is.na(res$Result))
  expect_equal(res$Result, "0")
})

test_that("Undefined Bond Stereocenter Count", {
  res <- pc_sect(1983, "Undefined Bond Stereocenter Count", parser = "string")
  expect_false(length(res$Result) == 1 && is.na(res$Result))
  expect_equal(res$Result, "0")
})

test_that("Covalently-Bonded Unit Count", {
  res <- pc_sect(1983, "Covalently-Bonded Unit Count", parser = "string")
  expect_false(length(res$Result) == 1 && is.na(res$Result))
  expect_equal(res$Result, "1")
})

test_that("Compound Is Canonicalized", {
  res <- pc_sect(1983, "Compound Is Canonicalized", parser = "string")
  expect_false(length(res$Result) == 1 && is.na(res$Result))
  expect_equal(res$Result, "Yes")
})

test_that("Physical Description", {
  res <- pc_sect(1983, "Physical Description", parser = "string")
  expect_false(length(res$Result) == 1 && is.na(res$Result))
})

test_that("Odor", {
  res <- pc_sect(1983, "Odor", parser = "string")
  expect_false(length(res$Result) == 1 && is.na(res$Result))
})

test_that("Taste", {
  res <- pc_sect(1983, "Taste", parser = "string")
  expect_false(length(res$Result) == 1 && is.na(res$Result))
})

test_that("Boiling Point", {
  res <- pc_sect(1983, "Boiling Point", parser = "string")
  expect_false(length(res$Result) == 1 && is.na(res$Result))
  expect_true(all(grepl("^>", res$Result)))
})

test_that("Melting Point", {
  res <- pc_sect(1983, "Melting Point", parser = "string")
  expect_false(length(res$Result) == 1 && is.na(res$Result))
})

test_that("Solubility", {
  res <- pc_sect(1983, "Solubility", parser = "string")
  expect_false(length(res$Result) == 1 && is.na(res$Result))
})

test_that("Density", {
  res <- pc_sect(1983, "Density", parser = "string")
  expect_false(length(res$Result) == 1 && is.na(res$Result))
})

test_that("Vapor Density", {
  res <- pc_sect(1983, "Vapor Density", parser = "string")
  expect_false(length(res$Result) == 1 && is.na(res$Result))
})

test_that("Vapor Pressure", {
  res <- pc_sect(1983, "Vapor Pressure", parser = "string")
  expect_false(length(res$Result) == 1 && is.na(res$Result))
})

test_that("LogP", {
  res <- pc_sect(1983, "LogP", parser = "string")
  expect_false(length(res$Result) == 1 && is.na(res$Result))
  expect_equal(res$Result, "0.5")
})

test_that("Autoignition Temperature", {
  res <- pc_sect(1983, "Autoignition Temperature", parser = "string")
  expect_false(length(res$Result) == 1 && is.na(res$Result))
})

test_that("pH", {
  res <- pc_sect(1983, "pH", parser = "string")
  expect_false(length(res$Result) == 1 && is.na(res$Result))
})

test_that("Dissociation Constants", {
  res <- pc_sect(1983, "Dissociation Constants", parser = "string")
  expect_false(length(res$Result) == 1 && is.na(res$Result))
})

test_that("Collision Cross Section", {
  res <- pc_sect(1983, "Collision Cross Section", parser = "string")
  expect_false(length(res$Result) == 1 && is.na(res$Result))
})

test_that("1D NMR Spectra", {
  res <- pc_sect(1983, "1D NMR Spectra", parser = "table")
  expect_equal(
    res$`1D NMR Spectra`[1],
    "http://nmrshiftdb.nmr.uni-koeln.de/portal/js_pane/P-Results/nmrshiftdbaction/showDetailsFromHome/molNumber/89614"
  )
})

test_that("1H NMR Spectra", {
  res <- pc_sect(1983, "1H NMR Spectra", parser = "table")
  expect_equal(
    res$`Shifts [ppm]:Intensity`[1],
    "9.63:20.93, 6.67:40.69, 6.65:35.56, 1.97:100.00, 7.31:35.75, 7.33:37.16, 9.11:34.80"
  )
})

test_that("GC-MS", {
  res <- pc_sect(1983, "GC-MS", parser = "table")
  expect_equal(
    res$`Top 5 Peaks`[1],
    "109.0:99.99, 151.0:34.59, 80.0:9.49, 110.0:7.64, 108.0:7.14"
  )
})

test_that("MS-MS", {
  res <- pc_sect(1983, "MS-MS", parser = "table")
  expect_equal(
    res$`Top 5 Peaks`[1],
    "152.349:100, 109.615:81.87, 111.118:68.32, 111.745:66.47, 151.471:56.21"
  )
})

test_that("LC-MS", {
  res <- pc_sect(1983, "LC-MS", parser = "table")
  expect_equal(res$`Collision Energy`[1], "Ramp 20%-70% (nominal)")
})

test_that("Other MS", {
  res <- pc_sect(1983, "Other MS", parser = "table")
  expect_true(nrow(res) > 5)
})

test_that("Raman Spectra", {
  res <- pc_sect(1983, "Raman Spectra", parser = "table")
  expect_true(nrow(res) > 3)
})

test_that("Other Spectra", {
  res <- pc_sect(1983, "Other Spectra", parser = "string")
  expect_false(length(res$Result) == 1 && is.na(res$Result))
  expect_equal(
    res$Result[1],
    "Intense mass spectral peaks: 80 m/z, 109 m/z, 151 m/z"
  )
})
