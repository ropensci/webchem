test_that("InChI", {
  res <- pc_sect(1983, "InChI")
  expect_equal(
    res$Result,
    "InChI=1S/C8H9NO2/c1-6(10)9-7-2-4-8(11)5-3-7/h2-5,11H,1H3,(H,9,10)"
  )
})

test_that("Molecular Formula", {
  res <- pc_sect(1983, "Molecular Formula")
  expect_equal(res$Result[1], "C8H9NO2")
})

test_that("CAS", {
  res <- pc_sect(1983, "CAS")
  expect_equal(res$Result[1], "103-90-2")
})

test_that("MeSH Entry Terms", {
  res <- pc_sect(1983, "MeSH Entry Terms", form = "long")
  expect_equal(res$Result[1], "Acetaminophen")
})

test_that("Depositor-Supplied Synonyms", {
  res <- pc_sect(1983, "Depositor-Supplied Synonyms")
  expect_true(length(res$Result) > 10)
  expect_equal(res$Result[2], "Paracetamol")
})

test_that("Molecular Weight", {
  res <- pc_sect(1983, "Molecular Weight")
  expect_equal(res$Result, "151.16 g/mol")
})

test_that("XLogP3", {
  res <- pc_sect(1983, "XLogP3")
  expect_equal(res$Result, "0.5")
})

test_that("Hydrogen Bond Donor Count", {
  res <- pc_sect(1983, "Hydrogen Bond Donor Count")
  expect_equal(res$Result, "2")
})

test_that("Hydrogen Bond Acceptor Count", {
  res <- pc_sect(1983, "Hydrogen Bond Acceptor Count")
  expect_equal(res$Result, "2")
})

test_that("Rotatable Bond Count", {
  res <- pc_sect(1983, "Rotatable Bond Count")
  expect_equal(res$Result, "1")
})

test_that("Exact Mass", {
  res <- pc_sect(1983, "Exact Mass")
  expect_equal(res$Result, "151.063328530 Da")
})

test_that("Monoisotopic Mass", {
  res <- pc_sect(1983, "Monoisotopic Mass")
  expect_equal(res$Result, "151.063328530 Da")
})

test_that("Topological Polar Surface Area", {
  res <- pc_sect(1983, "Topological Polar Surface Area")
  expect_equal(res$Result, "49.3")
})

test_that("Heavy Atom Count", {
  res <- pc_sect(1983, "Heavy Atom Count")
  expect_equal(res$Result, "11")
})

test_that("Formal Charge", {
  res <- pc_sect(1983, "Formal Charge")
  expect_equal(res$Result, "0")
})

test_that("Complexity", {
  res <- pc_sect(1983, "Complexity")
  expect_equal(res$Result, "139")
})

test_that("Isotope Atom Count", {
  res <- pc_sect(1983, "Isotope Atom Count")
  expect_equal(res$Result, "0")
})

test_that("Defined Atom Stereocenter Count", {
  res <- pc_sect(1983, "Defined Atom Stereocenter Count")
  expect_equal(res$Result, "0")
})

test_that("Undefined Atom Stereocenter Count", {
  res <- pc_sect(1983, "Undefined Atom Stereocenter Count")
  expect_equal(res$Result, "0")
})

test_that("Defined Bond Stereocenter Count", {
  res <- pc_sect(1983, "Defined Bond Stereocenter Count")
  expect_equal(res$Result, "0")
})

test_that("Undefined Bond Stereocenter Count", {
  res <- pc_sect(1983, "Undefined Bond Stereocenter Count")
  expect_equal(res$Result, "0")
})

test_that("Covalently-Bonded Unit Count", {
  res <- pc_sect(1983, "Covalently-Bonded Unit Count")
  expect_equal(res$Result, "1")
})

test_that("Compound Is Canonicalized", {
  res <- pc_sect(1983, "Compound Is Canonicalized")
  expect_equal(res$Result, "Yes")
})

test_that("Physical Description", {
  res <- pc_sect(1983, "Physical Description")
  expect_equal(res$Result[3], "Solid")
})

test_that("Color / Form", {
  res <- pc_sect(1983, "color / form")
  expect_equal(res$Result, "Large monoclinic prisms from water")
})

test_that("Odor", {
  res <- pc_sect(1983, "Odor")
  expect_equal(res$Result, "Odorless")
})

test_that("Taste", {
  res <- pc_sect(1983, "Taste")
  expect_equal(res$Result, "Slightly bitter taste")
})

test_that("Boiling Point", {
  res <- pc_sect(1983, "Boiling Point")
  expect_equal(res$Result[1], ">500")
})

test_that("Melting Point", {
  res <- pc_sect(1983, "Melting Point")
  expect_equal(res$Result[2], "168-172")
})

test_that("Solubility", {
  res <- pc_sect(1983, "Solubility")
  expect_equal(res$Result[6], "14 mg/mL at 25 °C")
})

test_that("Density", {
  res <- pc_sect(1983, "Density")
  expect_equal(
    res$Result[1],
    "1.293 at 70 °F (NTP, 1992) - Denser than water; will sink"
  )
})

test_that("Vapor Density", {
  res <- pc_sect(1983, "Vapor Density")
  expect_equal(res$Result, "Relative vapor density  (air = 1): 5.2")
})

test_that("Vapor Pressure", {
  res <- pc_sect(1983, "Vapor Pressure")
  expect_equal(res$Result[1], "0.000007 [mmHg]")
})

test_that("LogP", {
  res <- pc_sect(1983, "LogP")
  expect_equal(res$Result[1], "0.46")
})

test_that("Stability / Shelf Life", {
  res <- pc_sect(1983, "Stability / Shelf Life")
  expect_equal(res$Result, "Stable under recommended storage conditions.")
})

test_that("Autoignition Temperature", {
  res <- pc_sect(1983, "Autoignition Temperature")
  expect_equal(res$Result, "540 °C")
})

test_that("pH", {
  res <- pc_sect(1983, "pH")
  expect_equal(res$Result, "Saturated aqueous solution: 5.5-6.5")
})

test_that("Dissociation Constants", {
  res <- pc_sect(1983, "Dissociation Constants")
  expect_equal(res$Result, "pKa = 9.38")
})

test_that("Collision Cross Section", {
  res <- pc_sect(1983, "Collision Cross Section")
  expect_true(nrow(res) > 5)
})

test_that("1D NMR Spectra", {
  res <- pc_sect(1983, "1D NMR Spectra")
  expect_equal(
    res$`1D NMR Spectra`[1],
    "http://nmrshiftdb.nmr.uni-koeln.de/portal/js_pane/P-Results/nmrshiftdbaction/showDetailsFromHome/molNumber/89614"
  )
})

test_that("1H NMR Spectra", {
  res <- pc_sect(1983, "1H NMR Spectra")
  expect_equal(
    res$`Shifts [ppm]:Intensity`[1],
    "9.63:20.93, 6.67:40.69, 6.65:35.56, 1.97:100.00, 7.31:35.75, 7.33:37.16, 9.11:34.80"
  )
})

test_that("GC-MS", {
  res <- pc_sect(1983, "GC-MS")
  expect_equal(
    res$`Top 5 Peaks`[1],
    "109.0:99.99, 151.0:34.59, 80.0:9.49, 110.0:7.64, 108.0:7.14"
  )
})

test_that("MS-MS", {
  res <- pc_sect(1983, "MS-MS")
  expect_equal(
    res$`Top 5 Peaks`[1],
    "152.349:100, 109.615:81.87, 111.118:68.32, 111.745:66.47, 151.471:56.21"
  )
})

test_that("LC-MS", {
  res <- pc_sect(1983, "LC-MS")
  expect_equal(res$`Collision Energy`[1], "Ramp 20%-70% (nominal)")
})

test_that("Other MS", {
  res <- pc_sect(1983, "Other MS")
  expect_true(nrow(res) > 5)
})

test_that("Raman Spectra", {
  res <- pc_sect(1983, "Raman Spectra")
  expect_true(nrow(res) > 3)
})

test_that("Other Spectra", {
  res <- pc_sect(1983, "Other Spectra")
  expect_equal(
    res$Result[1],
    "Intense mass spectral peaks: 80 m/z, 109 m/z, 151 m/z"
  )
})

test_that("Drug Indication", {
  res <- pc_sect(1983, "drug indication")
  expect_true(nrow(res) > 100)
  expect_equal(res$MeSH_Heading[1], "Migraine Disorders")
})

test_that("FDA Medication Guides", {
  res <- pc_sect(1983, "fda medication guides")
  expect_equal(res$Drug, "DARVOCET-N 50")
})

test_that("FDA Approved Drugs", {
  res <- pc_sect(1983, "fda approved drugs")
  expect_true(nrow(res) > 700)
  expect_equal(res$Drug[1], "NEOPAP")
})

test_that("FDA Orange Book", {
  res <- pc_sect(1983, "fda orange book")
  expect_true(nrow(res) > 700)
  expect_equal(res$Trade_Name[1], "ACETAMINOPHEN AND CODEINE PHOSPHATE")
})

test_that("ClinicalTrials.gov", {
  res <- pc_sect(1983, "ClinicalTrials.gov")
  expect_true(nrow(res) > 800)
  expect_true("NCT06305754" %in% res$CTID)
})

test_that("EU Clinical Trials Register", {
  res <- pc_sect(1983, "EU Clinical Trials Register")
  expect_true(nrow(res) > 100)
  expect_true("2023-000185-34" %in% res$EudraCT)
})

test_that("Drug Targets", {
  res <- pc_sect(1983, "Drug Targets")
  expect_true(nrow(res) > 300)
  expect_true("ENSG00000095303" %in% res$Open_Targets_Target_ID)
})

test_that("Uses", {
  res <- pc_sect(1983, "Uses")
  expect_true(nrow(res) > 300)
  expect_true("Pharmaceutical" %in% res$Function_Category)
})

test_that("protein sequences", {
  A <- pc_sect("1ZHY_A", "Sequence", "protein")
  expect_equal(
    A$Header,
    ">pdb|1ZHY|A Chain A, KES1 protein (Run BLAST)"
  )
})

test_that("pc_decompose_pointer()", {
  A <- pc_decompose_pointer("collection=toxvaldb&kind=^LC$")
  B <- pc_decompose_pointer("collection=chemidplus&query_type=sid&query=134972565")
  C <- pc_decompose_pointer("clinicaltrials")
  expect_equal(names(A), c("collection", "kind"))
  expect_equal(unname(unlist(A)), c("toxvaldb", "^LC$"))
  expect_equal(names(B), c("collection", "query_type", "query"))
  expect_equal(unname(unlist(B)), c("chemidplus", "sid", "134972565"))
  expect_equal(names(C), "collection")
  expect_equal(C$collection, "clinicaltrials")
})

test_that("pc_parse_information_element()", {
  # String with markup, multiple strings
  A <- pc_page(1983, "gc-ms") |> pc_find_section("gc-ms")
  A <- A$Information[[38]]
  expect_no_error(pc_parse_information_element(A, 1983, "cid"))
  # String with markup, single string
  B <- pc_page(1983, "inchi") |> pc_find_section("inchi")
  B <- B$Information[[1]]
  expect_no_error(pc_parse_information_element(B, 1983, "cid"))
  # String with markup, single string with unit
  C <- pc_page(1983, "molecular weight") |> pc_find_section("molecular weight")
  C <- C$Information[[1]]
  expect_no_error(pc_parse_information_element(C, 1983, "cid"))
  # Number, single number
  D <- pc_page(1983, "xlogp3") |> pc_find_section("xlogp3")
  D <- D$Information[[1]]
  expect_no_error(pc_parse_information_element(D, 1983, "cid"))
  # URL
  E <- pc_page(1983, "1h nmr spectra") |> pc_find_section("1h nmr spectra")
  E <- E$Information[[16]]
  expect_no_error(pc_parse_information_element(E, 1983, "cid"))
  # String with markup but info is the URL
  G <- pc_page(1983, "1d nmr spectra") |> pc_find_section("1d nmr spectra")
  G <- G$Information[[1]]
  expect_no_error(pc_parse_information_element(G, 1983, "cid"))
  # ExternalTableName
  H <- pc_page(1983, "acute effects") |> pc_find_section("acute effects")
  H <- H$Information[[1]]
  expect_no_error(pc_parse_information_element(H, 1983, "cid"))
})
