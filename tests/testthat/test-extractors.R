
test_that("extractors work with etox", {
  skip_on_cran()
  skip_if_not(ping_service("etox"), "ETOX service is down")

  out_etox_basic <- etox_basic(8252)
  expect_equivalent(cas(out_etox_basic), "50-00-0")
  expect_error(inchikey(out_etox_basic))
  expect_error(smiles(out_etox_basic))
})

test_that("extractors work with chemid", {
  skip_on_cran()
  skip_if_not(ping_service("ci"), "CHEMID service is down")

  out_ci_query <- ci_query(c('Aspirin', 'Triclosan'), from = 'name')
  expect_equivalent(cas(out_ci_query),  c("50-78-2", "3380-34-5"))
  expect_equivalent(inchikey(out_ci_query),
                    c("BSYNRYMUTXBXSQ-UHFFFAOYSA-N", "XEFQLINVKFYRCS-UHFFFAOYSA-N"))
  expect_equivalent(smiles(out_ci_query), c("CC(=O)", "Oc1cc(Cl)"))
})

test_that("extractors work with opsin", {
  skip_on_cran()
  skip_if_not(ping_service("opsin"), "OPSIN service is down")

  out_opsin_query <- opsin_query(c('Cyclopropane', 'Octane'))
  expect_error(cas(out_opsin_query), "CAS is not returned by this datasource!")
  expect_equivalent(inchikey(out_opsin_query),
                    c("LVZWSLJZHVFIQJ-UHFFFAOYSA-N", "TVMXDCGIABBOFY-UHFFFAOYSA-N"))
  expect_equivalent(smiles(out_opsin_query), c("C1CC1", "CCCCCCCC"))
})

test_that("extractors work with Alanwood", {
  skip_on_cran()
  skip_if_not(ping_service("aw"), "Alanwood database not reachable")

  out_aw_query <- aw_query(c('Fluazinam', 'Diclofop'), from = 'name')
  expect_equivalent(cas(out_aw_query), c("79622-59-6", "40843-25-2"))
  expect_equivalent(inchikey(out_aw_query),
                    c("UZCGKGPEKUCDTF-UHFFFAOYSA-N", "OOLBCHYXZDXLDS-UHFFFAOYSA-N"))
  expect_error(smiles(out_aw_query), "SMILES is not returned by this datasource!")

})

test_that("extractors work with Wikidata", {
  skip_on_cran()
  skip_if_not(ping_service("wd"), "Wikidata service is down")

  id <- c("Q408646", "Q18216")
  out_wd_ident <- wd_ident(id)
  expect_equivalent(cas(out_wd_ident), c("3380-34-5", "50-78-2"))
  expect_equivalent(inchikey(out_wd_ident),
                    c("XEFQLINVKFYRCS-UHFFFAOYSA-N", "BSYNRYMUTXBXSQ-UHFFFAOYSA-N"))
  expect_equivalent(smiles(out_wd_ident),
                    c("C1=CC(=C(C=C1Cl)O)OC2=C(C=C(C=C2)Cl)Cl","CC(=O)OC1=CC=CC=C1C(=O)O"))
})

test_that("extractors work with pubchem", {
  skip_on_cran()
  skip_if_not(ping_service("pc"), "Pubchem service is down")

  out_pc_prop <- pc_prop(c(5564, 2244))
  out_pc_prop2 <- pc_prop(5564, properties = c('MolecularFormula', 'MolecularWeight'))
  expect_error(cas(out_pc_prop))
  expect_equivalent(inchikey(out_pc_prop),
                    c("XEFQLINVKFYRCS-UHFFFAOYSA-N", "BSYNRYMUTXBXSQ-UHFFFAOYSA-N"))
  expect_error(inchikey(out_pc_prop2))
  expect_equivalent(smiles(out_pc_prop),
                    c("C1=CC(=C(C=C1Cl)O)OC2=C(C=C(C=C2)Cl)Cl", "CC(=O)OC1=CC=CC=C1C(=O)O"))
  expect_error(smiles(out_pc_prop2))
})

test_that("extractors work with PAN", {
  skip_on_cran()
  skip_if_not(ping_service("pan"), "PAN service is down")

  out_pan_query <- pan_query(c('2,4-dichlorophenol', 'Atrazin'), match = 'best')
  expect_equivalent(cas(out_pan_query),  c("120-83-2", "1912-24-9"))
  expect_error(inchikey(out_pan_query))
  expect_error(smiles(out_pan_query))
})
