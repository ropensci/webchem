require(RCurl)
is.down <- function(qurl){
  cont <- try(getURL(qurl, .encoding = 'UTF-8', .opts = list(timeout = 3)),
              silent = TRUE)
  return(inherits(cont, 'try-error'))
}
down_cts <- is.down('http://cts.fiehnlab.ucdavis.edu/service/compound/XEFQLINVKFYRCS-UHFFFAOYSA-N')
down_etox <- is.down('https://webetox.uba.de/webETOX/public/search/stoff.do')
down_ci <- is.down("http://chem.sis.nlm.nih.gov/chemidplus")
down_opsin <- is.down("http://opsin.ch.cam.ac.uk/opsin/")
down_aw <- is.down("http://www.alanwood.net/pesticides")
down_wd <- is.down("https://www.wikidata.org/w/api.php")

test_that("extractors work with cts", {
  skip_on_cran()
  skip_if(down_cts, "CTS service is down")

  inchikeys <- c("XEFQLINVKFYRCS-UHFFFAOYSA-N","BSYNRYMUTXBXSQ-UHFFFAOYSA-N" )
  out_cts_compinfo <- cts_compinfo(inchikeys)
  expect_equivalent(inchikey(out_cts_compinfo),
                    c("XEFQLINVKFYRCS-UHFFFAOYSA-N", "BSYNRYMUTXBXSQ-UHFFFAOYSA-N" ))
})


test_that("extractors work with etox", {
  skip_on_cran()
  skip_if(down_etox, "ETOX service is down")

  out_etox_basic <- etox_basic(8252)
  expect_equivalent(cas(out_etox_basic), "50-00-0")
  expect_error(inchikey(out_etox_basic))
  expect_error(smiles(out_etox_basic))
})

test_that("extractors work with chemid", {
  skip_on_cran()
  skip_if(down_ci, "CHEMID service is down")

  out_ci_query <- ci_query(c('Aspirin', 'Triclosan'), type = 'name')
  expect_equivalent(cas(out_ci_query),  c("50-78-2", "3380-34-5"))
  expect_equivalent(inchikey(out_ci_query),
                    c("BSYNRYMUTXBXSQ-UHFFFAOYSA-N", "XEFQLINVKFYRCS-UHFFFAOYSA-N"))
  expect_equivalent(smiles(out_ci_query), c("CC(=O)", "c1(Oc2c(cc(Cl)"))
})

test_that("extractors work with opsin", {
  skip_on_cran()
  skip_if(down_opsin, "OPSIN service is down")

  out_opsin_query <- opsin_query(c('Cyclopropane', 'Octane'))
  expect_error(cas(out_opsin_query))
  expect_equivalent(inchikey(out_opsin_query),
                    c("LVZWSLJZHVFIQJ-UHFFFAOYSA-N", "TVMXDCGIABBOFY-UHFFFAOYSA-N"))
  expect_equivalent(smiles(out_opsin_query), c("C1CC1", "CCCCCCCC"))
})

test_that("extractors work with Alanwood", {
  skip_on_cran()
  skip_if(down_aw, "Alanwood database not reachable")

  out_aw_query <- aw_query(c('Fluazinam', 'Diclofop'), type = 'com')
  expect_equivalent(cas(out_aw_query), c("79622-59-6", "40843-25-2"))
  expect_equivalent(inchikey(out_aw_query),
                    c("UZCGKGPEKUCDTF-UHFFFAOYSA-N", "OOLBCHYXZDXLDS-UHFFFAOYSA-N"))
  expect_error(smiles(out_aw_query))

})

test_that("extractors work with Wikidata", {
  skip_on_cran()
  skip_if(down_wd, "Wikidata service is down")

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
  skip_if_not(ping_pubchem(), "Pubchem service is down")

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
  skip_if_not(ping_pan(), "PAN service is down")

  out_pan_query <- pan_query(c('2,4-dichlorophenol', 'Atrazin'), match = 'best')
  expect_equivalent(cas(out_pan_query),  c("120-83-2", "1912-24-9"))
  expect_error(inchikey(out_pan_query))
  expect_error(smiles(out_pan_query))
})

test_that("extractors work with ChemSpider", {
  skip_on_cran()
  skip_on_travis()
  skip_on_appveyor()
  skip_if_not(ping_cs(), "ChemSpider service is down")

  out_cs_compinfo <- cs_compinfo('5363')
  expect_equivalent(inchikey(out_cs_compinfo), "XEFQLINVKFYRCS-UHFFFAOYSA-N")
  expect_equivalent(smiles(out_cs_compinfo), "c1cc(c(cc1Cl)O)Oc2ccc(cc2Cl)Cl")
})