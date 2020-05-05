#
#
# # CTS
# inchikeys <- c("XEFQLINVKFYRCS-UHFFFAOYSA-N","BSYNRYMUTXBXSQ-UHFFFAOYSA-N" )
# out_cts_compinfo <- cts_compinfo(inchikeys)
#
# # ETOX
# out_etox_basic <- etox_basic(8252)
#
# # ChemID
# out_ci_query <- ci_query(c('Aspirin', 'Triclosan'), type = 'name')
#
# # OPSIN
# out_opsin_query <- opsin_query(c('Cyclopropane', 'Octane'))
#
# # Alan wood
# out_aw_query <- aw_query(c('Fluazinam', 'Diclofop'), type = 'com')
#
# # Wikidata
# id <- c("Q408646", "Q18216")
# out_wd_ident <- wd_ident(id)
# # Pubchem
# out_pc_prop <- pc_prop(c(5564, 2244))
# out_pc_prop2 <- pc_prop(5564, properties = c('MolecularFormula', 'MolecularWeight'))
#
# # pan
# out_pan_query <- pan_query(c('2,4-dichlorophenol', 'Atrazin'), match = 'best')
#
#
# test_that("cas is working", {
#   skip_on_cran()
#
#   expect_equivalent(cas(out_etox_basic), "50-00-0")
#   expect_error(cas(out_opsin_query))
#   expect_equivalent(cas(out_aw_query), c("79622-59-6", "40843-25-2"))
#   expect_equivalent(cas(out_wd_ident), c("3380-34-5", "50-78-2"))
#   expect_error(cas(out_pc_prop))
#   expect_equivalent(cas(out_pan_query),  c("120-83-2", "1912-24-9"))
#   expect_equivalent(cas(out_ci_query),  c("50-78-2", "3380-34-5"))
# })
#
# test_that("inchikey is working", {
#   skip_on_cran()
#
#   expect_equivalent(inchikey(out_cts_compinfo), c("XEFQLINVKFYRCS-UHFFFAOYSA-N", "BSYNRYMUTXBXSQ-UHFFFAOYSA-N" ))
#   expect_error(inchikey(out_etox_basic))
#   expect_equivalent(inchikey(out_opsin_query), c("LVZWSLJZHVFIQJ-UHFFFAOYSA-N", "TVMXDCGIABBOFY-UHFFFAOYSA-N"))
#   expect_equivalent(inchikey(out_aw_query), c("UZCGKGPEKUCDTF-UHFFFAOYSA-N", "OOLBCHYXZDXLDS-UHFFFAOYSA-N"))
#   expect_equivalent(inchikey(out_wd_ident), c("XEFQLINVKFYRCS-UHFFFAOYSA-N", "BSYNRYMUTXBXSQ-UHFFFAOYSA-N"))
#   expect_equivalent(inchikey(out_pc_prop), c("XEFQLINVKFYRCS-UHFFFAOYSA-N", "BSYNRYMUTXBXSQ-UHFFFAOYSA-N"))
#   expect_error(inchikey(out_pc_prop2))
#   expect_error(inchikey(out_pan_query))
#   expect_equivalent(inchikey(out_ci_query), c("BSYNRYMUTXBXSQ-UHFFFAOYSA-N", "XEFQLINVKFYRCS-UHFFFAOYSA-N"))
#
#   skip_on_appveyor()
#   skip_on_travis()
#   out_cs_compinfo <- cs_compinfo('5363')
#   expect_equivalent(inchikey(out_cs_compinfo), "XEFQLINVKFYRCS-UHFFFAOYSA-N")
# })
#
# test_that("smiles is working", {
#   skip_on_cran()
#
#   expect_error(smiles(out_cts_compinfo))
#   expect_error(smiles(out_etox_basic))
#   expect_equivalent(smiles(out_opsin_query), c("C1CC1", "CCCCCCCC"))
#   expect_error(smiles(out_aw_query))
#   expect_equivalent(smiles(out_wd_ident), c("C1=CC(=C(C=C1Cl)O)OC2=C(C=C(C=C2)Cl)Cl","CC(=O)OC1=CC=CC=C1C(=O)O"))
#   expect_equivalent(smiles(out_pc_prop), c("C1=CC(=C(C=C1Cl)O)OC2=C(C=C(C=C2)Cl)Cl", "CC(=O)OC1=CC=CC=C1C(=O)O"))
#   expect_error(smiles(out_pc_prop2))
#   expect_error(smiles(out_pan_query))
#   expect_equivalent(smiles(out_ci_query), c("CC(=O)", "c1(Oc2c(cc(Cl)"))
#
#   skip_on_appveyor()
#   skip_on_travis()
#   out_cs_compinfo <- cs_compinfo('5363')
#   expect_equivalent(smiles(out_cs_compinfo), "c1cc(c(cc1Cl)O)Oc2ccc(cc2Cl)Cl")
# })