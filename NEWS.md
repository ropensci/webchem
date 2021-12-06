# webchem 1.1.2

## NEW FEATURES

* Export chemical structures in Mol format with write_mol().

## BUG FIXES

* ci_query() can no longer query chemicals by name.
* Non-exported function ping_pubchem_pw() was incorrectly reporting that PUG VIEW was down.  This has been fixed.
* is.cas() now catches whitespaces correctly.
* aw_query() was renamed and adapted to bcpc_query, as the alanwood site has moved

## MINOR IMPROVEMENTS

* webchem functions now default to global options regarding verbose messages.

# webchem 1.1.1.

## NEW FEATURES

* Fetch LIPID MAPS and SwissLipids identifiers from Wikidata.

## BUG FIXES

* Fix get_csid() so it doesn't break when a query is invalid.

# webchem 1.1.0.

## NEW FEATURES

* Download images of substances from Chemical Identifier Resolver (CIR) with `cir_img()`.
* Download images of substances from ChemSpider with `cs_img()`.
* `find_db()` checks if a query gets a hit in most databases integrated in webchem. Useful for deciding which of several databases to focus on given a set of chemicals.

## MINOR IMPROVEMENTS

* Most functions now use httr::RETRY() to access webservices.
* Verbose messages are now harmonized.
* The `"type"` argument in `ci_query()` and `aw_query()` has been changed to `"from"` for consistency with other functions.
* `fn_percept()` and `cts_compinfo()` now have `"query"` and `"from"` arguments for consistency with other functions.
* Possible values for `"from"` have been made more consistent across functions.
* `pc_synonyms()`, `cts_convert()`, `cir_query()` have been changed to use the `match` argument instead of `choices` for consistency with other functions.
* `get_etoxid()` output changed slightly so that the matched chemical name string no longer includes the etoxid in parentheses.
* `is.cas()` is now vectorized.

## BUG FIXES

* Fix URL encoding so SMILES queries don't fail on some special characters.

# webchem 1.0.0

## NEW FEATURES

* get_cid() now can search by registry IDs (e.g. CAS RN), and can handle more complex requests like searching for similar compounds.
* Retrieve chemical data from PubChem content pages with pc_sect().
* get_etoxid() now can search by CAS, EC, GSBL and RTECS numbers. Added `from = ` argument.
* nist_ri() now can search by name, InChI, InChIKey, or CAS.  The `cas` argument is deprecated.  Use `query` instead with `from = "cas"`.

## MINOR IMPROVEMENTS

* All `get_*()` functions now output tibbles with a column for the query and a column for the retrieved ID.
* Changes to arguments in `get_*()` functions to make them more consistent.
* aw_idx.rda is no longer included in the package as a data set. Instead, it is built by build_aw_idx() to tempdir().


## BUG FIXES

* nist_ri() returned malformed tables or errored if there was only one entry for a query.
* get_csid() now returns all csids when queried from formula.
* get_csid() returned an error when query was NA.
* get_chebiid() and chebi_comp_entity() fixed for invalid queries.
* get_cid() returned the PubChem ID of sodium when the query was NA.
* aw_query() returned a list for successful queries, NA for unsuccessful queries.

## DEPRECATED FUNCTIONS

## DEFUNCT FUNCTIONS

# webchem 0.5.0


## NEW FEATURES

* Retrieve data from ChEBI (https://www.ebi.ac.uk/chebi/) webservice with get_chebiid() and chebi_comp_entity(). ChEBI comprises a rich data base on chemicals with bilogical interest.
* Retrieve retention indices from NIST (https://webbook.nist.gov) with nist_ri().
* Get record details from US EPA Substance Registry Services (https://cdxnodengn.epa.gov/cdx-srs-rest/) with srs_query().
* "first" argument in cts_convert() and cir_query() and "interactive" argument in pc_synonyms() deprecated.  Use "choices" instead to return either a list of all results, only the first result, or an interactive menu to choose a result to return.
* ChemSpider functions now look for an API token stored in .Renviron or .Rprofile by default so you can keep them hidden more easily.

## MINOR IMPROVEMENTS

* as.cas() added.
* Removed documentation files for non-exported functions that were only used internally.

## BUG FIXES

* cs_prop() failed with duplicated return values.
* pp_query() failed when compound present, but no properties.
* ci_query() failed when missing table.
* get_csid() failed because of a major change in the ChemSpider API.
* multiple functions failed because of a major change in the ChemSpider API.
* cir_query() mistook NA for sodium.
* fixed functions that communicate with the ChemSpider API.
* get_etoxid() printed incorrect results for certain match types.

## DEPRECATED FUNCTIONS

* cs_extcompinfo() cannot be fixed as there is no equivalent in the new ChemSpider API yet.

## DEFUNCT FUNCTIONS

* ppdb_parse() has been removed. webchem no longer offers any support for PPDB.
* pp_query() has been removed. Physprop API is no longer active.
* cs_prop() has been removed.

# webchem 0.4.0

## NEW FEATURES

## MINOR IMPROVEMENTS

## BUG FIXES

* extr_num() did not work properly with decimal numbers.
* cs_prop() failed when epi-suite data was not available.
* cs_prop() failed with invalid html.
* cs_prop() gave incorrect answer, if entries were not available.
* cs_prop() did not parse scientific number correctly.
* is.smiles() failed because of changes in rcdk.
* cir_query() failed with identifiers containing spaces (e.g. 'acetic acid').
* Aeveral other functions failed with identifiers containing spaces & returned wrong distance.

## DEPRECATED FUNCTIONS

## DEFUNCT FUNCTIONS


# webchem 0.3.0


## NEW FEATURES

## MINOR IMPROVEMENTS

* cs_prop() now also return experimental data for Boiling and Melting Points.
* pc_synonyms gained an argument 'interactive' to enter an interactive mode for selecting synonyms.
* cts_convert now returns NA if no matches are found.

## BUG FIXES

* cs_prop() failed with some CSIDs.
* wd_ident() failed if multiple entries where found. Now returns the first hit only.
* ci_query() did not return fully cleaned smiles and inchi.

## DEPRECATED FUNCTIONS

## DEFUNCT FUNCTIONS


# webchem 0.2.0


## NEW FEATURES

* fn_percept() extracts flavor percepts using CAS numbers from www.flavornet.org. Flavornet is a database of 738 compounds with human-detectible odors.

## MINOR IMPROVEMENTS

## BUG FIXES

## DEPRECATED FUNCTIONS

## DEFUNCT FUNCTIONS



# webchem 0.1.1


## NEW FEATURES
* Added ping_pubchem() to check whether pubchem is up & running.
* Added cs_web_ping () to check whether the chemspider webpage is functional.

## MINOR IMPROVEMENTS
* Updated allan wood index.

## BUG FIXES
* pc_prop() returned to many rows if last cid supplied was NA.
* Switched to https for NCBI, chemspider & chemid.
* get_wdid() failed if non-ascii characters where returned by wikipedia.
* rcdk:parse.smiles() now returns NA if a SMILES string could not be parsed.
   => broke is.smiles

## DEPRECATED FUNCTIONS

## DEFUNCT FUNCTIONS



# webchem 0.1.0


## NEW FEATURES
* Added cts_to() and cts_from() to retrieve possible ids that can be queried.
* cts_*(), pp_query(), cir_query(), get_cid(), get_etoxid(), etox_*(), pan_query() get_wdid(), aw_query(), get_csid(), cs_prop(), cs_compinfo() and ci_query() can handle multiple inputs.
* pc_prop() queries properties and pc_synonmy() synonyms from PUG-REST.
* Added extractors for webchem objects: cas(), inchikey() and smiles().


## MINOR IMPROVEMENTS
* Rewrite of pubchem functions using PUG-REST.
* ChemSpider: better use of NA in input (=return NA).
* More robust matching in get_etoxid.

## BUG FIXES

* pan_query() did not return numeric values.
* get_cid() failed with multiple results.

## DEPRECATED FUNCTIONS


## DEFUNCT FUNCTIONS

* ppdb_query() has been removed due to copyright issues.
The new ppdb_parse() parses only a html, but does not interact with the database.
* pan()
* alanwood()
* get_cid()
* cid_compinfo()
* chemid()
* physprop()



# webchem 0.0.5

## NEW FEATURES

* is.smiles() checks SMILES strings, by parsing via (R)CDK.
* get_wdid() and wd_indent() to retrieve information from wikidata.
* get_etoxid() can handle multi inputs (interactive mode, best match, first match, NA and all matches).
* ci_query() can handle multi inputs (interactive mode, best match, first match and NA).
* cs_prop() queries predictions (ACD and EPiSuite) from ChemSpider.

## MINOR IMPROVEMENTS

* webchem uses exclusively xml2 (instead of XML).
* All function return source_url for (micro-)attribution of sources.
* cs_compinfo(): names of returned list changed.
* cs_extcompinfo():
  - names of returned list changed.
  - result is numeric where appropriate.
* cir(): result is numeric where appropriate.
* Unified naming scheme of functions.
* is.inchikey_cs() has been integrated into is.inchikey().
* aw_query() returns multiple inchikey if found.
* pan() now returns chemical name and matched synonym.

## BUG FIXES

* Utility functions are not vectorized and throw an error.
* chemid() did mot work with inchikey as input.
* ppdb_idx returned duplicated CAS values, which caused ppdb() to fail.
* ppdb() failed in some cases because of false encoding.
* etox_*() functions are more robust.
* ci_query() failed if multi hits were found. Now returns first hit.
* aw_fuery() failed if inchikey was not found.

## DEPRECATED FUNCTIONS

* pan_query() replaces pan().
* aw_query() replaces alanwood().
* get_pcid() replaces get_cid().
* pc_compinfo() replaces cid_compinfo().
* ci_query() replaces chemid().
* pp_query() replaces physprop().

## DEFUNCT FUNCTIONS

* csid_compinfo()
* csid_extcompinfo()




# webchem 0.0.4


## NEW FEATURES

* chemid() to query ChemIDplus http://chem.sis.nlm.nih.gov/chemidplus/.
* is.inchikey() and is.cas() to check if a string is valid inchikey or CAS registry number.
* parse_mol(): A simple molfile parser.
* Functions to work with ChemSpider InChI API:
  + cs_csid_mol() : convert csid to mol.
  + cs_inchikey_csid() : convert inchikey to csid.
  + cs_inchikey_inchi() : convert inchikey to inchi.
  + cs_inchikey_mol() : convert inchikey to Molfile.
  + cs_inchi_csid() : convert inchi to csid.
  + cs_inchi_inchikey : convert inchi to inchikey.
  + cs_inchi_mol() : convert inchi to molfile.
  + cs_inchi_smiles() : convert inchi to smiles.
  + cs_smiles_inchi() : convert smiles to inchi.
  + These are all wrapped into cs_convert().
  + is.inchikey_cs() : Check via ChemSpider if inchikey is valid.
* webchem has now a zenodo doi, please cite if you use it.


## MINOR IMPROVEMENTS

* cts_compinfo() checks if input is a inchikey (via exported function is.inchikey()).
* cts_compinfo() is now more robust and verbose, if problems are encountered.
* alanwood() returns separate inchi and ichikeys in case of isomers.
* alanwood() returns also subactvity (e.g. $Fluazinam$activity [1] "fungicides" and $Fluazinam$subactivity [1] "pyridine fungicides").
* physprop() also returns boiling and melting points. Moreover, values are now numeric.


## BUG FIXES

* alanwood() returns only results for first match in case of multiple links found.
* physprop() stopped working after change of SRC to https, fixed now.
* Changed etox_* functions to https.


## DEPRECATED FUNCTIONS

* ppdb() replaces ppdb_query() and accepts individual index as created by ppdb_buildidx().
* cir() replaces cir_query().
* cs_compinfo() replaces csid_compinfo().
* cs_extcompinfo() replaces csid_extcompinfo().


## DEFUNCT FUNCTIONS

* allanwood()




# webchem 0.0.3


## NEW FEATURES

* Query SRC PHYSPROP Database with physprop().
* Query the ETOX ID with get_etoxid(); query basic information with etox_basic();
  quality targets with etox_targets() and test results with etox_tests().
* Query PPDB with ppdb_query().

## MINOR IMPROVEMENTS

* Added exceptions/checks to tests.
* Improved robustness of cir_query().

## BUG FIXES

* Correct the spelling of Alan Wood and rename function allanwood() to alanwood().



# webchem 0.0.2


## NEW FEATURES

* Query the PAN Pesticides Database with pan().
* Query Allan Woods Compendium of Pesticide Common Names with allanwood().

## MINOR IMPROVEMENTS

* Added checks for user input.
* Fixed documentation, added example for bulk processing.
* cts_convert() returns NA if no result was found.
* Set 'verbose = TRUE' as default for all functions.
* Added unit tests.
* All functions return silently NA, if API is not reachable.

## BUG FIXES

* cts_convert() does not ignore 'first' argument.
* get_csid() did not return NA, if there was a problem with the API.
* Many functions returned 'NA2+' if NA was given - now return NA by default.
* Many fixes in NA handling, e.g. when no hit was found.
