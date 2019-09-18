
<!-- README.md is generated from README.Rmd. Please edit that file -->

# Webchem

<!-- badges: start -->

[![Build
Status](https://travis-ci.org/ropensci/webchem.png)](https://travis-ci.org/ropensci/webchem)
[![Build
status](https://ci.appveyor.com/api/projects/status/e3sa6e918jlemv46/branch/master)](https://ci.appveyor.com/project/EDiLD/webchem)
[![Coverage
Status](https://codecov.io/github/ropensci/webchem/coverage.svg?branch=tests)](https://codecov.io/gh/ropensci/webchem/branch/tests)
[![Open
Issues](https://img.shields.io/github/issues/ropensci/webchem.svg)](https://github.com/ropensci/webchem/issues)
<http://cranlogs.r-pkg.org/badges/webchem> [![CRAN
status](https://www.r-pkg.org/badges/version/webchem)](https://CRAN.R-project.org/package=webchem)
[![DOI](https://zenodo.org/badge/17223/ropensci/webchem.svg)](https://zenodo.org/badge/latestdoi/17223/ropensci/webchem)
<!-- badges: end -->

`webchem` is a R package to retrieve chemical information from the web.
This package interacts with a suite of web APIs to retrieve chemical
information.

The functions in the package that hit a specific API have a prefix and
suffix separated by an underscore (`prefix_suffix()`) They follow the
format of `source_functionality`, with the exception of functions that
retrieve database identifiers which follow the format of
`get_identifier`. e.g.`cs_compinfo` uses ChemSpider to retrieve compound
informations and `get_csid()` retrieves ChemSpider
IDs.

## Currently implemented in `webchem`

| Source                                                                                        | Function(s)                                                                                                                | API Docs                                                           | API key                                       |
| --------------------------------------------------------------------------------------------- | -------------------------------------------------------------------------------------------------------------------------- | ------------------------------------------------------------------ | --------------------------------------------- |
| [Chemical Identifier Resolver (CIR)](http://cactus.nci.nih.gov/chemical/structure)            | `cir_query()`                                                                                                              | [link](http://cactus.nci.nih.gov/chemical/structure_documentation) | none                                          |
| [ChemSpider](http://www.chemspider.com/)                                                      | `cs_datasources()`,`cs_name_csid()`, `cs_element_csid()`, `cs_convert()`, `cs_compinfo()`, `cs_extcompinfo()`, `cs_prop()` | [link](https://developer.rsc.org/compounds-v1/apis)                | required [(link)](https://developer.rsc.org/) |
| [PubChem](https://pubchem.ncbi.nlm.nih.gov/)                                                  | `get_cid()`, `pc_prop()`, `pc_synonyms()`                                                                                  | [link](https://pubchem.ncbi.nlm.nih.gov/)                          | none                                          |
| [Chemical Translation Service (CTS)](http://cts.fiehnlab.ucdavis.edu/)                        | `cts_convert()`, `cts_compinfo()`                                                                                          | none                                                               | none                                          |
| [PAN Pesticide Database](http://www.pesticideinfo.org/)                                       | `pan_query()`                                                                                                              | none                                                               | none                                          |
| [Alan Wood’s Compendium of Pesticide Common Names](http://www.alanwood.net/pesticides/)       | `aw_query()`                                                                                                               | none                                                               | none                                          |
| [PHYSPROP Database](http://www.srcinc.com/what-we-do/environmental/scientific-databases.html) | `pp_query()`                                                                                                               | none                                                               | none                                          |
| [ETOX](http://webetox.uba.de/webETOX/index.do)                                                | `get_etoxid()`, `etox_basic()`. `etox_targets()`, `etox_tests()`                                                           | none                                                               | none                                          |
| PPDB                                                                                          | `ppdb_parse()` (only parsing)                                                                                              | none                                                               | none                                          |
| [ChemIDplus](http://chem.sis.nlm.nih.gov/chemidplus/)                                         | `ci_query()`                                                                                                               | none                                                               | none                                          |
| [Wikidata](https://www.wikidata.org/wiki/Wikidata:WikiProject_Chemistry)                      | `get_wdid()`, `wd_ident()`                                                                                                 | [link](https://www.mediawiki.org/wiki/API:Main_page)               | none                                          |
| [OPSIN](http://opsin.ch.cam.ac.uk/instructions.html)                                          | `opsin_query()`                                                                                                            | [link](http://opsin.ch.cam.ac.uk/instructions.html)                | none                                          |
| [Flavornet](http://www.flavornet.org)                                                         | `fn_percept()`                                                                                                             | none                                                               | none                                          |
| [NIST](https://webbook.nist.gov)                                                              | `nist_ri()`                                                                                                                | none                                                               | none                                          |
| [ChEBI](https://www.ebi.ac.uk/chebi/)                                                         | `chebi_lite_entity()`, `chebi_comp_entity()`                                                                               | [link](https://www.ebi.ac.uk/chebi/webServices.do)                 | none                                          |

Moreover, there are some functions to check indentifiers:
`is.inchikey()`, `is.cas()` and `is.smiles()`.

#### API keys

Some ChemSpider functions require an API key. Please register at RSC
(<https://developer.rsc.org/>) to retrieve an API key.

## Installation

#### Install from CRAN (stable version)

``` r
install.packages("webchem")
```

#### Install from Github (development version)

``` r
install.packages("devtools")
library("devtools")
install_github("ropensci/webchem")
```

## Quickstart

``` r
library("webchem")
```

#### Chemical Identifier Resolver (CIR)

CAS numbers and molecular weight for
[Triclosan](http://en.wikipedia.org/wiki/Triclosan). Use `choices = 1`
to return only the first hit.

``` r
cir_query('Triclosan', 'cas')
#> $Triclosan
#> [1] "3380-34-5"   "112099-35-1" "88032-08-0"
cir_query('Triclosan', 'cas', choices = 1)
#>    Triclosan1    Triclosan2    Triclosan3 
#>   "3380-34-5" "112099-35-1"  "88032-08-0"
cir_query('Triclosan', 'mw')
#> $Triclosan
#> [1] 289.5451
```

Query SMILES and InChIKey from CAS (Triclosan). Inputs might by
ambiguous and we can specify where to search using `resolver=`.

``` r
cir_query('3380-34-5', 'smiles')
#> $`3380-34-5`
#> [1] "Oc1cc(Cl)ccc1Oc2ccc(Cl)cc2Cl"
cir_query('3380-34-5', 'stdinchikey', resolver = 'cas_number')
#> $`3380-34-5`
#> [1] "InChIKey=XEFQLINVKFYRCS-UHFFFAOYSA-N"
```

Query the number of rings using the InChiKey (Triclosan)

``` r
cir_query('XEFQLINVKFYRCS-UHFFFAOYSA-N', 'ring_count')
#> $`XEFQLINVKFYRCS-UHFFFAOYSA-N`
#> [1] 2
```

#### ChemSpider

You’ll need a API key:

``` r
apikey = '<YOUR TOKEN HERE>'
```

Retrieve the ChemSpider ID of Triclosan

``` r
(id <- cs_name_csid('Triclosan', apikey = apikey))
#> $results
#> [1] 5363
#> 
#> $limitedToMaxAllowed
#> [1] FALSE
```

Use this ID to query information from
ChemSpider

``` r
# cs_compinfo(id, fields = c("Formula", "MolecularWeight"), apikey = apikey)
```

Note that the URL of the source if also returned (`source_url`) and can
be used for (micro-)attribution.

Or to convert to a Mol-Object

``` r
# mol <- cs_convert(id, from = 'csid', to = 'mol', apikey = apikey)
# head(parse_mol(mol$ab))
```

Note that the Molfile is parsed into a R object (via `parse_mol()`) and
that an API-key is needed

`cs_convert()` handles a lot of input / output
formats:

``` r
cs_convert('XEFQLINVKFYRCS-UHFFFAOYAS', from = 'inchikey', to = 'csid', apikey = apikey)
#> $results
#> [1] 5363
#> 
#> $limitedToMaxAllowed
#> [1] FALSE
cs_convert('XEFQLINVKFYRCS-UHFFFAOYAS', from = 'inchikey', to = 'inchi', apikey = apikey)
#> [1] "InChI=1/C12H7Cl3O2/c13-7-1-3-11(9(15)5-7)17-12-4-2-8(14)6-10(12)16/h1-6,16H"
cs_convert('c1cc(c(cc1Cl)O)Oc2ccc(cc2Cl)Cl', from = 'smiles', to = 'inchi', apikey = apikey)
#> [1] "InChI=1/C12H7Cl3O2/c13-7-1-3-11(9(15)5-7)17-12-4-2-8(14)6-10(12)16/h1-6,16H"
```

And get EPISuit predictions from ChemSpider

``` r
cs_prop('5363')[['5363']]$epi[ , c(1:4)]
```

#### PubChem

Retrieve PubChem CID

``` r
get_cid(c('Triclosan', 'Aspirin'))
#> $Triclosan
#> [1] 5564
#> 
#> $Aspirin
#> [1] 2244
get_cid('3380-34-5')
#> $`3380-34-5`
#> [1] 5564
```

Use this CID to retrieve some chemical
properties:

``` r
pc_prop(c(5564,2244), properties = c('InChIKey', 'MolecularFormula', 'MolecularWeight'))
#>    CID MolecularFormula MolecularWeight                    InChIKey
#> 1 5564       C12H7Cl3O2          289.50 XEFQLINVKFYRCS-UHFFFAOYSA-N
#> 2 2244           C9H8O4          180.16 BSYNRYMUTXBXSQ-UHFFFAOYSA-N
```

and synonyms

``` r
pc_synonyms(5564, from = 'cid')[[1]][1:5]
#> [1] "5564"                                     
#> [2] "triclosan"                                
#> [3] "3380-34-5"                                
#> [4] "5-CHLORO-2-(2,4-DICHLOROPHENOXY)PHENOL"   
#> [5] "2,4,4'-Trichloro-2'-hydroxydiphenyl ether"
pc_synonyms('Triclosan', from = 'name')[[1]][1:5]
#> [1] "5564"                                     
#> [2] "triclosan"                                
#> [3] "3380-34-5"                                
#> [4] "5-CHLORO-2-(2,4-DICHLOROPHENOXY)PHENOL"   
#> [5] "2,4,4'-Trichloro-2'-hydroxydiphenyl ether"
```

#### Chemical Translation Service (CTS)

CTS allows to convert from nearly every possible identifier to nearly
every possible identifier:

``` r
cts_convert(query = '3380-34-5', from = 'CAS', to = 'ChemSpider')
#> $`3380-34-5`
#> [1] "31465"
(inchk <- cts_convert(query = '50-00-0', from = 'CAS', to = 'inchikey'))
#> $`50-00-0`
#> [1] "WSFSSNUMVMOOMR-UHFFFAOYSA-N"
```

Moreover, we can a lot of information stored in the CTS database using
InChIkey

``` r
info <- cts_compinfo(inchikey = inchk[[1]])
info[[1]][1:5]
#> $inchikey
#> [1] "WSFSSNUMVMOOMR-UHFFFAOYSA-N"
#> 
#> $inchicode
#> [1] "InChI=1S/CH2O/c1-2/h1H2"
#> 
#> $molweight
#> [1] 30.02602
#> 
#> $exactmass
#> [1] 30.01056
#> 
#> $formula
#> [1] "CH2O"
```

#### PAN Pesticide Database

`pan_query()` returns a list of 75 entries, here I extract only 4 of
those:

``` r
pan_list <- pan_query('lambda-Cyhalothrin', match = 'best')
pan_list[[1]][c("CAS Number", "Chemical Class", "Water Solubility (Avg, mg/L)", "Adsorption Coefficient (Koc)" )]
#> $`CAS Number`
#> [1] "91465-08-6"
#> 
#> $`Chemical Class`
#> [1] "Pyrethroid"
#> 
#> $`Water Solubility (Avg, mg/L)`
#> [1] NA
#> 
#> $`Adsorption Coefficient (Koc)`
#> [1] 157000
```

#### Alan Wood’s Compendium of Pesticide Common Names

`aw_query()` returns a list of 9 entries and can query common names and
cas numbers:

``` r
aw_query('Fluazinam', type = 'commonname')
#> $Fluazinam
#> $Fluazinam$cname
#> [1] "Fluazinam"
#> 
#> $Fluazinam$status
#> [1] "ISO 1750 (published)"
#> 
#> $Fluazinam$pref_iupac_name
#> [1] "3-chloro-N-[3-chloro-2,6-dinitro-4-(trifluoromethyl)phenyl]-5-(trifluoromethyl)pyridin-2-amine"
#> 
#> $Fluazinam$iupac_name
#> [1] "3-chloro-N-(3-chloro-5-trifluoromethyl-2-pyridyl)-α,α,α-trifluoro-2,6-dinitro-p-toluidine"
#> 
#> $Fluazinam$cas
#> [1] "79622-59-6"
#> 
#> $Fluazinam$formula
#> [1] "C13H4Cl2F6N4O4"
#> 
#> $Fluazinam$activity
#> [1] "fungicides"
#> 
#> $Fluazinam$subactivity
#> [1] "pyridine fungicides"
#> 
#> $Fluazinam$inchikey
#> [1] "UZCGKGPEKUCDTF-UHFFFAOYSA-N"
#> 
#> $Fluazinam$inch
#> [1] "InChI=1S/C13H4Cl2F6N4O4/c14-6-1-4(12(16,17)18)3-22-11(6)23-9-7(24(26)27)2-5(13(19,20)21)8(15)10(9)25(28)29/h1-3H,(H,22,23)"
#> 
#> $Fluazinam$source_url
#> [1] "http://www.alanwood.net/pesticides/fluazinam.html"
#> 
#> 
#> attr(,"class")
#> [1] "list"     "aw_query"
aw_query('79622-59-6', type = 'cas')[[1]]$cname
#> [1] "fluazinam"
```

#### SRC PHYSPROP Database

[SRCs PHYSPROP
Database](http://www.srcinc.com/what-we-do/environmental/scientific-databases.html)
contains chemical structures, names and physical properties for over
41,000 chemicals. You can use `pp_query()` to query this database using
a CAS number:

``` r
pp_query('50-00-0')
#> $`50-00-0`
#> $`50-00-0`$cas
#> [1] "50-00-0"
#> 
#> $`50-00-0`$cname
#> [1] "FORMALDEHYDE"
#> 
#> $`50-00-0`$mw
#> [1] 30.026
#> 
#> $`50-00-0`$prop
#>                       variable      value             unit     temp type
#> 1             Water Solubility  4.000e+05             mg/L 20 deg C  EXP
#> 2        Log P (octanol-water)  3.500e-01                  25 deg C  EXP
#> 3               Vapor Pressure  3.886e+03            mm Hg 25 deg C  EXT
#> 4    pKa Dissociation Constant  1.327e+01                  25 deg C  EXP
#> 5         Henry's Law Constant  3.370e-07       atm-m3/mol 25 deg C  EXP
#> 6 Atmospheric OH Rate Constant  9.370e-12 cm3/molecule-sec 25 deg C  EXP
#> 7                Melting Point -9.200e+01            deg C     <NA> <NA>
#> 8                Boiling Point -1.950e+01            deg C     <NA> <NA>
#>                                ref
#> 1        PICKRELL,JA ET AL. (1983)
#> 2           HANSCH,C ET AL. (1995)
#> 3          BOUBLIK,T ET AL. (1984)
#> 4   SERJEANT,EP & DEMPSEY,B (1979)
#> 5 BETTERTON,EA & HOFFMAN,MR (1988)
#> 6     KWOK,ESC & ATKINSON,R (1994)
#> 7                             <NA>
#> 8                             <NA>
#> 
#> $`50-00-0`$source_url
#> [1] "http://esc.syrres.com/fatepointer/webprop.asp?CAS=50000"
```

#### ETOX

ETOX: Information System Ecotoxicology and Environmental Quality Targets
is a database run by the Federal Environment Agency of Germany and
provides data on synonyms, identifiers, Quality Targest and Effects.

First we need to query a substance ID:

``` r
ids <- get_etoxid('Triclosan', match = 'best')
ids
#>   etoxid               match distance     query
#> 1  20179 Triclosan ( 20179 )        0 Triclosan
```

`get_etoxid` tries to find the best match for you (check the matched and
distance attributes), if multiple hits are found. Other options are
`match = 'ask'` to enter a interactive mode, `'na'` to return `NA`,
`'all'` to return all hits and `'first'` to return the first hit.

``` r
get_etoxid('Triclosan', match = 'all')
#> [[1]]
#> [1] "/webETOX/public/search/stoff.do?orderBy=name"
#> [2] "89236"                                       
#> [3] "20179"                                       
#> attr(,"matched")
#> [1] NA                          "Methyltriclosan ( 89236 )"
#> [3] "Triclosan ( 20179 )"      
#> attr(,"distance")
#> [1] "all"
```

With this substance ID we can query further information from ETOX, e.g.:

``` r
etox_basic(ids$etoxid)[[1]]
#> $cas
#> [1] "3380-34-5"
#> 
#> $ec
#> character(0)
#> 
#> $gsbl
#> [1] "117338"
#> 
#> $synonyms
#>                                          name  language
#> 3      5-chloro-2-(2,4-dichlorophenoxy)phenol   English
#> 4   Phenol, 5-chloro-2-(2,4-dichlorophenoxy)-   English
#> 8     2,4,4'-Trichlor-2'-hydroxydiphenylether    German
#> 9      2,4,4-Trichlor-2'-hydroxydiphenylether    German
#> 10   2,4,4'-Trichloro-2'-hydroxydiphenylether    German
#> 12     Chlor-2-(2,4-dichlorphenoxy)phenol, 5- universal
#> 13 Trichloro-2'-hydroxydiphenylether, 2,4,4'- universal
#> 14      5-Chlor-2-(2,4-dichlorphenoxy)-phenol universal
#> 15    Chlor-2-(2,4-dichlorphenoxy)-phenol, 5- universal
#> 16       5-Chlor-2-(2,4-dichlorphenoxy)phenol universal
#> 17                                  triclosán   Spanish
#> 18                                triklosaani   Finnish
#> 19                                 triclosano   Italian
#> 20                                  triklosan   Swedish
#> 
#> $source_url
#> [1] "https://webetox.uba.de/webETOX/public/basics/stoff.do?language=en&id=20179"
```

Which returns CAS, EC and GSBL numbers, as well as a synonym list.

We can also retrieve Quality Targets:

``` r
targets <- etox_targets(ids$etoxid)[[1]]
targets$res[ , c('Substance', 'Country_or_Region', 'Designation', 'Value_Target_LR', 'Unit')]
#>    Substance Country_or_Region      Designation Value_Target_LR Unit
#> 1  Triclosan               AUS             PNEC           0.050 µg/l
#> 2  Triclosan               CHE AA-QS_freshwater           0.020 µg/l
#> 3  Triclosan               CHE           MAC-QS           0.020 µg/l
#> 4  Triclosan               DEU           AA-EQS           0.020 µg/l
#> 5  Triclosan               DEU          MAC-EQS           0.200 µg/l
#> 6  Triclosan               DEU       QS_fw, eco           0.020 µg/l
#> 7  Triclosan               DEU   MAC-QS_fw, eco           0.160 µg/l
#> 8  Triclosan               DEU       QS_sw, eco           0.002 µg/l
#> 9  Triclosan               DEU   MAC-QS_sw, eco           0.016 µg/l
#> 10 Triclosan               DEU           AA-EQS           0.020 µg/l
#> 11 Triclosan               DEU           AA-EQS           0.002 µg/l
#> 12 Triclosan               DEU          MAC-EQS           0.200 µg/l
#> 13 Triclosan               DEU          MAC-EQS           0.020 µg/l
```

and results of ecotox tests:

``` r
tests <- etox_tests(ids$etoxid)[[1]]
tests$res[ , c('Organism', 'Effect', 'Duration', 'Time_Unit','Endpoint', 'Value', 'Unit')]
#>                           Organism                 Effect Duration
#> 1              Anabaena flos-aquae           not reported        4
#> 2          Brachionus calyciflorus           not reported        2
#> 3          Brachionus calyciflorus           not reported        2
#> 4          Brachionus calyciflorus           not reported        2
#> 5                Brachydanio rerio Embryo-Larval-Toxicity       10
#> 6               Ceriodaphnia dubia              Lethality        7
#> 7               Ceriodaphnia dubia              Mortality        2
#> 8               Ceriodaphnia dubia              Mortality        7
#> 9               Ceriodaphnia dubia           not reported        7
#> 10              Ceriodaphnia dubia           Reproduction        7
#> 11              Ceriodaphnia dubia           Reproduction        7
#> 12                   Daphnia magna              Mortality       21
#> 13                   Daphnia magna           Reproduction       21
#> 14         Desmodesmus subspicatus     Cell Proliferation        4
#> 15          Dunaliella tertiolecta     Cell Proliferation        4
#> 16          Dunaliella tertiolecta     Cell Proliferation        4
#> 17             Oncorhynchus mykiss Embryo-Larval-Toxicity        4
#> 18             Pimephales promelas              Mortality        4
#> 19 Pseudokirchneriella subcapitata       Wachstumshemmung        3
#> 20         Scenedesmus subspicatus                Biomass        3
#> 21         Scenedesmus subspicatus           not reported        4
#> 22         Scenedesmus subspicatus           not reported        4
#> 23         Scenedesmus subspicatus           not reported        4
#> 24         Scenedesmus subspicatus           Reproduction        3
#> 25                 Hyalella azteca              Mortality       10
#>    Time_Unit Endpoint   Value Unit
#> 1          d     NOEC   0.810     
#> 2          d     NOEC  50.000 µg/l
#> 3          d     NOEC  50.000 µg/l
#> 4          d     NOEC  50.000 µg/l
#> 5          d     NOEC 200.000 µg/l
#> 6          d     NOEC 339.000 µg/l
#> 7          d     EC50 120.000 µg/l
#> 8          d     NOEC  50.000 µg/l
#> 9          d     NOEC   4.000 µg/l
#> 10         d     NOEC   6.000 µg/l
#> 11         d     NOEC 182.000 µg/l
#> 12         d     NOEC 132.000 µg/l
#> 13         d     NOEC  40.000 µg/l
#> 14         d    ErC50   1.610 µg/l
#> 15         d     NOEC   1.600 µg/l
#> 16         d    EbC50   3.550 µg/l
#> 17         d     NOEC  34.100 µg/l
#> 18         d     LC50 260.000 µg/l
#> 19         d     NOEC   0.200 µg/l
#> 20         d     NOEC   0.500 µg/l
#> 21         d     NOEC   0.690 µg/l
#> 22         d     NOEC   0.742 µg/l
#> 23         d     NOEC   2.380 µg/l
#> 24         d     NOEC   0.500 µg/l
#> 25         d     NOEC   5.000 µg/l
```

#### PPDB

The PPDB holds a lot of chemical and ecotoxicological information.
Earlier versions of `webchem` allowed also to search and download the
database. However, this is explicitly against the terms and conditions
of use. On request we also removed all links to the database.

Therefore, `webchem` can only provide a function to parse html-code into
a R object: `parse_ppdb()`. However, no examples can be given as saving
and downloading the html source is explicitly against the terms and
conditions of use.

#### ChemIDplus

``` r
out <- ci_query(query = 'Triclosan', type = 'name', match = 'best')
out[['Triclosan']]$physprop
#>              Physical Property    Value            Units Temp (deg C)
#> 1                Melting Point       NA            deg C           NA
#> 2        log P (octanol-water) 4.76e+00           (none)           NA
#> 3             Water Solubility 1.00e+01             mg/L           20
#> 4               Vapor Pressure 6.45e-07            mm Hg           25
#> 5         Henry's Law Constant 4.99e-09      atm-m3/mole           25
#> 6 Atmospheric OH Rate Constant 1.61e-11 cm3/molecule-sec           25
#>   Source
#> 1    EXP
#> 2    EXP
#> 3    EXP
#> 4    EST
#> 5    EST
#> 6    EST
```

#### Wikidata

``` r
ids <- get_wdid(query = 'Triclosan')
ids
#>          id     match distance     query
#> 1 Q56228675 Triclosan        0 Triclosan

# quera identifiers from wikidata
wd_ident(ids$id)[1:5]
#>   smiles  cas  cid einecs csid
#> 1   <NA> <NA> <NA>   <NA> <NA>
```

#### OPSIN

``` r
opsin_query(c('Cyclopropane', 'Octane'))
#>                                                    inchi
#> Cyclopropane                InChI=1/C3H6/c1-2-3-1/h1-3H2
#> Octane       InChI=1/C8H18/c1-3-5-7-8-6-4-2/h3-8H2,1-2H3
#>                                                  stdinchi
#> Cyclopropane                InChI=1S/C3H6/c1-2-3-1/h1-3H2
#> Octane       InChI=1S/C8H18/c1-3-5-7-8-6-4-2/h3-8H2,1-2H3
#>                              stdinchikey   smiles message  status
#> Cyclopropane LVZWSLJZHVFIQJ-UHFFFAOYSA-N    C1CC1         SUCCESS
#> Octane       TVMXDCGIABBOFY-UHFFFAOYSA-N CCCCCCCC         SUCCESS
#>                     query
#> Cyclopropane Cyclopropane
#> Octane             Octane
```

#### Flavornet

``` r
fn_percept(CAS = c("75-07-0", "123-32-0"))
#>                                    75-07-0 
#>                           "pungent, ether" 
#>                                   123-32-0 
#> "cocoa, roasted nut, roast beef, medicine"
```

#### NIST

Identification of gas chromatography peaks is often aided by retention
idices. NIST provides tables of retention indices reported in the
literature organized by retention index type (Kovats, linear, normal
alkane, and Lee), column polarity, and temperature
program.

``` r
RIs <- nist_ri("78-70-6", type = "kovats", polarity = "non-polar", temp_prog = "ramp")
head(RIs)
#>       CAS      type  phase   RI length    gas substrate diameter thickness
#> 1 78-70-6 Capillary   DB-5 1098     30 Helium      <NA>     0.26      0.25
#> 2 78-70-6 Capillary   DB-1 1086     30 Helium      <NA>     0.25      0.25
#> 3 78-70-6 Capillary DB-5MS 1101     30 Helium      <NA>     0.25      0.25
#> 4 78-70-6 Capillary HP-5MS 1104     30 Helium      <NA>     0.25      0.25
#> 5 78-70-6 Capillary HP-5MS 1106     60 Helium      <NA>     0.25      0.25
#> 6 78-70-6 Capillary   DB-5 1099     30 Helium      <NA>     0.25      0.25
#>   temp_start temp_end temp_rate hold_start hold_end
#> 1         60      246         3         NA       NA
#> 2         50      280         5          3       NA
#> 3         60      180         3         NA       15
#> 4         60      280         2          8       30
#> 5         40      250         3          1       20
#> 6         60       NA         3         NA       NA
#>                                reference comment
#> 1 Adams, González Elizondo, et al., 2006 MSDC-RI
#> 2     Allegrone, Belliardo, et al., 2006 MSDC-RI
#> 3           Angioni, Barra, et al., 2006 MSDC-RI
#> 4      Hazzit, Baaliouamer, et al., 2006 MSDC-RI
#> 5   Jalali-Heravi, Zekavat, et al., 2006 MSDC-RI
#> 6      Lucero, Fredrickson, et al., 2006 MSDC-RI
```

#### ChEBI

Chemical Entities of Biological Interest (ChEBI) is a freely available
dictionary of molecular entities focused on ‘small’ chemical compounds.
`chebi_lite_entity()` returns a list of data.frames which matching query
results. The data.frames contain the **chebiid**, the
**chebiiasciiname**, a **searchscore** and **entity stars** (either 2 or
3, depending on whether the entity was checked
thoroughly).

``` r
ids <- chebi_lite_entity(c('Isoproturon', 'RZVAJINKPMORJF-UHFFFAOYSA-N'), verbose = FALSE)
ids
#> $Isoproturon
#>       chebiid           chebiasciiname searchscore entitystar
#> 1  CHEBI:6049              isoproturon        0.58          3
#> 2 CHEBI:83468 isoproturon-monodemethyl        0.29          3
#> 3 CHEBI:83514   isoproturon-didemethyl        0.29          3
#> 4 CHEBI:43405    para-isopropylaniline        0.06          3
#> 
#> $`RZVAJINKPMORJF-UHFFFAOYSA-N`
#>       chebiid chebiasciiname searchscore entitystar
#> 1 CHEBI:46195    paracetamol        0.03          3
```

The **chebiid** can then be used to query the complete ChEBI entity
using `chebi_comp_entity()`. The complete entity contains several
different data structures which are returned in a list. The data
structures are explained in greater detail at the [ChEBI
website](https://www.ebi.ac.uk/chebi/searchId.do?chebiId=CHEBI:6049).
Here, the list elements are showcased:

``` r
# ecample entities
isoproturon <- chebi_comp_entity(ids$Isoproturon$chebiid[1])
paracetamol <- chebi_comp_entity(ids$`RZVAJINKPMORJF-UHFFFAOYSA-N`$chebiid[1])

# properties: a data.frame with general properties
lapply(isoproturon, '[[', 'properties')
#> $`CHEBI:6049`
#>      chebiid chebiasciiname
#> 1 CHEBI:6049    isoproturon
#>                                                                                                                                                                                                                                                                                                                                                                                                                          definition
#> 1 A member of the class of phenylureas that is 1,1-dimethylurea substituted by a p-cumenyl group at position 3. A selective, systemic herbicide used to control annual grasses and broadleaf weeds in cereals, its use within the EU has been banned after September 2017 on the grounds of potential groundwater contamination and risks to aquatic life; there have also been concerns about its endocrine-disrupting properties.
#>    status                     smiles
#> 1 CHECKED CC(C)c1ccc(NC(=O)N(C)C)cc1
#>                                                                              inchi
#> 1 InChI=1S/C12H18N2O/c1-9(2)10-5-7-11(8-6-10)13-12(15)14(3)4/h5-9H,1-4H3,(H,13,15)
#>                      inchikey charge      mass monoisotopicmass entitystar
#> 1 PUIYMUZLKQOUOZ-UHFFFAOYSA-N      0 206.28410        206.14191          3
# chem_structure: a list of chemical structure formats (e.g. mol)
lapply(isoproturon, '[[', 'chem_structure')
#> $`CHEBI:6049`
#> $`CHEBI:6049`[[1]]
#> $`CHEBI:6049`[[1]]$structure
#> $`CHEBI:6049`[[1]]$structure[[1]]
#> [1] "\n  Mrv0541 11101412262D          \n\n 15 15  0  0  0  0            999 V2000\n    7.5478   -4.8268    0.0000 C   0  0  0  0  0  0  0  0  0  0  0  0\n    7.5478   -4.0026    0.0000 C   0  0  0  0  0  0  0  0  0  0  0  0\n    8.2674   -3.5905    0.0000 C   0  0  0  0  0  0  0  0  0  0  0  0\n    8.9830   -4.0026    0.0000 C   0  0  0  0  0  0  0  0  0  0  0  0\n    8.9830   -4.8268    0.0000 C   0  0  0  0  0  0  0  0  0  0  0  0\n    8.2674   -5.2389    0.0000 C   0  0  0  0  0  0  0  0  0  0  0  0\n    9.6957   -5.2389    0.0000 N   0  0  0  0  0  0  0  0  0  0  0  0\n   10.4117   -4.8310    0.0000 C   0  0  0  0  0  0  0  0  0  0  0  0\n   11.1278   -5.2431    0.0000 N   0  0  0  0  0  0  0  0  0  0  0  0\n   11.8396   -4.8352    0.0000 C   0  0  0  0  0  0  0  0  0  0  0  0\n   10.4076   -4.0068    0.0000 O   0  0  0  0  0  0  0  0  0  0  0  0\n   11.1236   -6.0632    0.0000 C   0  0  0  0  0  0  0  0  0  0  0  0\n    6.8318   -3.5947    0.0000 C   0  0  0  0  0  0  0  0  0  0  0  0\n    6.8318   -2.7746    0.0000 C   0  0  0  0  0  0  0  0  0  0  0  0\n    6.1157   -4.0068    0.0000 C   0  0  0  0  0  0  0  0  0  0  0  0\n  1  2  2  0  0  0  0\n  7  8  1  0  0  0  0\n  2  3  1  0  0  0  0\n  8  9  1  0  0  0  0\n  3  4  2  0  0  0  0\n  9 10  1  0  0  0  0\n  4  5  1  0  0  0  0\n  8 11  2  0  0  0  0\n  5  6  2  0  0  0  0\n  9 12  1  0  0  0  0\n  6  1  1  0  0  0  0\n  2 13  1  0  0  0  0\n 13 14  1  0  0  0  0\n  5  7  1  0  0  0  0\n 13 15  1  0  0  0  0\nM  END\n"
#> 
#> 
#> $`CHEBI:6049`[[1]]$type
#> $`CHEBI:6049`[[1]]$type[[1]]
#> [1] "mol"
#> 
#> 
#> $`CHEBI:6049`[[1]]$dimension
#> $`CHEBI:6049`[[1]]$dimension[[1]]
#> [1] "2D"
#> 
#> 
#> $`CHEBI:6049`[[1]]$defaultStructure
#> $`CHEBI:6049`[[1]]$defaultStructure[[1]]
#> [1] "true"
# synonyms: a data.frame of synonyms (collected from different sources)
lapply(isoproturon, '[[', 'synonyms')
#> $`CHEBI:6049`
#>                                      data    type                 source
#> 1 1,1-dimethyl-3-(p-isopropylphenyl)-urea SYNONYM NIST Chemistry WebBook
#> 2            3-p-cumenyl-1,1-dimethylurea SYNONYM Alan Wood's Pesticides
#> 3    N-4-isopropylphenyl-N,N-dimethylurea SYNONYM NIST Chemistry WebBook
# iupacnames: a data.frame of IUPAC names (collected from different sources)
lapply(isoproturon, '[[', 'iupacnames')
#> $`CHEBI:6049`
#>                                         data       type source
#> 1 1,1-dimethyl-3-[4-(propan-2-yl)phenyl]urea IUPAC NAME  IUPAC
# formulae: a data.frame of chemical formulae (collected from different sources)
lapply(isoproturon, '[[', 'formulae')
#> $`CHEBI:6049`
#>        data source
#> 1 C12H18N2O  ChEBI
# regnumbers: a data.frame of registry numbers (e.g. CAS, Beilstein, Reaxys) (collected from different sources)
lapply(isoproturon, '[[', 'regnumbers')
#> $`CHEBI:6049`
#>         data                   type                 source
#> 1    2214033 Reaxys Registry Number                 Reaxys
#> 2 34123-59-6    CAS Registry Number          KEGG COMPOUND
#> 3 34123-59-6    CAS Registry Number NIST Chemistry WebBook
#> 4 34123-59-6    CAS Registry Number             ChemIDplus
# chebiid_snd: a data.frame with secondary ChEBI ids
lapply(paracetamol, '[[', 'chebiid_snd')
#> $`CHEBI:46195`
#>      chebiids
#> 1 CHEBI:46191
#> 2  CHEBI:2386
# citations: Publications which cite the entity along with hyperlinks to the PubMed entry via Europe PMC
head(lapply(paracetamol, '[[', 'citations')[[1]])
#>       data            type     source
#> 1 11084378 PubMed citation Europe PMC
#> 2 11304127 PubMed citation Europe PMC
#> 3 16716555 PubMed citation Europe PMC
#> 4 18953082 PubMed citation Europe PMC
#> 5 21108564 PubMed citation Europe PMC
#> 6 22770225 PubMed citation Europe PMC
# parents: parent ontologies of the entity
lapply(paracetamol, '[[', 'parents')
#> $`CHEBI:46195`
#>                               chebiName     chebiId                  type
#> 1            cyclooxygenase 2 inhibitor CHEBI:50629              has role
#> 2            cyclooxygenase 1 inhibitor CHEBI:50630              has role
#> 3                non-narcotic analgesic CHEBI:35481              has role
#> 4                           antipyretic CHEBI:35493              has role
#> 5  non-steroidal anti-inflammatory drug CHEBI:35475              has role
#> 6                               phenols CHEBI:33853                  is a
#> 7                         4-aminophenol CHEBI:17602 has functional parent
#> 8                            xenobiotic CHEBI:35703              has role
#> 9                     hepatotoxic agent CHEBI:50908              has role
#> 10         human blood serum metabolite CHEBI:85234              has role
#> 11           cyclooxygenase 3 inhibitor CHEBI:73263              has role
#> 12            environmental contaminant CHEBI:78298              has role
#> 13                           acetamides CHEBI:22160                  is a
#>     status cyclicRelationship
#> 1  CHECKED              false
#> 2  CHECKED              false
#> 3  CHECKED              false
#> 4  CHECKED              false
#> 5  CHECKED              false
#> 6  CHECKED              false
#> 7  CHECKED              false
#> 8  CHECKED              false
#> 9  CHECKED              false
#> 10 CHECKED              false
#> 11 CHECKED              false
#> 12 CHECKED              false
#> 13 CHECKED              false
# children: child ontologies of the entity
lapply(paracetamol, '[[', 'children')
#> $`CHEBI:46195`
#>                                              chebiName      chebiId
#> 1                  acetaminophen glutathione conjugate  CHEBI:32639
#> 2                   2-methoxyacetaminophen glucuronide CHEBI:133005
#> 3  S-(5-acetamido-2-hydroxyphenyl)-N-acetyl-L-cysteine CHEBI:133435
#> 4                                           methacetin CHEBI:139354
#> 5                                           phenacetin   CHEBI:8050
#> 6              S-(5-acetamido-2-hydroxyphenyl)cysteine CHEBI:133066
#> 7           acetaminophen O-beta-D-glucosiduronic acid  CHEBI:32636
#> 8                                 3-nitroacetaminophen CHEBI:139475
#> 9                             3-nitroacetaminophen-TMS CHEBI:139476
#> 10                                 paracetamol sulfate  CHEBI:32635
#>                     type  status cyclicRelationship
#> 1  has functional parent CHECKED              false
#> 2  has functional parent CHECKED              false
#> 3  has functional parent CHECKED              false
#> 4  has functional parent CHECKED              false
#> 5  has functional parent CHECKED              false
#> 6  has functional parent CHECKED              false
#> 7  has functional parent CHECKED              false
#> 8  has functional parent CHECKED              false
#> 9  has functional parent CHECKED              false
#> 10 has functional parent CHECKED              false
# dblinks: Links to other data bases
lapply(paracetamol, '[[', 'dblinks')
#> $`CHEBI:46195`
#>            data                    type
#> 1            52  Drug Central accession
#> 2 Acetaminophen     Wikipedia accession
#> 3        C06804 KEGG COMPOUND accession
#> 4      CPD-7669       MetaCyc accession
#> 5        D00217     KEGG DRUG accession
#> 6       DB00316      DrugBank accession
#> 7   HMDB0001859          HMDB accession
#> 8      LSM-5533         LINCS accession
#> 9           TYL      PDBeChem accession
# comments: General comment(s)
lapply(paracetamol, '[[', 'comments')
#> $`CHEBI:46195`
#>                                                                             text
#> 1 Stravs M, Schymanski E, Singer H, Department of Environmental Chemistry, Eawag
#>         date
#> 1 2014-10-29
# Metabolites of Species
head(lapply(paracetamol, '[[', 'origins')[[1]])
#>    speciesText speciesAccession   SourceType SourceAccession
#> 1 Mus musculus   NCBI:txid10090 MetaboLights        MTBLS292
#> 2 Homo sapiens    NCBI:txid9606    PubMed Id        19309105
#> 3 Homo sapiens    NCBI:txid9606    PubMed Id        18502700
#> 4 Homo sapiens    NCBI:txid9606    PubMed Id        12097436
#> 5 Homo sapiens    NCBI:txid9606    PubMed Id        21359215
#> 6 Homo sapiens    NCBI:txid9606 MetaboLights         MTBLS90
#>         componentText componentAccession
#> 1                <NA>               <NA>
#> 2               urine        BTO:0001419
#> 3 cerebrospinal fluid     UBERON:0001359
#> 4              saliva     UBERON:0001836
#> 5               blood     UBERON:0000178
#> 6         blood serum        BTO:0000133
```

#### Extractor functions

The sources provide a lot of informations that can be retrieved using
the functions described above. Often only specific inforamtion is
needed. Therefore, we added extractor functions for common identifiers.

``` r
wi <- wd_ident("Q408646")
wi
#>                                   smiles       cas  cid    einecs csid
#> 1 C1=CC(=C(C=C1Cl)O)OC2=C(C=C(C=C2)Cl)Cl 3380-34-5 5564 222-182-2 5363
#>                                                                    inchi
#> 1 1S/C12H7Cl3O2/c13-7-1-3-11(9(15)5-7)17-12-4-2-8(14)6-10(12)16/h1-6,16H
#>                      inchikey drugbank    zvg  chebi    chembl       unii
#> 1 XEFQLINVKFYRCS-UHFFFAOYSA-N    08604 490400 164200 CHEMBL849 4NM5039Y5X
#>                              source_url   query
#> 1 https://www.wikidata.org/wiki/Q408646 Q408646
cas(wi)
#> [1] "3380-34-5"
inchikey(wi)
#> [1] "XEFQLINVKFYRCS-UHFFFAOYSA-N"
smiles(wi)
#> [1] "C1=CC(=C(C=C1Cl)O)OC2=C(C=C(C=C2)Cl)Cl"

# smiles(etox_basic(5564))
```

#### Misc functions

##### Check if a string is a valid CAS registry number

``` r
is.cas('64-17-5')
#> [1] TRUE
is.cas('64-17-6')
#> [1] FALSE
```

##### Check if a string is a valid InChIKey

Using a pure R implementation:

``` r
is.inchikey('BQJCRHHNABKAKU-KBQPJGBKSA-N')
#> [1] TRUE
is.inchikey('BQJCRHHNABKAKU-KBQPJGBKXA-N')
#> [1] FALSE
```

Using the ChemSpider API

``` r
is.inchikey('BQJCRHHNABKAKU-KBQPJGBKSA-N', type = 'chemspider')
#> [1] TRUE
is.inchikey('BQJCRHHNABKAKU-KBQPJGBKXA-N', type = 'chemspider')
#> [1] FALSE
```

##### Check if a string is a valid SMILES

``` r
is.smiles('Clc(c(Cl)c(Cl)c1C(=O)O)c(Cl)c1Cl')
# 'J' is not found in the periodic table
is.smiles('Clc(c(Cl)c(Cl)c1C(=O)O)c(Cl)c1ClJ')
```

### Acknowledgements

Without the fantastic web services `webchem` wouldn’t be here.
Therefore, kudos to the web service providers and developers\!

### Related Projects

If you’re more familiar with Python you should check out [Matt
Swains](https://github.com/mcs07) repositories:
[ChemSpiPy](https://github.com/mcs07/ChemSpiPy),
[PubChemPy](https://github.com/mcs07/PubChemPy) and
[CirPy](https://github.com/mcs07/CIRpy) provide similar functionality as
`webchem`.

### Contributors

  - [Eduard Szöcs](https://github.com/EDiLD)
  - [Daniel
    Münch](https://github.com/ropensci/webchem/commits?author=Dahaniel)
  - [Johannes
    Ranke](https://github.com/ropensci/webchem/commits?author=jranke)
  - [Eric R
    Scott](https://github.com/ropensci/webchem/commits?author=Aariq)

### Want to contribute?

Check out our [contribution guide
here](https://github.com/ropensci/webchem/blob/master/CONTRIBUTING.md).

### Meta

  - Please [report any issues, bugs or feature
    requests](https://github.com/ropensci/webchem/issues).
  - License: MIT
  - Get citation information for `webchem` in R doing
    `citation("webchem")`

[![ropensci](http://ropensci.org/public_images/github_footer.png)](http://ropensci.org)
