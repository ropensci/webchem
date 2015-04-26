webchem
=============



[![Build Status](https://travis-ci.org/ropensci/webchem.png)](https://travis-ci.org/ropensci/webchem)
[![Build status](https://ci.appveyor.com/api/projects/status/e3sa6e918jlemv46/branch/master)](https://ci.appveyor.com/project/EDiLD/webchem)
[![Coverage Status](https://coveralls.io/repos/ropensci/webchem/badge.svg?branch=master)](https://coveralls.io/r/ropensci/webchem?branch=master)
[![Open Issues](https://img.shields.io/github/issues/ropensci/webchem.svg)](https://github.com/ropensci/webchem/issues)
[![Downloads](http://cranlogs.r-pkg.org/badges/webchem)](http://cranlogs.r-pkg.org/badges/webchem)

`webchem` is a R package to retrieve chemical information from  the web. 
This package interacts with a suite of web APIs to retrieve chemical information.


### Currently implemented in `webchem`

Source | Function(s | API Docs | API key
------ | --------- | -------- | --------
Chemical Identifier Resolver (CIR) | `cir_query()` | [link](http://cactus.nci.nih.gov/chemical/structure_documentation) | none
ChemSpider | `get_csid()`, `csid_compinfo()`, `csid_extcompinfo()` | [link](http://www.chemspider.com/AboutServices.aspx?) | required [(link)](https://www.rsc.org/rsc-id/register )
PubChem | `get_cid()`, `cid_compinfo()` | [link](https://pubchem.ncbi.nlm.nih.gov/) | none
Chemical Translation Service (CTS) | `cts_convert()`, `cts_compinfo()` | [link](http://cts.fiehnlab.ucdavis.edu/) | none
PAN Pesticide Database | `pan()` | [link](http://www.pesticideinfo.org/) | none

#### API keys
ChemSpider functions require a security token. 
Please register at RSC (https://www.rsc.org/rsc-id/register) to retrieve a security token.

### Installation
#### Install from CRAN (stable version)

```r
install.packages("webchem")
```


#### Install from Github (development version)

```r
install.packages("devtools")
library("devtools")
install_github("ropensci/webchem")
```

### Quickstart


```r
library("webchem")
```

#### Chemical Identifier Resolver (CIR)

CAS numbers and molecular weight for [Triclosan](http://en.wikipedia.org/wiki/Triclosan).
Use `first` to return only the first hit.

```r
cir_query('Triclosan', 'cas')
#> [1] "3380-34-5"   "112099-35-1" "88032-08-0"
cir_query('Triclosan', 'cas', first = TRUE)
#> [1] "3380-34-5"
cir_query('Triclosan', 'mw')
#> [1] "289.5451"
```

Query SMILES and InChIKey from CAS (Triclosan).
Inputs might by ambiguous and we can specify where to search using `resolver=`.

```r
cir_query('3380-34-5', 'smiles')
#> [1] "C1=CC(=CC(=C1OC2=CC=C(C=C2Cl)Cl)O)Cl"
cir_query('3380-34-5', 'stdinchikey', resolver = 'cas_number')
#> [1] "InChIKey=XEFQLINVKFYRCS-UHFFFAOYSA-N"
```

Convert InChiKey (Triclosan) to ChemSpider ID and retrieve the number of rings

```r
cir_query('XEFQLINVKFYRCS-UHFFFAOYSA-N', 'chemspider_id', first = TRUE)
#> [1] "5363"
cir_query('XEFQLINVKFYRCS-UHFFFAOYSA-N', 'ring_count')
#> [1] "2"
```


#### ChemSpider


You'll need a API key:

```r
token = '<YOUR TOKEN HERE'
```

Retrieve the ChemSpider ID of Triclosan

```r
(id <- get_csid('Triclosan', token = token))
#> [1] "5363"
```

Use this ID to query information from ChemSpider

```r
csid_extcompinfo(id, token = token)
#>                                                                          CSID 
#>                                                                        "5363" 
#>                                                                            MF 
#>                                                      "C_{12}H_{7}Cl_{3}O_{2}" 
#>                                                                        SMILES 
#>                                              "c1cc(c(cc1Cl)O)Oc2ccc(cc2Cl)Cl" 
#>                                                                         InChI 
#> "InChI=1/C12H7Cl3O2/c13-7-1-3-11(9(15)5-7)17-12-4-2-8(14)6-10(12)16/h1-6,16H" 
#>                                                                      InChIKey 
#>                                                   "XEFQLINVKFYRCS-UHFFFAOYAS" 
#>                                                                   AverageMass 
#>                                                                    "289.5418" 
#>                                                               MolecularWeight 
#>                                                                    "289.5418" 
#>                                                              MonoisotopicMass 
#>                                                                  "287.951172" 
#>                                                                   NominalMass 
#>                                                                         "288" 
#>                                                                         ALogP 
#>                                                                        "5.53" 
#>                                                                         XLogP 
#>                                                                           "5" 
#>                                                                    CommonName 
#>                                                                   "Triclosan"
```


#### PubChem

Retrieve PubChem CID

```r
get_cid('Triclosan')
#>  [1] "4093"     "5564"     "13190"    "131203"   "627458"   "15942656"
#>  [7] "16220126" "16220128" "16220129" "16220130" "18413505" "22947105"
#> [13] "23656593" "24848164" "25023954" "25023955" "25023956" "25023957"
#> [19] "25023958" "25023959" "25023960" "25023961" "25023962" "25023963"
#> [25] "25023964" "25023965" "25023966" "25023967" "25023968" "25023969"
#> [31] "25023970" "25023971" "25023972" "25023973" "45040608" "45040609"
#> [37] "67606151" "71752714"
cid <- get_cid('3380-34-5')
```

Use this CID to retrieve some chemical properties:

```r
props <- cid_compinfo(cid)
props$InChIKey
#> [1] "XEFQLINVKFYRCS-UHFFFAOYSA-N"
props$MolecularWeight
#> [1] "289.541780"
props$IUPACName
#> [1] "5-chloro-2-(2,4-dichlorophenoxy)phenol"
```


#### Chemical Translation Service (CTS)

CTS allows to convert from nearly every possible identifier to nearly every possible identifier:

```r
cts_convert(query = '3380-34-5', from = 'CAS', to = 'PubChem CID')
#> [1] "5564"
cts_convert(query = '3380-34-5', from = 'CAS', to = 'ChemSpider')
#> [1] "5363"
(inchk <- cts_convert(query = 'Triclosan', from = 'Chemical Name', to = 'inchikey'))
#> [1] "XEFQLINVKFYRCS-UHFFFAOYSA-N"
```

Moreover, we can a lot of information stored in the CTS database using InChIkey

```r
info <- cts_compinfo(inchikey = inchk)
info[1:5]
#> $inchikey
#> [1] "XEFQLINVKFYRCS-UHFFFAOYSA-N"
#> 
#> $inchicode
#> [1] "InChI=1S/C12H7Cl3O2/c13-7-1-3-11(9(15)5-7)17-12-4-2-8(14)6-10(12)16/h1-6,16H"
#> 
#> $molweight
#> [1] 289.5418
#> 
#> $exactmass
#> [1] 287.9512
#> 
#> $formula
#> [1] "C12H7Cl3O2"
```



#### PAN Pesticide Database
`pan()` returns a list of 73 entries, here I extract only 4 of those:

```r
pan_list <- pan('lambda-Cyhalothrin', first = TRUE)
pan_list[c("CAS Number", "Chemical Class", "Water Solubility (Avg, mg/L)", "Adsorption Coefficient (Koc)" )]
#> $`CAS Number`
#> [1] "91465-08-6"
#> 
#> $`Chemical Class`
#> [1] "Pyrethroid"
#> 
#> $`Water Solubility (Avg, mg/L)`
#> [1] "0.0050"
#> 
#> $`Adsorption Coefficient (Koc)`
#> [1] "157000"
```



### Acknowledgements
Without the fantastic web services `webchem` wouldn't be here.
Therefore, kudos to the web service providers and developers!


### Related Projects
If you're more familiar with Python you should check out [Matt Swains](https://github.com/mcs07) repositories: [ChemSpiPy](https://github.com/mcs07/ChemSpiPy), [PubChemPy](https://github.com/mcs07/PubChemPy) and [CirPy](https://github.com/mcs07/CIRpy) provide similar functionality as `webchem`.


### Contributors

+ [Eduard Szöcs](https://github.com/EDiLD)
+ [Daniel Münch](https://github.com/Dahaniel)

### Meta

* Please [report any issues, bugs or feature requests](https://github.com/edild/webchem/issues).
* License: MIT

[![ropensci](http://ropensci.org/public_images/github_footer.png)](http://ropensci.org)
