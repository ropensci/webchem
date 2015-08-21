webchem
=============



[![Build Status](https://travis-ci.org/ropensci/webchem.png)](https://travis-ci.org/ropensci/webchem)
[![Build status](https://ci.appveyor.com/api/projects/status/e3sa6e918jlemv46/branch/master)](https://ci.appveyor.com/project/EDiLD/webchem)
[![Coverage Status](https://coveralls.io/repos/ropensci/webchem/badge.svg?branch=master)](https://coveralls.io/r/ropensci/webchem?branch=master)
[![Open Issues](https://img.shields.io/github/issues/ropensci/webchem.svg)](https://github.com/ropensci/webchem/issues)
[![Downloads](http://cranlogs.r-pkg.org/badges/webchem)](http://cranlogs.r-pkg.org/badges/webchem)
[![CRAN_Status_Badge](http://www.r-pkg.org/badges/version/webchem)](http://cran.r-project.org/web/packages/webchem)

`webchem` is a R package to retrieve chemical information from  the web. 
This package interacts with a suite of web APIs to retrieve chemical information.


## Currently implemented in `webchem`

Source | Function(s | API Docs | API key
------ | --------- | -------- | --------
[Chemical Identifier Resolver (CIR)](http://cactus.nci.nih.gov/chemical/structure) | `cir_query()` | [link](http://cactus.nci.nih.gov/chemical/structure_documentation) | none
[ChemSpider](http://www.chemspider.com/) | `get_csid()`, `csid_compinfo()`, `csid_extcompinfo()` | [link](http://www.chemspider.com/AboutServices.aspx?) | required [(link)](https://www.rsc.org/rsc-id/register )
PubChem | `get_cid()`, `cid_compinfo()` | [link](https://pubchem.ncbi.nlm.nih.gov/) | none
[Chemical Translation Service (CTS)](http://cts.fiehnlab.ucdavis.edu/) | `cts_convert()`, `cts_compinfo()` | none | none
[PAN Pesticide Database](http://www.pesticideinfo.org/) | `pan()` | none | none
[Allan Wood's Compendium of Pesticide Common Names](http://www.alanwood.net/pesticides/) | `allanwood()` | none | none
[PHYSPROP Database](http://www.srcinc.com/what-we-do/environmental/scientific-databases.html) | `physprop()` | none | none
[ETOX](http://webetox.uba.de/webETOX/index.do) | `get_etoxid()`, `etox_basic()`. `etox_targets()` | none | none

#### API keys
ChemSpider functions require a security token. 
Please register at RSC (https://www.rsc.org/rsc-id/register) to retrieve a security token.

## Installation
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


## Quickstart

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

Query the number of rings using the InChiKey (Triclosan) 

```r
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
#>  [1] "5564"     "131203"   "627458"   "15942656" "16220126" "16220128"
#>  [7] "16220129" "16220130" "18413505" "22947105" "23656593" "24848164"
#> [13] "25023954" "25023955" "25023956" "25023957" "25023958" "25023959"
#> [19] "25023960" "25023961" "25023962" "25023963" "25023964" "25023965"
#> [25] "25023966" "25023967" "25023968" "25023969" "25023970" "25023971"
#> [31] "25023972" "25023973" "45040608" "45040609" "67606151" "71752714"
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



#### Allan Wood's Compendium of Pesticide Common Names

`allanwood()` returns a list of 9 entries and can query common names and cas numbers:

```r
allanwood('Fluazinam', type = 'commonname')
#> $cname
#> [1] "Fluazinam"
#> 
#> $status
#> [1] "ISO 1750 (published)"
#> 
#> $pref_iupac_name
#> [1] "3-chloro-N-[3-chloro-2,6-dinitro-4-(trifluoromethyl)phenyl]-5-(trifluoromethyl)pyridin-2-amine"
#> 
#> $iupac_name
#> [1] "3-chloro-N-(3-chloro-5-trifluoromethyl-2-pyridyl)-α,α,α-trifluoro-2,6-dinitro-p-toluidine"
#> 
#> $cas
#> [1] "79622-59-6"
#> 
#> $formula
#> [1] "C13H4Cl2F6N4O4"
#> 
#> $activity
#> [1] "fungicides (pyridine fungicides)"
#> 
#> $inchikey
#> [1] "UZCGKGPEKUCDTF-UHFFFAOYSA-N"
#> 
#> $inch
#> [1] "InChI=1S/C13H4Cl2F6N4O4/c14-6-1-4(12(16,17)18)3-22-11(6)23-9-7(24(26)27)2-5(13(19,20)21)8(15)10(9)25(28)29/h1-3H,(H,22,23)"
allanwood('79622-59-6', type = 'cas')$cname
#> [1] "fluazinam"
```

#### SRC PHYSPROP Database
[SRCs PHYSPROP Database](http://www.srcinc.com/what-we-do/environmental/scientific-databases.html) contains chemical structures, names and physical properties for over 41,000 chemicals.
You can use `physprop()` to query this database using a CAS number:


```r
physprop('50-00-0')
#> $cas
#> [1] "50-00-0"
#> 
#> $cname
#> [1] "FORMALDEHYDE"
#> 
#> $mw
#> [1] "30.026"
#> 
#> $prop
#>                       variable            value             unit     temp
#> 1             Water Solubility           400000             mg/L 20 deg C
#> 2        Log P (octanol-water)             0.35                  25 deg C
#> 3               Vapor Pressure             3886            mm Hg 25 deg C
#> 4    pKa Dissociation Constant            13.27                  25 deg C
#> 5         Henry's Law Constant      0.000000337       atm-m3/mol 25 deg C
#> 6 Atmospheric OH Rate Constant 0.00000000000937 cm3/molecule-sec 25 deg C
#>   type                              ref
#> 1  EXP        PICKRELL,JA ET AL. (1983)
#> 2  EXP           HANSCH,C ET AL. (1995)
#> 3  EXT          BOUBLIK,T ET AL. (1984)
#> 4  EXP   SERJEANT,EP & DEMPSEY,B (1979)
#> 5  EXP BETTERTON,EA & HOFFMAN,MR (1988)
#> 6  EXP     KWOK,ESC & ATKINSON,R (1994)
```


#### ETOX
ETOX: Information System Ecotoxicology and Environmental Quality Targets is a database run by the Federal Environment Agency of Germany and provides data on synonyms, identifiers, Quality Targest and Effects.

First we need to query a substance ID:


```r
id <- get_etoxid('Triclosan')
id
#> [1] "20179"
#> attr(,"matched")
#> [1] "Triclosan ( 20179 )"
#> attr(,"distance")
#> [1] 0.5263158
```
`get_etoxid` tries to find the best match for you (check the matched and distance attributes))

With this substance ID we can query further information from ETOX, e.g.:


```r
etox_basic(id)
#> $cas
#> [1] "3380-34-5"
#> 
#> $ec
#> [1] "222-182-2"
#> 
#> $gsbl
#> [1] "117338"
#> 
#> $synonyms
#>                                          name  language
#> 5      5-chloro-2-(2,4-dichlorophenoxy)phenol   English
#> 8   Phenol, 5-chloro-2-(2,4-dichlorophenoxy)-    German
#> 9     2,4,4'-Trichlor-2'-hydroxydiphenylether    German
#> 10                             Irgasan DP 300    German
#> 11                                  Vikol THP    German
#> 12     2,4,4-Trichlor-2'-hydroxydiphenylether    German
#> 13   2,4,4'-Trichloro-2'-hydroxydiphenylether    German
#> 15     Chlor-2-(2,4-dichlorphenoxy)phenol, 5- universal
#> 16  Trichlor-2'-hydroxydiphenylether, 2,4,4'- universal
#> 17   Trichlor-2'-hydroxydiphenylether, 2,4,4- universal
#> 18 Trichloro-2'-hydroxydiphenylether, 2,4,4'- universal
#> 19      5-Chlor-2-(2,4-dichlorphenoxy)-phenol universal
#> 20    Chlor-2-(2,4-dichlorphenoxy)-phenol, 5- universal
#> 21       5-Chlor-2-(2,4-dichlorphenoxy)phenol universal
#> 22                                  triclosán   Spanish
#> 23                                triklosaani   Finnish
#> 24                                 triclosano   Italian
#> 25                                  triklosan   Swedish
```

Which returns CAS, EC and GSBL numbers, as well as a synonym list.

We can also retrieve Quality Targets:


```r
targets <- etox_targets(id)
targets[ , c('Substance', 'Country_or_Region', 'Designation', 'Value_Target_LR', 'Unit')]
#>   Substance Country_or_Region      Designation Value_Target_LR Unit
#> 1 Triclosan               AUS             PNEC           0.050 µg/l
#> 2 Triclosan               CHE AA-QS_freshwater           0.020 µg/l
#> 3 Triclosan               CHE           MAC-QS           0.020 µg/l
#> 4 Triclosan               DEU           AA-EQS           0.020 µg/l
#> 5 Triclosan               DEU          MAC-EQS           0.200 µg/l
#> 6 Triclosan               DEU       QS_fw, eco           0.020 µg/l
#> 7 Triclosan               DEU   MAC-QS_fw, eco           0.160 µg/l
#> 8 Triclosan               DEU       QS_sw, eco           0.002 µg/l
#> 9 Triclosan               DEU   MAC-QS_sw, eco           0.016 µg/l
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

* Please [report any issues, bugs or feature requests](https://github.com/ropensci/webchem/issues).
* License: MIT

[![ropensci](http://ropensci.org/public_images/github_footer.png)](http://ropensci.org)
