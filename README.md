webchem
=============



[![Build Status](https://travis-ci.org/ropensci/webchem.png)](https://travis-ci.org/ropensci/webchem)
[![Build status](https://ci.appveyor.com/api/projects/status/e3sa6e918jlemv46/branch/master)](https://ci.appveyor.com/project/EDiLD/webchem)
[![Coverage Status](https://codecov.io/github/ropensci/webchem/coverage.svg?branch=tests)](https://codecov.io/github/ropensci/webchem?branch=tests)
<!--
[![Coverage Status](https://coveralls.io/repos/ropensci/webchem/badge.svg?branch=master)](https://coveralls.io/r/ropensci/webchem?branch=master)
-->
[![Open Issues](https://img.shields.io/github/issues/ropensci/webchem.svg)](https://github.com/ropensci/webchem/issues)
[![Downloads](http://cranlogs.r-pkg.org/badges/webchem)](http://cranlogs.r-pkg.org/badges/webchem)
[![CRAN_Status_Badge](http://www.r-pkg.org/badges/version/webchem)](http://cran.r-project.org/web/packages/webchem)
[![DOI](https://zenodo.org/badge/17223/ropensci/webchem.svg)](https://zenodo.org/badge/latestdoi/17223/ropensci/webchem)

`webchem` is a R package to retrieve chemical information from  the web. 
This package interacts with a suite of web APIs to retrieve chemical information.

The functions in the package that hit a specific API have a prefix and suffix separated by an underscore (`prefix_suffix()`)
They follow the format of `source_functionality`, e.g.`cs_compinfo` uses ChemSpider to retrieve compound informations.


## Currently implemented in `webchem`

Source | Function(s) | API Docs | API key
------ | --------- | -------- | --------
[Chemical Identifier Resolver (CIR)](http://cactus.nci.nih.gov/chemical/structure) | `cir_query()` | [link](http://cactus.nci.nih.gov/chemical/structure_documentation) | none
[ChemSpider](http://www.chemspider.com/) | `get_csid()`, `cs_compinfo()`, `cs_extcompinfo()` , `cs_convert()`, `cs_prop()`| [link](http://www.chemspider.com/AboutServices.aspx?) | required [(link)](https://www.rsc.org/rsc-id/register )
[PubChem](https://pubchem.ncbi.nlm.nih.gov/) | `get_cid()`, `pc_prop()`, `pc_synonyms()` | [link](https://pubchem.ncbi.nlm.nih.gov/) | none
[Chemical Translation Service (CTS)](http://cts.fiehnlab.ucdavis.edu/) | `cts_convert()`, `cts_compinfo()` | none | none
[PAN Pesticide Database](http://www.pesticideinfo.org/) | `pan_query()` | none | none
[Alan Wood's Compendium of Pesticide Common Names](http://www.alanwood.net/pesticides/) | `aw_query()` | none | none
[PHYSPROP Database](http://www.srcinc.com/what-we-do/environmental/scientific-databases.html) | `pp_query()` | none | none
[ETOX](http://webetox.uba.de/webETOX/index.do) | `get_etoxid()`, `etox_basic()`. `etox_targets()`, `etox_tests()` | none | none
PPDB | `ppdb_parse()` (only parsing) | none | none
[ChemIDplus](http://chem.sis.nlm.nih.gov/chemidplus/) | `ci_query()` | none | none
[Wikidata](https://www.wikidata.org/wiki/Wikidata:WikiProject_Chemistry) | `get_wdid()`, `wd_ident()` | [link](https://www.mediawiki.org/wiki/API:Main_page) | none

Moreover, there are some functions to check indentifiers: `is.inchikey()`, `is.cas()` and `is.smiles()`.

#### API keys

Some ChemSpider functions require a security token. 
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
#> $Triclosan
#> [1] NA
cir_query('Triclosan', 'cas', first = TRUE)
#> $Triclosan
#> [1] "3380-34-5"
cir_query('Triclosan', 'mw')
#> $Triclosan
#> [1] 289.5451
```

Query SMILES and InChIKey from CAS (Triclosan).
Inputs might by ambiguous and we can specify where to search using `resolver=`.

```r
cir_query('3380-34-5', 'smiles')
#> $`3380-34-5`
#> [1] "Oc1cc(Cl)ccc1Oc2ccc(Cl)cc2Cl"
cir_query('3380-34-5', 'stdinchikey', resolver = 'cas_number')
#> $`3380-34-5`
#> [1] "InChIKey=XEFQLINVKFYRCS-UHFFFAOYSA-N"
```

Query the number of rings using the InChiKey (Triclosan) 

```r
cir_query('XEFQLINVKFYRCS-UHFFFAOYSA-N', 'ring_count')
#> $`XEFQLINVKFYRCS-UHFFFAOYSA-N`
#> [1] 2
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
cs_extcompinfo(id, token = token)
#> $csid
#> [1] "5363"
#> 
#> $mf
#> [1] "C_{12}H_{7}Cl_{3}O_{2}"
#> 
#> $smiles
#> [1] "c1cc(c(cc1Cl)O)Oc2ccc(cc2Cl)Cl"
#> 
#> $inchi
#> [1] "InChI=1/C12H7Cl3O2/c13-7-1-3-11(9(15)5-7)17-12-4-2-8(14)6-10(12)16/h1-6,16H"
#> 
#> $inchikey
#> [1] "XEFQLINVKFYRCS-UHFFFAOYAS"
#> 
#> $average_mass
#> [1] 289.5418
#> 
#> $mw
#> [1] 289.5418
#> 
#> $monoiso_mass
#> [1] 287.9512
#> 
#> $nominal_mass
#> [1] 288
#> 
#> $alogp
#> [1] 5.53
#> 
#> $xlogp
#> [1] 5
#> 
#> $common_name
#> [1] "Triclosan"
#> 
#> $source_url
#> [1] "http://www.chemspider.com/Chemical-Structure.5363.html"
```

Note that the URL of the source if also returned (`source_url`) and can be used for (micro-)attribution.

Or to convert to a Mol-Object


```r
mol <- cs_convert(id, from = 'csid', to = 'mol', token = token)
head(mol$ab)
#>         x      y z  a d c s h b v H m n e NA NA
#> 1 -1.7350 2.0001 0 Cl 0 0 0 0 0 0 0 0 0 0  0  0
#> 2 -0.8675 1.5027 0  C 0 0 0 0 0 0 0 0 0 0  0  0
#> 3 -0.8675 0.4975 0  C 0 0 0 0 0 0 0 0 0 0  0  0
#> 4  0.0000 0.0000 0  C 0 0 0 0 0 0 0 0 0 0  0  0
#> 5  0.8675 0.4975 0  C 0 0 0 0 0 0 0 0 0 0  0  0
#> 6  0.8675 1.5027 0  C 0 0 0 0 0 0 0 0 0 0  0  0
```
Note that the Molfile is parsed into a R object (via `parse_mol()`) and that a API-key is needed


`cs_convert()` handles a lot of input / output formats, even without API-key:


```r
cs_convert('XEFQLINVKFYRCS-UHFFFAOYAS', from = 'inchikey', to = 'csid')
#> [1] "5363"
cs_convert('XEFQLINVKFYRCS-UHFFFAOYAS', from = 'inchikey', to = 'inchi')
#> [1] "InChI=1/C12H7Cl3O2/c13-7-1-3-11(9(15)5-7)17-12-4-2-8(14)6-10(12)16/h1-6,16H"
cs_convert('c1cc(c(cc1Cl)O)Oc2ccc(cc2Cl)Cl', from = 'smiles', to = 'inchi')
#> [1] "InChI=1S/C12H7Cl3O2/c13-7-1-3-11(9(15)5-7)17-12-4-2-8(14)6-10(12)16/h1-6,16H"
```

And get EPISuit predictions from ChemSpider


```r
cs_prop('5363')$epi[ , c(1:4)]
#>                                               prop value_pred
#> 1                 Log Octanol-Water Partition Coef     4.6600
#> 2                                    Boiling Point   373.6200
#> 3                                    Melting Point   136.7900
#> 4                        Water Solubility from KOW     4.6210
#> 5                  Water Solubility from Fragments     9.2998
#> 6 Log Octanol-Air Partition Coefficient (25 deg C)    11.4500
#> 7                  Log Soil Adsorption Coefficient     4.2650
#>         unit_pred                     source_pred
#> 1            <NA> Log Kow (KOWWIN v1.67 estimate)
#> 2           deg C                   MPBPWIN v1.42
#> 3           deg C                   MPBPWIN v1.42
#> 4 mg/L (25 deg C)                     WSKOW v1.41
#> 5            mg/L                            <NA>
#> 6            <NA>                    KOAWIN v1.10
#> 7            <NA>                  PCKOCWIN v1.66
```

#### PubChem

Retrieve PubChem CID

```r
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

Use this CID to retrieve some chemical properties:

```r
pc_prop(c(5564,2244), properties = c('InChIKey', 'MolecularFormula', 'MolecularWeight'))
#>    CID MolecularFormula MolecularWeight                    InChIKey
#> 1 5564       C12H7Cl3O2        289.5418 XEFQLINVKFYRCS-UHFFFAOYSA-N
#> 2 2244           C9H8O4        180.1574 BSYNRYMUTXBXSQ-UHFFFAOYSA-N
```

and synonyms


```r
pc_synonyms(5564, from = 'cid')[[1]][1:5]
#> [1] "5564"                                     
#> [2] "triclosan"                                
#> [3] "5-CHLORO-2-(2,4-DICHLOROPHENOXY)PHENOL"   
#> [4] "3380-34-5"                                
#> [5] "2,4,4'-Trichloro-2'-hydroxydiphenyl ether"
pc_synonyms('Triclosan', from = 'name')[[1]][1:5]
#> [1] "5564"                                     
#> [2] "triclosan"                                
#> [3] "5-CHLORO-2-(2,4-DICHLOROPHENOXY)PHENOL"   
#> [4] "3380-34-5"                                
#> [5] "2,4,4'-Trichloro-2'-hydroxydiphenyl ether"
```



#### Chemical Translation Service (CTS)

CTS allows to convert from nearly every possible identifier to nearly every possible identifier:

```r
cts_convert(query = '3380-34-5', from = 'CAS', to = 'ChemSpider')
#> $`3380-34-5`
#> [1] "31465"
(inchk <- cts_convert(query = '50-00-0', from = 'CAS', to = 'inchikey'))
#> $`50-00-0`
#> [1] "WSFSSNUMVMOOMR-UHFFFAOYSA-N"
```

Moreover, we can a lot of information stored in the CTS database using InChIkey

```r
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
`pan_query()` returns a list of 73 entries, here I extract only 4 of those:

```r
pan_list <- pan_query('lambda-Cyhalothrin', first = TRUE)
pan_list[c("CAS Number", "Chemical Class", "Water Solubility (Avg, mg/L)", "Adsorption Coefficient (Koc)" )]
#> $`CAS Number`
#> [1] "91465-08-6"
#> 
#> $`Chemical Class`
#> [1] "Pyrethroid"
#> 
#> $`Water Solubility (Avg, mg/L)`
#> [1] 0.005
#> 
#> $`Adsorption Coefficient (Koc)`
#> [1] 157000
```



#### Alan Wood's Compendium of Pesticide Common Names

`aw_query()` returns a list of 9 entries and can query common names and cas numbers:

```r
aw_query('Fluazinam', type = 'commonname')
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
#> [1] "fungicides"
#> 
#> $subactivity
#> [1] "pyridine fungicides"
#> 
#> $inchikey
#> [1] "UZCGKGPEKUCDTF-UHFFFAOYSA-N"
#> 
#> $inch
#> [1] "InChI=1S/C13H4Cl2F6N4O4/c14-6-1-4(12(16,17)18)3-22-11(6)23-9-7(24(26)27)2-5(13(19,20)21)8(15)10(9)25(28)29/h1-3H,(H,22,23)"
#> 
#> $source_url
#> [1] "http://www.alanwood.net/pesticides/fluazinam.html"
aw_query('79622-59-6', type = 'cas')$cname
#> [1] "fluazinam"
```

#### SRC PHYSPROP Database
[SRCs PHYSPROP Database](http://www.srcinc.com/what-we-do/environmental/scientific-databases.html) contains chemical structures, names and physical properties for over 41,000 chemicals.
You can use `pp_query()` to query this database using a CAS number:


```r
pp_query('50-00-0')
#> $`50-00-0`
#> [1] NA
```


#### ETOX
ETOX: Information System Ecotoxicology and Environmental Quality Targets is a database run by the Federal Environment Agency of Germany and provides data on synonyms, identifiers, Quality Targest and Effects.

First we need to query a substance ID:


```r
id <- get_etoxid('Triclosan', mult = 'best')
id
#> [1] "20179"
#> attr(,"matched")
#> [1] "Triclosan ( 20179 )"
#> attr(,"distance")
#> [1] 0.5263158
```
`get_etoxid` tries to find the best match for you (check the matched and distance attributes), if multiple hits are found.
Other options are `mult = 'ask'` to enter a interactive mode, `'na'` to return `NA`, `'all'` to return all hits and `'first'` to return the first hit.


```r
get_etoxid('Triclosan', mult = 'all')
#> [1] "20179" "89236"
#> attr(,"matched")
#> [1] "Triclosan ( 20179 )"       "Methyltriclosan ( 89236 )"
#> attr(,"distance")
#> [1] "all"
```



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
#> 4      5-chloro-2-(2,4-dichlorophenoxy)phenol   English
#> 7   Phenol, 5-chloro-2-(2,4-dichlorophenoxy)-    German
#> 8     2,4,4'-Trichlor-2'-hydroxydiphenylether    German
#> 9                              Irgasan DP 300    German
#> 10                                  Vikol THP    German
#> 11     2,4,4-Trichlor-2'-hydroxydiphenylether    German
#> 12   2,4,4'-Trichloro-2'-hydroxydiphenylether    German
#> 14     Chlor-2-(2,4-dichlorphenoxy)phenol, 5- universal
#> 15  Trichlor-2'-hydroxydiphenylether, 2,4,4'- universal
#> 16   Trichlor-2'-hydroxydiphenylether, 2,4,4- universal
#> 17 Trichloro-2'-hydroxydiphenylether, 2,4,4'- universal
#> 18      5-Chlor-2-(2,4-dichlorphenoxy)-phenol universal
#> 19    Chlor-2-(2,4-dichlorphenoxy)-phenol, 5- universal
#> 20       5-Chlor-2-(2,4-dichlorphenoxy)phenol universal
#> 21                                  triclosán   Spanish
#> 22                                triklosaani   Finnish
#> 23                                 triclosano   Italian
#> 24                                  triklosan   Swedish
#> 
#> $source_url
#> [1] "https://webetox.uba.de/webETOX/public/basics/stoff.do?language=en&id=20179"
```

Which returns CAS, EC and GSBL numbers, as well as a synonym list.

We can also retrieve Quality Targets:


```r
targets <- etox_targets(id)
targets$res[ , c('Substance', 'Country_or_Region', 'Designation', 'Value_Target_LR', 'Unit')]
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

and results of ecotox tests:

```r
tests <- etox_tests(id)
tests$res[ , c('Organism', 'Effect', 'Duration', 'Time_Unit','Endpoint', 'Value', 'Unit')]
#>                           Organism                  Effect Duration
#> 1              Anabaena flos-aquae                    k.A.        4
#> 2          Brachionus calyciflorus                    k.A.        2
#> 3          Brachionus calyciflorus                    k.A.        2
#> 4          Brachionus calyciflorus                    k.A.        2
#> 5                Brachydanio rerio Embryo-Larval-Toxizität       10
#> 6               Ceriodaphnia dubia               Letalität        7
#> 7               Ceriodaphnia dubia              Mortalität        2
#> 8               Ceriodaphnia dubia              Mortalität        7
#> 9               Ceriodaphnia dubia                    k.A.        7
#> 10              Ceriodaphnia dubia            Reproduktion        7
#> 11              Ceriodaphnia dubia            Reproduktion        7
#> 12                   Daphnia magna              Mortalität       21
#> 13                   Daphnia magna            Reproduktion       21
#> 14         Desmodesmus subspicatus          Zellvermehrung        4
#> 15          Dunaliella tertiolecta          Zellvermehrung        4
#> 16          Dunaliella tertiolecta          Zellvermehrung        4
#> 17             Oncorhynchus mykiss Embryo-Larval-Toxizität        4
#> 18             Pimephales promelas              Mortalität        4
#> 19 Pseudokirchneriella subcapitata        Wachstumshemmung        3
#> 20         Scenedesmus subspicatus                Biomasse        3
#> 21         Scenedesmus subspicatus                    k.A.        4
#> 22         Scenedesmus subspicatus                    k.A.        4
#> 23         Scenedesmus subspicatus                    k.A.        4
#> 24         Scenedesmus subspicatus            Reproduktion        3
#> 25                 Hyalella azteca              Mortalität       10
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
Earlier versions of `webchem` allowed also to search and download the database.
However, this is explicitly against the terms and conditions of use.
On request we also removed all links to the database.

Therefore, `webchem` can only provide a function to parse html-code into a R object:
`parse_ppdb()`.
However, no examples can be given as saving and downloading the html source is explicitly 
against the terms and conditions of use.





#### ChemIDplus


```r
out <- ci_query(query = 'Triclosan', type = 'name')
out$physprop
#>              Physical Property    Value            Units Temp (deg C)
#> 1                Melting Point       NA            deg C             
#> 2        log P (octanol-water) 4.76e+00           (none)             
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

```r
ids <- get_wdid(query = 'Triclosan', language = 'en')
ids
#> [1] "Q408646"
#> attr(,"matched")
#> [1] "Triclosan"

# quera identifiers from wikidata
wd_ident(ids)[1:3]
#> $smiles
#> [1] "Oc1cc(Cl)ccc1Oc2ccc(Cl)cc2Cl"
#> 
#> $cas
#> [1] "3380-34-5"
#> 
#> $cid
#> [1] "5564"
```


#### Misc functions

##### Check if a string is a valid CAS registry number


```r
is.cas('64-17-5')
#> [1] TRUE
is.cas('64-17-6')
#> [1] FALSE
```


##### Check if a string is a valid InChIKey

Using a pure R implementation:

```r
is.inchikey('BQJCRHHNABKAKU-KBQPJGBKSA-N')
#> [1] TRUE
is.inchikey('BQJCRHHNABKAKU-KBQPJGBKXA-N')
#> [1] FALSE
```

Using the ChemSpider API

```r
is.inchikey('BQJCRHHNABKAKU-KBQPJGBKSA-N', type = 'chemspider')
#> [1] TRUE
is.inchikey('BQJCRHHNABKAKU-KBQPJGBKXA-N', type = 'chemspider')
#> [1] FALSE
```

##### Check if a string is a valid SMILES


```r
is.smiles('Clc(c(Cl)c(Cl)c1C(=O)O)c(Cl)c1Cl')
#> [1] TRUE
# 'J' is not found in the periodic table
is.smiles('Clc(c(Cl)c(Cl)c1C(=O)O)c(Cl)c1ClJ')
#> [1] FALSE
```



#### I have multiple compounds. How should I query those?

The simples possibly is to 1) Query all compounds ant store the results in a list and 2) extract the needed information from this list.
Every function has an example with multiple queries.
Maybe, this will ge easier in the future...

First we query alanwood:

```r
cmp <- c("Isodrin", "Naphthalin1,6-disulfonat", "Pencycuron")
# query alanwood 
aw_out <- lapply(cmp, aw_query)
# this gives for each compound one list
# str(aw_out)
```

Next we extract the cas:


```r
cas <- unlist(sapply(aw_out, '[', 'cas')) 
data.frame(cmp, cas)
#>                        cmp        cas
#> 1                  Isodrin   465-73-6
#> 2 Naphthalin1,6-disulfonat       <NA>
#> 3               Pencycuron 66063-05-6
```


### Acknowledgements
Without the fantastic web services `webchem` wouldn't be here.
Therefore, kudos to the web service providers and developers!


### Related Projects
If you're more familiar with Python you should check out [Matt Swains](https://github.com/mcs07) repositories: [ChemSpiPy](https://github.com/mcs07/ChemSpiPy), [PubChemPy](https://github.com/mcs07/PubChemPy) and [CirPy](https://github.com/mcs07/CIRpy) provide similar functionality as `webchem`.


### Contributors

+ [Eduard Szöcs](https://github.com/EDiLD)
+ [Daniel Münch](https://github.com/ropensci/webchem/commits?author=Dahaniel)
+ [Johannes Ranke](https://github.com/ropensci/webchem/commits?author=jranke)

### Want to contribute?

Checkout our [contribution guide here](https://github.com/ropensci/webchem/blob/master/CONTRIBUTING.md).

### Meta

* Please [report any issues, bugs or feature requests](https://github.com/ropensci/webchem/issues).
* License: MIT
* Get citation information for `webchem` in R doing `citation(package = 'webchem')`

[![ropensci](http://ropensci.org/public_images/github_footer.png)](http://ropensci.org)
