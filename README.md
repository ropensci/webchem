
<!-- README.md is generated from README.Rmd. Please edit that file -->

# Webchem

<!-- badges: start -->

[![status](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)
[![CRAN](https://www.r-pkg.org/badges/version/webchem)](https://CRAN.R-project.org/package=webchem)
[![R build
status](https://github.com/ropensci/webchem/workflows/R-CMD-check/badge.svg)](https://github.com/ropensci/webchem/actions)
[![Coverage](https://codecov.io/github/ropensci/webchem/coverage.svg?branch=master)](https://app.codecov.io/gh/ropensci/webchem/branch/master)
[![Downloads](https://cranlogs.r-pkg.org/badges/webchem)](https://cran.r-project.org/package=webchem)
[![Total
Downloads](https://cranlogs.r-pkg.org/badges/grand-total/webchem?color=blue)](https://cran.r-project.org/package=webchem)
[![DOI](https://img.shields.io/badge/DOI-10.18637%2Fjss.v093.i13-blue)](https://doi.org/10.18637/jss.v093.i13)

<!-- badges: end -->

`webchem` is a R package to retrieve chemical information from the web.
This package interacts with a suite of web APIs to retrieve chemical
information.

The functions in the package that hit a specific API have a prefix and
suffix separated by an underscore (`prefix_suffix()`). They follow the
format of `source_functionality`, with the exception of functions that
retrieve database identifiers which follow the format of
`get_identifier`. e.g.`cs_compinfo` uses ChemSpider to retrieve compound
informations and `get_csid()` retrieves ChemSpider IDs.

## Chemical databases currently accessed by webchem

At least some of the data in the following sources is accesible through
`webchem` functions. To learn more about what is available, browse the
documentation
[here](https://docs.ropensci.org/webchem/reference/index.html).

- [BCPC Compendium of Pesticide Common
  Names](https://pesticidecompendium.bcpc.org) (formerly Alan Wood’s
  Compendium of Pesticide Common Names)
- [ChEBI](https://www.ebi.ac.uk/chebi/)
- [Chemical Identifier Resolver
  (CIR)](https://cactus.nci.nih.gov/chemical/structure)
- [Chemical Translation Service (CTS)](http://cts.fiehnlab.ucdavis.edu/)
- [ChemSpider](http://www.chemspider.com/) (requires an [API
  token](https://developer.rsc.org/))
- [ETOX](http://webetox.uba.de/webETOX/index.do)
- [Flavornet](http://www.flavornet.org)
- [NIST](https://webbook.nist.gov) (currently gas chromatography
  retention indices only)
- [OPSIN](http://opsin.ch.cam.ac.uk/instructions.html)
- [PubChem](https://pubchem.ncbi.nlm.nih.gov/)
- [U.S. EPA Substance Registry Service
  (SRS)](https://cdxnodengn.epa.gov/cdx-srs-rest/)
- [Wikidata](https://www.wikidata.org/wiki/Wikidata:WikiProject_Chemistry)

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

### Use Cases

See how `webchem` has been used or cited in literature
[here](https://scholar.google.com/scholar?cites=14244442030948237605&as_sdt=40000005&sciodt=0,22&hl=en).

### Citation

If you use `webchem` in a publication, please cite our paper:

- Szöcs E, Stirling T, Scott ER, et al (2020) webchem: An R Package to
  Retrieve Chemical Information from the Web. J Stat Soft 93:.
  <https://doi.org/10.18637/jss.v093.i13>

### Acknowledgements

Without the fantastic web services `webchem` wouldn’t be here.
Therefore, kudos to the web service providers and developers! Please
remember to acknowledge these data resources in your work using
`webchem`.

### Want to contribute?

Check out our [contribution guide
here](https://github.com/ropensci/webchem/blob/master/CONTRIBUTING.md).

### Meta

- Please [report any issues, bugs or feature
  requests](https://github.com/ropensci/webchem/issues).
- License: MIT
- Get citation information for `webchem` in R with `citation("webchem")`
- Please note that this package is released with a [Contributor Code of
  Conduct](https://ropensci.org/code-of-conduct/). By contributing to
  this project, you agree to abide by its terms.

[![ropensci](https://ropensci.org/public_images/github_footer.png)](https://ropensci.org)
