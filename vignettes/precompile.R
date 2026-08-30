# Precomplie vignettes locally
# More info here: https://ropensci.org/technotes/2019/12/08/precompute-vignettes/
library(knitr)
knit("vignettes/webchem.Rmd.orig", "vignettes/webchem.Rmd") #Get Started
knit("vignettes/pubchem-pages.Rmd.orig", "vignettes/pubchem-pages.Rmd") #PubChem pages
knit("vignettes/webchem-offline.Rmd.orig", "vignettes/webchem-offline.Rmd") #PubChem pages
