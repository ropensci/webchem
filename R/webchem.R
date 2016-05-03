#' webchem: An R package to retrieve chemical information from the web.
#'
#'
#' @docType package
#' @name webchem
NULL



#' Organic plant protection products in the river Jagst / Germany in 2013
#'
#' This dataset comprises environmental monitoring data of organic plant protection products
#' in the year 2013 in the river Jagst, Germany.
#' The data is publicly available and can be retrieved from the
#' LUBW Landesanstalt für Umwelt, Messungen und Naturschutz Baden-Württemberg.
#' It has been preprocessed and comprises measurements of 34 substances.
#' Substances without detects have been removed.
#' on 13 sampling occasions.
#' Values are given in ug/L.
#'
#' @format A data frame with 442 rows and 4 variables:
#' \describe{
#'   \item{date}{sampling data}
#'   \item{substance}{substance names}
#'   \item{value}{concentration in ug/L}
#'   \item{qual}{qualifier, indicating values < LOQ}
#' }
#' @source \url{http://jdkfg.lubw.baden-wuerttemberg.de/servlet/is/300/}
"jagst"


#' Acute toxicity data from U.S. EPA ECOTOX
#'
#' This dataset comprises acute ecotoxicity data of 124 insecticides.
#' The data is publicly available and can be retrieved from the EPA ECOTOX database
#' (\url{http://cfpub.epa.gov/ecotox/})
#' It comprises acute toxicity data (D. magna, 48h, Laboratory, 48h) and has been
#' preprocessed (remove non-insecticides, aggregate multiple value, keep only numeric data etc).
#'
#' @format A data frame with 124 rows and 2 variables:
#' \describe{
#'   \item{cas}{CAS registry number}
#'   \item{value}{LC50value}
#' }
#' @source \url{http://cfpub.epa.gov/ecotox/}
"lc50"


#' Index of Alan Woods Compendium of Pesticides
#'
#' This dataset is a index of Alan Woods Compendium of Pesticides  \url{http://www.alanwood.net/pesticides}.
#' This index is if for use with \code{\link{aw_query}}.
#' You can use the function \code{\link{build_aw_idx}} to rebuild the index.
#' Date of build: 12. Feb. 2016
#'
#' @format A data frame with 2152 rows and 4 variables:
#' \describe{
#'   \item{names}{CAS numbers}
#'   \item{links}{URL to webpage}
#'   \item{linknames}{names in link / substance names}
#'   \item{source}{source of link, either from CAS (rn) or Commonname (cn)}
#' }
#' @source \url{http://www.alanwood.net/pesticides}
"aw_idx"