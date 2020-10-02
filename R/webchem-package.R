#' webchem: An R package to retrieve chemical information from the web.
#'
#' Chemical information from around the web. This package interacts with a suite
#' of web APIs for chemical information.
#'
#' @docType package
#' @name webchem
#' @importFrom methods is
#' @importFrom utils globalVariables
if (getRversion() >= "2.15.1")
  globalVariables(c("."))



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
#' @source \url{https://udo.lubw.baden-wuerttemberg.de/?highlightglobalid=gewaesserguetedaten}
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
