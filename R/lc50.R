#' Acute toxicity data from U.S. EPA ECOTOX
#'
#' This dataset comprises acute ecotoxicity data of 124 insecticides.
#' The data is publicly available and can be retrieved from the EPA ECOTOX database
#' (\url{https://cfpub.epa.gov/ecotox/})
#' It comprises acute toxicity data (D. magna, 48h, Laboratory, 48h) and has been
#' preprocessed (remove non-insecticides, aggregate multiple value, keep only numeric data etc).
#'
#' @format A data frame with 124 rows and 2 variables:
#' \describe{
#'   \item{cas}{CAS registry number}
#'   \item{value}{LC50value}
#' }
#' @source \url{https://cfpub.epa.gov/ecotox/}
"lc50"
