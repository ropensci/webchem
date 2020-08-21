#' Get record details from U.S. EPA Substance Registry Servives (SRS)
#'
#' Get record details from SRS, see \url{https://cdxnodengn.epa.gov/cdx-srs-rest/}
#'
#'@param query character; query ID.
#'@param from character; type of query ID, e.g. \code{'itn'} , \code{'cas'},
#'  \code{'epaid'}, \code{'tsn'}, \code{'name'}.
#'@param ... not currently used.
#'@return a list of lists (for each supplied query): a list of 22. subsKey,
#'  internalTrackingNumber, systematicName, epaIdentificationNumber,
#'  currentCasNumber, currentTaxonomicSerialNumber, epaName, substanceType,
#'  categoryClass, kingdomCode, iupacName, pubChemId, molecularWeight,
#'  molecularFormula, inchiNotation, smilesNotation, classifications,
#'  characteristics, synonyms, casNumbers, taxonomicSerialNumbers, relationships
#'@author Gordon Getzinger, \email{gjg3@@duke.edu}
#'@export
#'
#' @examples
#' \donttest{
#' # might fail if API is not available
#' srs_query(query = '50-00-0', from = 'cas')
#'
#' ### multiple inputs
#' casrn <- c('50-00-0', '67-64-1')
#' srs_query(query = casrn, from = 'cas')
#' }
srs_query <-
  function(query,
           from = c("itn", "cas", "epaid", "tsn", "name"),
           verbose = TRUE, ...) {
    from <- match.arg(from)
    entity_url <- "https://cdxnodengn.epa.gov/cdx-srs-rest/"

    rst <- lapply(query, function(x) {
      if (is.na(x)){
        if (verbose) message(webchem_string("na"))
        return(NA)
      }
      entity_query <- paste0(entity_url, "/substance/", from, "/", x)
      if (verbose) message(webchem_string("query", x), appendLF = FALSE)
      response <- httr::RETRY("GET",
                              entity_query,
                              httr::user_agent(webchem_string("webchem")),
                              terminate_on = 404,
                              quiet = TRUE)
      if (response$status_code == 200) {
        if (verbose) message(httr::message_for_status(response))
        text_content <- httr::content(response, "text")
        if (text_content == "[]") {
          if (verbose) message(webchem_string("not_available"))
          return(NA)
        } else {
          jsonlite::fromJSON(text_content)
        }
      } else {
        if (verbose) message(httr::message_for_status(response))
        return(NA)
      }
    })
    names(rst) <- query
    return(rst)
  }
