#' Get record details from U.S. EPA Substance Registry Servives (SRS)
#'
#' Get record details from SRS, see \url{https://cdxnodengn.epa.gov/cdx-srs-rest/}
#'
#'@param query character; query ID.
#'@param from character; type of query ID, e.g. \code{'itn'} , \code{'cas'},
#'  \code{'epaid'}, \code{'tsn'}, \code{'name'}.
#'@param verbose logical; should a verbose output be printed on the console?
#'@param ... not currently used.
#'@return a list of lists (for each supplied query): a list of 22. subsKey,
#'  internalTrackingNumber, systematicName, epaIdentificationNumber,
#'  currentCasNumber, currentTaxonomicSerialNumber, epaName, substanceType,
#'  categoryClass, kingdomCode, iupacName, pubChemId, molecularWeight,
#'  molecularFormula, inchiNotation, smilesNotation, classifications,
#'  characteristics, synonyms, casNumbers, taxonomicSerialNumbers, relationships
#'@export
#'
#' @examples
#' \dontrun{
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
           verbose = getOption("verbose"), ...) {

    if (!ping_service("srs")) stop(webchem_message("service_down"))
    names(query) <- query
    from <- match.arg(from)
    entity_url <- "https://cdxnodengn.epa.gov/cdx-srs-rest/"
    if (from == "cas"){
      query <- as.cas(query, verbose = verbose)
    }
    rst <- lapply(query, function(x) {
      if (is.na(x)){
        if (verbose) webchem_message("na")
        return(NA)
      }
      entity_query <- paste0(entity_url, "/substance/", from, "/", x)
      if (verbose) webchem_message("query", x, appendLF = FALSE)
      webchem_sleep(type = 'API')
      response <- try(httr::RETRY("GET",
                                  entity_query,
                                  httr::user_agent(webchem_url()),
                                  terminate_on = 404,
                                  quiet = TRUE), silent = TRUE)
      if (inherits(response, "try-error")) {
        if (verbose) webchem_message("service_down")
        return(NA)
      }
      if (verbose) message(httr::message_for_status(response))
      if (response$status_code == 200) {
        text_content <- httr::content(response, "text")
        if (text_content == "[]") {
          if (verbose) webchem_message("not_available")
          return(NA)
        } else {
          jsonlite::fromJSON(text_content)
        }
      } else {
        return(NA)
      }
    })
    return(rst)
  }
