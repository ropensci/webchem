#' Get Wikidata Item ID
#'
#' @import jsonlite httr
#'
#' @param query character; The searchterm
#' @param language character; the language to search in
#' @param first logical; If TRUE return only first result.
#' @param verbose logical; print message during processing to console?
#'
#' @return A character vector with the item ID and the additional attribute \code{matched}  (the matched
#' label).
#'
#' @note Only matches in labels are returned.
#'
#' @author Eduard Szoecs, \email{eduardszoecs@@gmail.com}
#' @export
#' @examples
#' \dontrun{
#' get_wdid('Triclosan', language = 'en')
#' # multiple inpus
#' comps <- c('Triclosan', 'Glyphosate')
#' sapply(comps, get_wdid, language = 'en')
#' }
get_wdid <- function(query, language, first = FALSE, verbose = TRUE){
  # language <-  'en'
  # query <- 'Triclosan'
  if (length(query) > 1) {
    stop('Cannot handle multiple input strings.')
  }
  limit <-  50
  qurl <- paste0("wikidata.org/w/api.php?action=wbsearchentities&format=json&type=item")
  qurl <- paste0(qurl, "&language=", language, "&limit=", limit, "&search=", query)
  if (verbose)
    message('Querying ', qurl)
  cont <- fromJSON(content(GET(qurl, user_agent('webchem (https://github.com/ropensci/webchem)')), 'text'))
  search <- cont$search
  if (length(search) == 0) {
    if (verbose)
      message('Substance not found! Returing NA. \n')
    return(NA)
  }
  # use only matches on label
  search <- search[search$match$type == 'label', ]
  search <- search[tolower(search$match$text) == tolower(query), ]

  if (first) {
    search <- search[1, ]
  }

  out <- search$id
  attr(out, "matched") <- search$label
  return(out)
}

#! Use SPARQL to search of chemical compounds (P31)?! For a finer / better search?
