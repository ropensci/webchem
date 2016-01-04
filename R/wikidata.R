#' Get Wikidata Item ID
#'
#' @import jsonlite httr
#'
#' @param  query character; The searchterm
#' @param language character; the language to search in
#' @param match character; \code{match = "all"} returns all matches,
#'   \code{match = "first"} the first one and \code{match = "best"} the hit with the lowest
#'    Levenshtein distance between query and hit.
#' @param verbose logical; print message during processing to console?
#'
#' @return A character vector with the item ID and additional attributes \code{matched}  (the matched
#' substance name) and \code{distance} (the normalized string distance of the query to the match).
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
get_wdid <- function(query, language, match = c('all', 'first', 'best'), verbose = TRUE){
  # language <-  'en'
  # query <- 'Triclosan'
  limit <-  50
  match <- match.arg(match)
  qurl <- paste0("wikidata.org/w/api.php?action=wbsearchentities&format=json&type=item")
  qurl <- paste0(qurl, "&language=", language, "&limit=", limit, "&search=", query)
  if (verbose)
    message('Querying ', qurl)
  cont <- fromJSON(content(GET(qurl, user_agent('webchem (https://github.com/ropensci/webchem)')), 'text'))
  search <- cont$search

  # use only matches on label
  search <- search[search$match$type == 'label', ]
  if (nrow(search) > 1) {
    if (match == 'first') {
      search <- search[1, ]
    }
    if (match == 'best') {
      hits <- search$label
      dists <- sapply(hits, function(x) min((adist(query, x) / nchar(x))[1 , ]))
      take <- which.min(dists)
      out <- search$id[take]
      attr(out, "matched") <- search$label[take]
      attr(out, "distance") <- dists[take]
    }
  } else {
    out <- search$id
    attr(out, "matched") <- search$label
    attr(out, "distance") <- 0
  }
  return(out)
}


