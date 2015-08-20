#' Get ETOX ID
#'
#' Query ETOX: Information System Ecotoxicology and Environmental Quality Targets
#' \url{http://webetox.uba.de/webETOX/index.do} for their substance ID
#'
#' @import XML RCurl
#'
#' @param  query character; The searchterm
#' @param verbose logical; print message during processing to console?
#'
#' @return A character vector, with the attributes \code{matched}  (the matched
#' substance name) and \code{distance} (the normalized string distance of the query to the match).
#'
#' @note If more than one reference is found only the first hit is taken.
#' @author Eduard Szoecs, \email{eduardszoecs@@gmail.com}
#' @export
#' @examples
#' \dontrun{
#' get_etoxid('Triclosan')
#' }
get_etoxid <- function(query, verbose = TRUE){
  clean_char <- function(x){
    # rm \n \t
    x <- gsub('\n | \t', '', x)
    # rm leading / trailling whitespace
    x <- gsub("^\\s+|\\s+$", "", x)
    # replace multiple spaces by one,
    # http://stackoverflow.com/questions/25707647/merge-multiple-spaces-to-single-space-remove-trailing-leading-spaces
    x <- gsub("(?<=[\\s])\\s*|^\\s+$", "", x, perl = TRUE)
    return(x)
  }

  # query <- 'Triclosan'
  if (verbose)
    message('Searching ', query)
  baseurl <- 'http://webetox.uba.de/webETOX/public/search/stoff.do'

  Sys.sleep(0.1)
  out <- postForm(baseurl,
                  .params = list('stoffname.selection[0].name' = query,
                                 event = 'Search'))

  # get substances and links
  tt <- htmlParse(out)
  subs <- clean_char(xpathSApply(tt,"//*/table[@class = 'listForm resultList']//a",
                                 xmlValue))
  if (length(subs) == 0) {
    if (verbose)
      message('Substance not found! Returing NA. \n')
    return(NA)
  }
  type <- clean_char(xpathSApply(tt,"//*/table[@class = 'listForm resultList']/tr/td[2]",
                                 xmlValue))
  links <- xpathSApply(tt, "//*/table[@class = 'listForm resultList']//a//@href")

  # match query with substance, get link
  if (length(unique(links)) > 1) {
    if (verbose)
      message("More then one Link found. Returning best match. \n")
    dd <- adist(query, subs[type == 'ETOX_NAME']) / nchar(subs[type == 'ETOX_NAME'])
    takelink <- links[type == 'ETOX_NAME'][which.min(dd)]
    d <- dd[which.min(dd)]
    matched_sub <- subs[type == 'ETOX_NAME'][which.min(dd)]
  } else {
    takelink <- unique(links)
    d <- 0
    matched_sub <- subs[type == 'ETOX_NAME'][1]
  }
  id <- gsub('^.*\\?id=(.*)', '\\1', takelink)
  names(id) <- NULL
  attr(id, "matched") <- matched_sub
  attr(id, "distance") <- d
  return(id)
}