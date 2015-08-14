#' Get record details from Chemical Translation Service (CTS)
#'
#' Get record details from CTS, see \url{http://cts.fiehnlab.ucdavis.edu}
#' @import RCurl jsonlite
#' @param inchikey character; InChIkey.
#' @param verbose logical; should a verbose output be printed on the console?
#' @param ... currently not used.
#' @return a list of 7. inchikey, inchicode, molweight, exactmass, formula, synonyms and externalIds
#' @author Eduard Szoecs, \email{eduardszoecs@@gmail.com}
#' @export
#' @examples
#' \donttest{
#' # might fail if API is not available
#' out <- cts_compinfo("XEFQLINVKFYRCS-UHFFFAOYSA-N")
#' # = Triclosa
#' str(out)
#' out[1:5]
#'
#' ### multiple inputs
#' comp <- c('Triclosan', 'Aspirin')
#' inchkeys <- sapply(comp, function(x) cir_query(x, 'stdinchikey', first = TRUE))
#' # ne to strip '#InChIKey='
#' inchkeys <- gsub('InChIKey=', '', inchkeys)
#' ll <- lapply(inchkeys, function(x) cts_compinfo(x)[1:5])
#' do.call(rbind, ll)
#' }
cts_compinfo <- function(inchikey, verbose = TRUE, ...){
  if (length(inchikey) > 1) {
    stop('Cannot handle multiple input strings.')
  }
  baseurl <- "http://cts.fiehnlab.ucdavis.edu/service/compound"
  qurl <- paste0(baseurl, '/', inchikey)
  if (verbose)
    message(qurl)
  Sys.sleep(0.1)
  h <- try(getURL(qurl), silent = TRUE)
  if (!inherits(h, "try-error")) {
    out <- fromJSON(h)
  } else{
    warning('Problem with web service encountered... Returning NA.')
    return(NA)
  }
  if (length(out) == 1 && grepl('invalid', out)) {
    message("invalid InChIKey. Returning NA.")
    return(NA)
  }
  return(out)
}

#' Convert Ids using Chemical Translation Service (CTS)
#'
#' Convert Ids using Chemical Translation Service (CTS), see \url{http://cts.fiehnlab.ucdavis.edu/conversion/index}
#' @import RCurl jsonlite
#' @importFrom utils URLencode
#' @param query character; query ID.
#' @param from character; type of query ID, e.g. \code{'Chemical Name'} , \code{'InChIKey'},
#'  \code{'PubChem CID'}, \code{'ChemSpider'}, \code{'CAS'}.
#' @param to character; type to convert to.
#' @param first logical; return only first result be returned?
#' @param verbose logical; should a verbose output be printed on the console?
#' @param ... currently not used.
#' @return a character vector.
#' @author Eduard Szoecs, \email{eduardszoecs@@gmail.com}
#' @details See see \url{http://cts.fiehnlab.ucdavis.edu/conversion/index}
#' for possible values of from and to.
#' @export
#' @examples
#' \donttest{
#' # might fail if API is not available
#' cts_convert('XEFQLINVKFYRCS-UHFFFAOYSA-N', 'inchikey', 'Chemical Name')
#'
#' ### multiple inputs
#' comp <- c('Triclosan', 'Aspirin')
#' sapply(comp, function(x) cts_convert(x, 'Chemical Name', 'CAS', first = TRUE))
#' }
cts_convert <- function(query, from, to, first = FALSE, verbose = TRUE, ...){
  if (length(query) > 1 | length(from) > 1 | length(to) > 1) {
    stop('Cannot handle multiple input strings.')
  }
  if (is.na(query)) {
    warning('Identifier is NA... Returning NA.')
    return(NA)
  }
  baseurl <- "http://cts.fiehnlab.ucdavis.edu/service/convert"
  qurl <- paste0(baseurl, '/', from, '/', to, '/', query)
  qurl <- URLencode(qurl)
  if (verbose)
    message(qurl)
  Sys.sleep(0.1)
  h <- try(getURL(qurl), silent = TRUE)
  if (!inherits(h, "try-error")) {
    out <- fromJSON(h)
  } else {
    warning('Problem with web service encountered... Returning NA.')
    return(NA)
  }
  if ('error' %in% names(out)) {
    warning('Error in query : \n', out['error'], "\n Returning NA.")
    return(NA)
  } else {
    out <- out$result[[1]]
  }
  if (length(out) == 0) {
    message("Not found. Returning NA.")
    return(NA)
  }
  if (first)
    out <- out[1]
  return(out)
}
