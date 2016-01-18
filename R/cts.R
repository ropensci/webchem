#' Get record details from Chemical Translation Service (CTS)
#'
#' Get record details from CTS, see \url{http://cts.fiehnlab.ucdavis.edu}
#' @import jsonlite
#' @importFrom stats rgamma
#' @param inchikey character; InChIkey.
#' @param verbose logical; should a verbose output be printed on the console?
#' @param ... currently not used.
#' @return a list of 7. inchikey, inchicode, molweight, exactmass, formula, synonyms and externalIds
#' @author Eduard Szoecs, \email{eduardszoecs@@gmail.com}
#'
#' @references Wohlgemuth, G., P. K. Haldiya, E. Willighagen, T. Kind, and O. Fiehn 2010The Chemical Translation Service
#' -- a Web-Based Tool to Improve Standardization of Metabolomic Reports. Bioinformatics 26(20): 2647–2648.
#' @export
#' @examples
#' \donttest{
#' # might fail if API is not available
#' out <- cts_compinfo("XEFQLINVKFYRCS-UHFFFAOYSA-N")
#' # = Triclosan
#' str(out)
#' out[1:5]
#'
#' ### multiple inputs
#' inchkeys <- c("XEFQLINVKFYRCS-UHFFFAOYSA-N","BSYNRYMUTXBXSQ-UHFFFAOYSA-N" )
#' ll <- lapply(inchkeys, function(x) cts_compinfo(x)[1:5])
#' do.call(rbind, ll)
#' }
cts_compinfo <- function(inchikey, verbose = TRUE, ...){
  # inchikey <- "XEFQLINVKFYRCS-UHFFFAOYSA-N"
  if (length(inchikey) > 1) {
    stop('Cannot handle multiple input strings.')
  }
  if (!is.inchikey(inchikey)) {
    stop('Input is not a valid inchikey!')
  }
  baseurl <- "http://cts.fiehnlab.ucdavis.edu/service/compound"
  qurl <- paste0(baseurl, '/', inchikey)
  if (verbose)
    message(qurl)
  Sys.sleep( rgamma(1, shape = 15, scale = 1/10))
  out <- try(fromJSON(qurl), silent = TRUE)
  if (inherits(out, "try-error")) {
    warning('Not found... Returning NA.')
    return(NA)
  }
  return(out)
}


#' Convert Ids using Chemical Translation Service (CTS)
#'
#' Convert Ids using Chemical Translation Service (CTS), see \url{http://cts.fiehnlab.ucdavis.edu/conversion/index}
#' @import RCurl jsonlite
#' @importFrom utils URLencode
#' @importFrom stats rgamma
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
#'
#' @references Wohlgemuth, G., P. K. Haldiya, E. Willighagen, T. Kind, and O. Fiehn 2010The Chemical Translation Service
#' -- a Web-Based Tool to Improve Standardization of Metabolomic Reports. Bioinformatics 26(20): 2647–2648.
#' @export
#' @examples
#' \donttest{
#' # might fail if API is not available
#' cts_convert('XEFQLINVKFYRCS-UHFFFAOYSA-N', 'inchikey', 'Chemical Name')
#'
#' ### multiple inputs
#' comp <- c('XEFQLINVKFYRCS-UHFFFAOYSA-N', 'BSYNRYMUTXBXSQ-UHFFFAOYSA-N')
#' sapply(comp, function(x) cts_convert(x, 'inchikey', 'Chemical Name', first = TRUE))
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
  Sys.sleep( rgamma(1, shape = 15, scale = 1/10))
  out <- try(fromJSON(qurl), silent = TRUE)
  if (inherits(out, "try-error")) {
    warning('Not found... Returning NA.')
    return(NA)
  }
  out <- out$result[[1]]
  if (length(out) == 0) {
    message("Not found. Returning NA.")
    return(NA)
  }
  if (first)
    out <- out[1]
  return(out)
}
