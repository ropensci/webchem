#' Get record details from Chemical Translation Service (CTS)
#'
#' Get record details from CTS, see \url{http://cts.fiehnlab.ucdavis.edu}
#' @import jsonlite
#' @importFrom stats rgamma
#' @importFrom stats setNames
#' @param inchikey character; InChIkey.
#' @param verbose logical; should a verbose output be printed on the console?
#' @return a list of lists (for each supplied inchikey):
#' a list of 7. inchikey, inchicode, molweight, exactmass, formula, synonyms and externalIds
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
#' out[[1]][1:5]
#'
#' ### multiple inputs
#' inchikeys <- c("XEFQLINVKFYRCS-UHFFFAOYSA-N","BSYNRYMUTXBXSQ-UHFFFAOYSA-N" )
#' out2 <- cts_compinfo(inchikeys)
#' str(out2)
#' # a list of two
#' # extract molecular weight
#' sapply(out2, function(y) y$molweight)
#' }
cts_compinfo <- function(inchikey, verbose = TRUE){
  # inchikey <- 'XEFQLINVKFYRCS-UHFFFAOYSA-N'
  foo <- function(inchikey, verbose) {
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
  out <- lapply(inchikey, foo, verbose = verbose)
  out <- setNames(out, inchikey)
  class(out) <- c('list', 'cts_compinfo')
  return(out)
}


#' Convert Ids using Chemical Translation Service (CTS)
#'
#' Convert Ids using Chemical Translation Service (CTS), see \url{http://cts.fiehnlab.ucdavis.edu/conversion/index}
#' @import RCurl jsonlite
#' @importFrom utils URLencode
#' @importFrom stats rgamma
#' @importFrom stats setNames
#' @param query character; query ID.
#' @param from character; type of query ID, e.g. \code{'Chemical Name'} , \code{'InChIKey'},
#'  \code{'PubChem CID'}, \code{'ChemSpider'}, \code{'CAS'}.
#' @param to character; type to convert to.
#' @param first logical; return only first result be returned?
#' @param verbose logical; should a verbose output be printed on the console?
#' @param ... currently not used.
#' @return a list of characters. If first = TRUE a vector.
#' @author Eduard Szoecs, \email{eduardszoecs@@gmail.com}
#' @details See also \url{http://cts.fiehnlab.ucdavis.edu/conversion/index}
#' for possible values of from and to.
#'
#' @seealso \code{\link{cts_from}} for possible values in the 'from' argument and
#' \code{\link{cts_to}} for possible values in the 'to' argument.
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
#' cts_convert(comp, 'inchikey', 'Chemical Name')
#' }
cts_convert <- function(query, from, to, first = FALSE, verbose = TRUE, ...){
  if (length(from) > 1 | length(to) > 1) {
    stop('Cannot handle multiple input strings.')
  }
  foo <- function(query, from, to , first, verbose){
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
    # if (length(out) == 0) {
    #   message("Not found. Returning NA.")
    #   return(NA)
    # }
    if (first)
      out <- out[1]
    return(out)
  }
  out <- lapply(query, foo, from = from, to = to, first = first, verbose = verbose)
  out <- setNames(out, query)
  if (first)
    out <- unlist(out)
  return(out)
}


#' Return a list of all possible ids
#'
#' Return a list of all possible ids that can be used in the 'from' argument
#' @import jsonlite
#' @param verbose logical; should a verbose output be printed on the console?
#' @return a character vector.
#' @author Eduard Szoecs, \email{eduardszoecs@@gmail.com}
#' @details See also \url{http://cts.fiehnlab.ucdavis.edu/moreServices/index#fromnames}
#'
#' @seealso \code{\link{cts_convert}}
#'
#' @references Wohlgemuth, G., P. K. Haldiya, E. Willighagen, T. Kind, and O. Fiehn 2010The Chemical Translation Service
#' -- a Web-Based Tool to Improve Standardization of Metabolomic Reports. Bioinformatics 26(20): 2647–2648.
#' @export
#' @examples
#' \donttest{
#' cts_from()
#' }
cts_from <- function(verbose = TRUE){
  fromJSON('http://cts.fiehnlab.ucdavis.edu/service/conversion/fromValues')
}


#' Return a list of all possible ids
#'
#' Return a list of all possible ids that can be used in the 'to' argument
#' @import jsonlite
#' @param verbose logical; should a verbose output be printed on the console?
#' @return a character vector.
#' @author Eduard Szoecs, \email{eduardszoecs@@gmail.com}
#' @details See also \url{http://cts.fiehnlab.ucdavis.edu/moreServices/index#fromnames}
#'
#' @seealso \code{\link{cts_convert}}
#'
#' @references Wohlgemuth, G., P. K. Haldiya, E. Willighagen, T. Kind, and O. Fiehn 2010The Chemical Translation Service
#' -- a Web-Based Tool to Improve Standardization of Metabolomic Reports. Bioinformatics 26(20): 2647–2648.
#' @export
#' @examples
#' \donttest{
#' cts_from()
#' }
cts_to <- function(verbose = TRUE){
  fromJSON('http://cts.fiehnlab.ucdavis.edu/service/conversion/toValues')
}