#' Get record details from Chemical Translation Service (CTS)
#'
#' Get record details from CTS, see \url{http://cts.fiehnlab.ucdavis.edu/}
#' @import jsonlite
#' @importFrom stats rgamma
#' @importFrom stats setNames
#' @param query character; InChIkey.
#' @param from character; currently only accepts "inchikey".
#' @param verbose logical; should a verbose output be printed on the console?
#' @param inchikey deprecated
#' @return a list of lists (for each supplied inchikey):
#' a list of 7. inchikey, inchicode, molweight, exactmass, formula, synonyms and externalIds
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
cts_compinfo <- function(query, from = "inchikey", verbose = TRUE, inchikey){

  if (!ping_service("cts")) stop(webchem_message("service_down"))

  if (!missing(inchikey)) {
    message('"inchikey" is deprecated.  Please use "query" instead.')
    query <- inchikey
  }
  from <- match.arg(from)
  foo <- function(query, verbose) {
    if (is.na(query)) {
      if (verbose) webchem_message("na")
      return(NA)
    }
    if(verbose) webchem_message("query", query, appendLF = FALSE)
    if (!is.inchikey(query, verbose = FALSE)) {
      if (verbose) message("Input is not a valid inchikey.")
      return(NA)
    }
    baseurl <- "http://cts.fiehnlab.ucdavis.edu/service/compound"
    qurl <- paste0(baseurl, '/', query)
    Sys.sleep(stats::rgamma(1, shape = 15, scale = 1/10))
    out <- try(httr::RETRY("GET",
                           qurl,
                           httr::user_agent(webchem_url()),
                           terminate_on = 404,
                           quiet = TRUE), silent = TRUE)
    if (inherits(out, "try-error")) {
      if (verbose) webchem_message("service_down")
      return(NA)
    }
    if (verbose) message(httr::message_for_status(out))
    if (out$status_code == 200) {
      out <- jsonlite::fromJSON(rawToChar(out$content))
      return(out)
    }
    else {
      return(NA)
    }
  }
  out <- lapply(query, foo, verbose = verbose)
  out <- setNames(out, query)
  class(out) <- c('cts_compinfo','list')
  return(out)
}


#' Convert Ids using Chemical Translation Service (CTS)
#'
#' Convert Ids using Chemical Translation Service (CTS), see \url{http://cts.fiehnlab.ucdavis.edu/}
#' @import RCurl jsonlite
#' @importFrom utils URLencode
#' @importFrom stats rgamma
#' @importFrom stats setNames
#' @param query character; query ID.
#' @param from character; type of query ID, e.g. \code{'Chemical Name'} , \code{'InChIKey'},
#'  \code{'PubChem CID'}, \code{'ChemSpider'}, \code{'CAS'}.
#' @param to character; type to convert to.
#' @param match character; How should multiple hits be handled? \code{"all"}
#' returns all matches, \code{"first"} returns only the first result,
#' \code{"ask"} enters an interactive mode and the user is asked for input,
#' \code{"na"} returns \code{NA} if multiple hits are found.
#' @param choices deprecated.  Use the \code{match} argument instead.
#' @param verbose logical; should a verbose output be printed on the console?
#' @param ... currently not used.
#' @return a list of character vectors or if \code{choices} is used, then a single named vector.
#' @details See also \url{http://cts.fiehnlab.ucdavis.edu/}
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
#' cts_convert("triclosan", "Chemical Name", "inchikey")
#'
#' ### multiple inputs
#' comp <- c("triclosan", "hexane")
#' cts_convert(comp, "Chemical Name", "cas")
#' }
cts_convert <- function(query,
                        from,
                        to,
                        match = c("all", "first", "ask", "na"),
                        verbose = TRUE,
                        choices = NULL,
                        ...){

  if (!ping_service("cts")) stop(webchem_message("service_down"))

  if(!missing("choices")) {
    if (is.null(choices)) {
      message('"choices" is deprecated.  Using match = "all" instead.')
      match <- "all"
    } else if(choices == 1) {
      message('"choices" is deprecated.  Using match= "first" instead.')
      match <- "first"
    } else if ((is.numeric(choices) & choices > 1) | choices == "all") {
      message('"choices" is deprecated.  Using match = "ask" instead.')
      match <- "ask"
    } else {
      message('"choices" is deprecated.  Using match = "all" instead.')
      match <- "all"
    }
  }
  if (length(from) > 1 | length(to) > 1) {
    stop('Cannot handle multiple input or output types.  Please provide only one argument for `from` and `to`.')
  }

  from <-  match.arg(tolower(from), c(cts_from(), "name"))
  to <-  match.arg(tolower(to), c(cts_to(), "name"))
  match <- match.arg(match)

  if (from == "name") {
    from <- "chemical name"
  }

  if (to == "name") {
    to <- "chemical name"
  }

  foo <- function(query, from, to, first, verbose){
    if (is.na(query)) {
      if (verbose) webchem_message("na")
      return(NA)
    }
    if(verbose) webchem_message("query", query, appendLF = FALSE)
    query <- URLencode(query, reserved = TRUE)
    from <- URLencode(from, reserved = TRUE)
    to <- URLencode(to, reserved = TRUE)
    baseurl <- "http://cts.fiehnlab.ucdavis.edu/service/convert"
    qurl <- paste0(baseurl, '/', from, '/', to, '/', query)
    Sys.sleep(stats::rgamma(1, shape = 15, scale = 1/10))
    res <- try(httr::RETRY("GET",
                           qurl,
                           httr::user_agent(webchem_url()),
                           terminate_on = 404,
                           quiet = TRUE), silent = TRUE)
    if (inherits(res, "try-error")) {
      if (verbose) webchem_message("service_down")
      return(NA)
    }
    if (verbose) message(httr::message_for_status(res))
    if (res$status_code == 200) {
      out <- jsonlite::fromJSON(rawToChar(res$content))
      if (length(out$result[[1]]) == 0) {
        if (verbose) webchem_message("not_found")
        return(NA)
      }
      out <- out$result[[1]]
      out <- matcher(out, match = match, query = query, from = from, verbose = verbose)
      return(out)
    }
    else {
      return(NA)
    }
  }
  out <- lapply(query, foo, from = from, to = to, first = first, verbose = verbose)
  out <- setNames(out, query)
  return(out)
}


#' Return a list of all possible ids
#'
#' Return a list of all possible ids that can be used in the 'from' argument
#' @import jsonlite
#' @param verbose logical; should a verbose output be printed on the console?
#' @return a character vector.
#' @details See also \url{http://cts.fiehnlab.ucdavis.edu/services}
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

  if (!ping_service("cts")) stop(webchem_message("service_down"))

  qurl <- "http://cts.fiehnlab.ucdavis.edu/service/conversion/fromValues"
  res <- try(httr::RETRY("GET",
                         qurl,
                         httr::user_agent(webchem_url()),
                         terminate_on = 404,
                         quiet = TRUE), silent = TRUE)
  if (inherits(res, "try-error")) {
    if (verbose) webchem_message("service_down")
    return(NA)
  }
  out <- tolower(jsonlite::fromJSON(rawToChar(res$content)))
  return(out)
}


#' Return a list of all possible ids
#'
#' Return a list of all possible ids that can be used in the 'to' argument
#' @import jsonlite
#' @param verbose logical; should a verbose output be printed on the console?
#' @return a character vector.
#' @details See also \url{http://cts.fiehnlab.ucdavis.edu/services}
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

  if (!ping_service("cts")) stop(webchem_message("service_down"))

  qurl <- "http://cts.fiehnlab.ucdavis.edu/service/conversion/toValues"
  res <- try(httr::RETRY("GET",
                         qurl,
                         httr::user_agent(webchem_url()),
                         terminate_on = 404,
                         quiet = TRUE), silent = TRUE)
  if (inherits(res, "try-error")) {
    if (verbose) webchem_message("service_down")
    return(NA)
  }
  out <- tolower(jsonlite::fromJSON(rawToChar(res$content)))
  return(out)
}
