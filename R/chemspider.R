#' Retrieve ChemSpider ID
#'
#' Return Chemspider ID (CSID) for a search query, see \url{http://www.chemspider.com/}.
#' @import RCurl XML
#'
#' @param query charachter; search term.
#' @param token character; your security token.
#' @param first logical; If TRUE return only first result.
#' @param verbose logical; should a verbose output be printed on the console?
#' @param ... currently not used.
#' @return a character vector of class 'csid' with ChemSpider IDs.
#'
#' @note A security token is neeeded. Please register at RSC
#' \url{https://www.rsc.org/rsc-id/register}
#' for a security token.
#'
#' @author Eduard Szoecs, \email{eduardszoecs@@gmail.com}
#' @seealso \code{\link{csid_compinfo}} and \code{\link{csid_extcompinfo}} to
#' retrieve compound details from csid.
#' @export
#' @examples
#' \dontrun{
#' # Fails because no TOKEN is included
#' token <- '<YOUR-SECURITY-TOKEN>'
#' get_csid("Triclosan", token = token)
#' # [1] "5363"
#' # attr(,"class")
#' # [1] "csid"
#' get_csid("3380-34-5", token = token)
#' # multiple inputs
#' sapply(c('Aspirin', 'Triclosan'), get_csid, token = token)
#' }
get_csid <- function(query, token = NULL, first = FALSE, verbose = TRUE,  ...){
  if(length(query) > 1){
    stop('Cannot handle multiple input strings.')
  }
  baseurl <- 'http://www.chemspider.com/Search.asmx/SimpleSearch?'
  qurl <- paste0(baseurl, 'query=', query, '&token=', token)
  if(verbose)
    message(qurl, '\n')
  h <- try(xmlParse(qurl, isURL = TRUE, useInternalNodes = TRUE))
  if(!inherits(h, "try-error")){
    out <- unlist(xmlToList(h))
  } else{
    warning('Problem with web service encountered... Returning NA.')
    out < NA
  }
  if(length(out) == 0){
    warning('No csid found... Returning NA.')
    out <- NA
  }
  if(first)
    out <- out[1]
  names(out) <- NULL
  return(out)
}



#' Get record details (CSID, StdInChIKey, StdInChI, SMILES) by ChemSpider ID
#'
#' Get record details from ChemspiderId (CSID), see \url{http://www.chemspider.com/}
#' @import RCurl XML
#' @param csid character, ChemSpider ID.
#' @param token character; security token.
#' @param verbose logical; should a verbose output be printed on the console?
#' @param ... currently not used.
#' @return a list of four, with entries: CSID (ChemSpider ID), InChI, InChIKey and SMILES string.

#' @note A security token is neeeded. Please register at RSC
#' \url{https://www.rsc.org/rsc-id/register}
#' for a security token.
#' @author Eduard Szoecs, \email{eduardszoecs@@gmail.com}
#' @seealso \code{\link{get_csid}} to retrieve ChemSpider IDs,
#' \code{\link{csid_extcompinfo}} for extended compound information.
#' @export
#' @examples
#' \dontrun{
#' # Fails because no TOKEN is included
#' token <- '<YOUR-SECURITY-TOKEN>'
#' # convert CAS to CSID
#' csid <- get_csid("Triclosan", token = token)
#' csid_compinfo(csid, token)
#' # multiple inpits
#' csids <- get_csid("3380-34-5", token = token)
#' # fails:
#' # csid_compinfo(csids, token = token)
#' lapply(csids, csid_compinfo, token = token)
#' }
csid_compinfo <- function(csid, token, verbose = TRUE, ...){
  if(length(csid) > 1){
    stop('Cannot handle multiple input strings.')
  }
  baseurl <- 'http://www.chemspider.com/Search.asmx/GetCompoundInfo?'
  qurl <- paste0(baseurl, 'CSID=', csid, '&token=', token)
  if(verbose)
    message(qurl)
  h <- try(xmlParse(qurl, isURL = TRUE))
  if(!inherits(h, "try-error")){
    out <- unlist(xmlToList(h))
  } else{
    warning('Problem with web service encountered... Returning NA.')
    out < NA
  }
  if (length(out) == 0){
    message("Not found. Returning NA.")
    out <- NA
  }
  return(out)
}


#' Get extended record details by ChemSpider ID
#'
#' Get extended info from Chemspider, see \url{http://www.chemspider.com/}
#' @import RCurl XML
#' @param csid character,  ChemSpider ID.
#' @param token character; security token.
#' @param verbose logical; should a verbose output be printed on the console?
#' @param ... currently not used.
#' @return a list with entries: CSID (ChemSpider ID), MF (Molecular Formula),
#' SMILES string, InChI, InChIKey, Average Mass, Molecular weight, MonoisotopicMass,
#' NominalMass, ALogP, XLogP and the common Name.
#' @note A security token is neeeded. Please register at RSC
#' \url{https://www.rsc.org/rsc-id/register}
#' for a security token.
#' @author Eduard Szoecs, \email{eduardszoecs@@gmail.com}
#' #' @seealso \code{\link{get_csid}} to retrieve ChemSpider IDs,
#' \code{\link{csid_compinfo}} for extended compound information.
#' @export
#' @examples
#' \dontrun{
#' # Fails because no TOKEN is included
#' token <- '<YOUR-SECURITY-TOKEN>'
#' # convert CAS to CSID
#' csid <- get_csid("Triclosan", token = token)
#' # get SMILES from CSID
#' csid_extcompinfo(csid, token)
#' # multiple inpits
#' csids <- get_csid("3380-34-5", token = token)
#' # fails:
#' # csid_extcompinfo(csids, token = token)
#' lapply(csids, csid_extcompinfo, token = token)
#' }
csid_extcompinfo <- function(csid, token, verbose = TRUE, ...){
  if(length(csid) > 1){
    stop('Cannot handle multiple input strings.')
  }
  baseurl <- 'http://www.chemspider.com/MassSpecAPI.asmx/GetExtendedCompoundInfo?'
  qurl <- paste0(baseurl, 'CSID=', csid, '&token=', token)
  if(verbose)
    message(qurl)
  h <- try(xmlParse(qurl, isURL = TRUE))
  if(!inherits(h, "try-error")){
    out <- unlist(xmlToList(h))
  } else{
    warning('Problem with web service encountered... Returning NA.')
    out <- NA
  }
  if (length(out) == 0){
    message("Not found. Returning NA.")
    out <- NA
  }
  return(out)
}
