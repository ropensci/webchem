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
#'
#' ###
#' # multiple inputs
#' sapply(c('Aspirin', 'Triclosan'), get_csid, token = token)
#' }
get_csid <- function(query, token = NULL, first = FALSE, verbose = TRUE,  ...){
  if (length(query) > 1) {
    stop('Cannot handle multiple input strings.')
  }
  if (is.na(query)) {
    warning('Identifier is NA... Returning NA.')
    return(NA)
  }
  baseurl <- 'http://www.chemspider.com/Search.asmx/SimpleSearch?'
  qurl <- paste0(baseurl, 'query=', query, '&token=', token)
  if (verbose)
    message(qurl, '\n')
  Sys.sleep(0.1)
  h <- try(xmlParse(qurl, isURL = TRUE, useInternalNodes = TRUE), silent = TRUE)
  if (!inherits(h, "try-error")) {
    out <- unlist(xmlToList(h))
  } else {
    warning('Problem with web service encountered... Returning NA.')
    return(NA)
  }
  if (length(out) == 0) {
    message('No csid found... Returning NA.')
    return(NA)
  }
  if (first)
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
#' cs_compinfo(csid, token)
#'
#' ###
#' # multiple inputs
#' csids <- sapply(c('Aspirin', 'Triclosan'), get_csid, token = token)
#' # fails:
#' # cs_compinfo(csids, token = token)
#' (ll <- lapply(csids, cs_compinfo, token = token))
#' # return a list, convert to matrix:
#' do.call(rbind, ll)
#' }
cs_compinfo <- function(csid, token, verbose = TRUE, ...){
  if (length(csid) > 1) {
    stop('Cannot handle multiple input strings.')
  }
  baseurl <- 'http://www.chemspider.com/Search.asmx/GetCompoundInfo?'
  qurl <- paste0(baseurl, 'CSID=', csid, '&token=', token)
  if (verbose)
    message(qurl)
  Sys.sleep(0.1)
  h <- try(xmlParse(qurl, isURL = TRUE), silent = TRUE)
  if (!inherits(h, "try-error")) {
    out <- unlist(xmlToList(h))
  } else {
    warning('CSID not found... Returning NA.')
    return(NA)
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
#' @seealso \code{\link{get_csid}} to retrieve ChemSpider IDs,
#' \code{\link{csid_compinfo}} for extended compound information.
#' @export
#' @examples
#' \dontrun{
#' # Fails because no TOKEN is included
#' token <- '<YOUR-SECURITY-TOKEN>'
#' # convert CAS to CSID
#' csid <- get_csid("Triclosan", token = token)
#' # get SMILES from CSID
#' cs_extcompinfo(csid, token)
#'
#' ###
#' # multiple inpits
#' csids <- sapply(c('Aspirin', 'Triclosan'), get_csid, token = token)
#' # fails:
#' # cs_extcompinfo(csids, token = token)
#' (ll <- lapply(csids, cs_extcompinfo, token = token))
#' # to matrix
#' do.call(rbind, ll)
#' }
cs_extcompinfo <- function(csid, token, verbose = TRUE, ...){
  if (length(csid) > 1) {
    stop('Cannot handle multiple input strings.')
  }
  baseurl <- 'http://www.chemspider.com/MassSpecAPI.asmx/GetExtendedCompoundInfo?'
  qurl <- paste0(baseurl, 'CSID=', csid, '&token=', token)
  if (verbose)
    message(qurl)
  Sys.sleep(0.1)
  h <- try(xmlParse(qurl, isURL = TRUE), silent = TRUE)
  if (!inherits(h, "try-error")) {
    out <- unlist(xmlToList(h))
  } else{
    warning('CSID not found... Returning NA.')
    return(NA)
  }
  return(out)
}


# cs_convert <- function(query, from = c('csid', 'inchi', 'inchikey', 'smiles', 'mol'),
#                        to = c('csid', 'inchi',  'inchikey', 'smiles', 'mol'),
#                        token) {
#
# }



#' Convert a CSID to a Molfile
#' @import xml2
#'
#' @param csid character,  ChemSpider ID.
#' @param token character; security token.
#' @param parse should the molfile be parsed to a R object?
#' If \code{FALSE} the raw mol is returned as string.
#' @param verbose logical; should a verbose output be printed on the console?
#' @param ... currently not used.
#'
#' @return If parse = FALSE then a charactersting, else a RMol-object (from \code{\link{parse_mol}})
#'
#' @seealso \code{\link{parse_mol}} for a description of the Mol R Object.
#' @note A security token is neeeded. Please register at RSC
#' \url{https://www.rsc.org/rsc-id/register}
#' for a security token.
#' @author Eduard Szoecs, \email{eduardszoecs@@gmail.com}
#' @examples
#' \dontrun{
#' # Fails because no TOKEN is included
#' token <- '<YOUR-SECURITY-TOKEN>'
#' # convert CAS to CSID
#' tric_mol <- cs_csid_mol(5363, token = token)
#' tric_mol
#' cs_csid_mol(5363, token = token, parse = FALSE)
#' }
cs_csid_mol <- function(csid, token, parse = TRUE, verbose = TRUE, ...){
  if (length(csid) > 1) {
    stop('Cannot handle multiple input strings.')
  }
  baseurl <- 'http://www.chemspider.com/InChI.asmx/CSIDToMol?'
  qurl <- paste0(baseurl, 'csid=', csid, '&token=', token)
  if (verbose)
    message(qurl)
  Sys.sleep(0.1)
  h <- try(read_xml(qurl), silent = TRUE)
  if (inherits(h, "try-error")) {
    warning('CSID not found... Returning NA.')
    out <- NA
  } else {
    mol <- xml_text(h)
    if (!parse) {
      out <- mol
    } else {
      out <- parse_mol(mol)
    }
  }
  return(out)
}



#' Check if input is a valid inchikey using ChemSpider API
#'
#' @param x character; input string
#' @param verbose logical; print messages during processing to console?
#' @return a logical
#'
#' @seealso \code{\link{is.inchikey}} for a pure-R implementation.
#' @author Eduard Szoecs, \email{eduardszoecs@@gmail.com}
#' @export
#' @examples
#' is.inchikey_cs('BQJCRHHNABKAKU-KBQPJGBKSA-N')
#' is.inchikey_cs('BQJCRHHNABKAKU-KBQPJGBKSA')
#' is.inchikey_cs('BQJCRHHNABKAKU-KBQPJGBKSA-5')
#' is.inchikey_cs('BQJCRHHNABKAKU-KBQPJGBKSA-n')
#' is.inchikey_cs('BQJCRHHNABKAKU/KBQPJGBKSA/N')
#' is.inchikey_cs('BQJCRHHNABKAKU-KBQPJGBKXA-N')
#' is.inchikey_cs('BQJCRHHNABKAKU-KBQPJGBKSB-N')
is.inchikey_cs <- function(x, verbose = TRUE){
  # x <- 'BQJCRHHNABKAKU-KBQPJGBKSA'
  if (length(x) > 1) {
    stop('Cannot handle multiple input strings.')
  }
  baseurl <- 'http://www.chemspider.com/InChI.asmx/IsValidInChIKey?'
  qurl <- paste0(baseurl, 'inchi_key=', x)
  if (verbose)
    message(qurl)
  Sys.sleep(0.1)
  h <- try(read_xml(qurl), silent = TRUE)
  if (inherits(h, "try-error")) {
    warning('Problem with webservice... Returning NA.')
    out <- NA
  } else {
    out <- as.logical(xml_text(h))
  }
  return(out)
}
