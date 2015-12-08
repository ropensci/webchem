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
#'
#' @param csid character, ChemSpider ID.
#' @param token character; security token.
#' @param verbose logical; should a verbose output be printed on the console?
#' @param ... currently not used.
#' @return a list of four, with entries: CSID (ChemSpider ID), InChI,
#'   InChIKey and SMILES string.
#'
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



#' Convert identifiers using ChemSpider
#'
#' @param query character; query ID.
#' @param from character; type of query ID.
#' @param to character; type to convert to.
#' @param token character; security token. Converting from csid to mol requires a token.
#' @param verbose logical; should a verbose output be printed on the console?
#' @param ... futher arguments passed. Currently onl \code{parse}, see also \code{\link{cs_csid_mol}}
#' @return Depends on to. if \code{to = 'mol'} then an RMol-Object, else a character string.
#'
#' @seealso \code{\link{parse_mol}} for a description of the Mol R Object.
#' @note A security token is neeeded. Please register at RSC
#' \url{https://www.rsc.org/rsc-id/register}
#' for a security token.
#' @author Eduard Szoecs, \email{eduardszoecs@@gmail.com}
#' @export
#'
#' @examples
#' \donttest{
#' # might fail if API is not available
#' cs_convert('BQJCRHHNABKAKU-KBQPJGBKSA-N', from = 'inchikey', to = 'csid')
#' cs_convert('BQJCRHHNABKAKU-KBQPJGBKSA-N', from = 'inchikey', to = 'inchi')
#' cs_convert('BQJCRHHNABKAKU-KBQPJGBKSA-N', from = 'inchikey', to = 'mol')
#'}
cs_convert <- function(query, from = c('csid', 'inchikey', 'inchi', 'smiles'),
                       to = c('csid', 'inchikey', 'inchi', 'smiles', 'mol'),
                       verbose = TRUE, token = NULL, ...) {
  if (length(query) > 1) {
    stop('Cannot handle multiple input strings.')
  }
  from <- match.arg(from)
  to <- match.arg(to)
  from_to <- paste(from, to , sep = '_')
  if (from_to == 'csid_mol' & is.null(token)) {
    stop('Need token for this conversion!')
  }
  # allowed combinations
  comb <- c('csid_mol', 'inchikey_csid', 'inchikey_inchi', 'inchikey_mol',
            'inchi_csid', 'inchi_inchikey', 'inchi_mol', 'inchi_smiles','smiles_inchi')
  if (!from_to %in% comb) {
    stop('Conversion from ', from, ' to ', to, ' currently not supported')
  }
  out <- switch(from_to,
         csid_mol = cs_csid_mol(csid = query, token = token, verbose = verbose, ...),
         inchikey_csid = cs_inchikey_csid(inchikey = query, verbose = verbose, ...),
         inchikey_inchi = cs_inchikey_inchi(inchikey = query, verbose = verbose, ...),
         inchikey_mol = cs_inchikey_mol(inchikey = query, verbose = verbose, ...),
         inchi_csid = cs_inchi_csid(inchi = query, verbose = verbose, ...),
         inchi_inchikey = cs_inchi_inchikey(inchi = query, verbose = verbose, ...),
         inchi_mol = cs_inchi_mol(inchi = query, verbose = verbose, ...),
         inchi_smiles = cs_inchi_smiles(inchi = query, verbose = verbose, ...),
         smiles_inchi = cs_smiles_inchi(smiles = query, verbose = verbose, ...)
         )
  return(out)
}



#' Convert a CSID to a Molfile
#' @import xml2
#'
#' @param csid character,  ChemSpiderID.
#' @param token character; security token.
#' @param parse should the molfile be parsed to a R object?
#' If \code{FALSE} the raw mol is returned as string.
#' @param verbose logical; should a verbose output be printed on the console?
#' @param ... currently not used.
#'
#' @return If parse = FALSE then a charactersting,
#'   else a RMol-object (from \code{\link{parse_mol}})
#'
#' @seealso \code{\link{parse_mol}} for a description of the Mol R Object.
#' @note A security token is neeeded. Please register at RSC
#' \url{https://www.rsc.org/rsc-id/register}
#' for a security token.
#' @author Eduard Szoecs, \email{eduardszoecs@@gmail.com}
#' @export
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



#' Convert a InChIKey to CSID
#' @import xml2
#'
#' @param inchikey character,  InChIKey
#' @param verbose logical; should a verbose output be printed on the console?
#' @param ... currently not used.
#'
#' @return A CSID.
#'
#' @author Eduard Szoecs, \email{eduardszoecs@@gmail.com}
#' @export
#' @examples
#' \donttest{
#' # might fail if API is not available
#' # convert CAS to CSID
#' cs_inchikey_csid('BQJCRHHNABKAKU-KBQPJGBKSA-N')
#' }
cs_inchikey_csid <- function(inchikey, verbose = TRUE, ...){
  # inchkey <- 'BQJCRHHNABKAKU-KBQPJGBKSA-N'
  if (length(inchikey) > 1) {
    stop('Cannot handle multiple input strings.')
  }
  baseurl <- 'http://www.chemspider.com/InChI.asmx/InChIKeyToCSID?'
  qurl <- paste0(baseurl, 'inchi_key=', inchikey)
  if (verbose)
    message(qurl)
  Sys.sleep(0.1)
  h <- try(read_xml(qurl), silent = TRUE)
  if (inherits(h, "try-error")) {
    warning('inchikey not found... Returning NA.')
    out <- NA
  } else {
    out <- xml_text(h)
  }
  return(out)
}


#' Convert a InChIKey to InChI
#' @import xml2
#'
#' @param inchikey character,  InChIKey
#' @param verbose logical; should a verbose output be printed on the console?
#' @param ... currently not used.
#' @return character; InChI
#'
#' @author Eduard Szoecs, \email{eduardszoecs@@gmail.com}
#' @export
#' @examples
#' \donttest{
#' # might fail if API is not available
#' cs_inchikey_inchi('BQJCRHHNABKAKU-KBQPJGBKSA-N')
#' }
cs_inchikey_inchi <- function(inchikey, verbose = TRUE, ...){
  # inchikey <- 'BQJCRHHNABKAKU-KBQPJGBKSA-N'
  if (length(inchikey) > 1) {
    stop('Cannot handle multiple input strings.')
  }
  baseurl <- 'http://www.chemspider.com/InChI.asmx/InChIKeyToInChI?'
  qurl <- paste0(baseurl, 'inchi_key=', inchikey)
  if (verbose)
    message(qurl)
  Sys.sleep(0.1)
  h <- try(read_xml(qurl), silent = TRUE)
  if (inherits(h, "try-error")) {
    warning('inchikey not found... Returning NA.')
    out <- NA
  } else {
    out <- xml_text(h)
  }
  return(out)
}


#' Convert a InChIkey to a Molfile
#' @import xml2
#'
#' @param inchikey character,  A InChIKey.
#' @param parse should the molfile be parsed to a R object?
#' If \code{FALSE} the raw mol is returned as string.
#' @param verbose logical; should a verbose output be printed on the console?
#' @param ... currently not used.
#'
#' @return If parse = FALSE then a charactersting,
#'   else a RMol-object (from \code{\link{parse_mol}})
#'
#' @seealso \code{\link{parse_mol}} for a description of the Mol R Object.
#' @author Eduard Szoecs, \email{eduardszoecs@@gmail.com}
#' @export
#' @examples
#' \donttest{
#' # might fail if API is not available
#' tric_mol <- cs_inchikey_mol('BQJCRHHNABKAKU-KBQPJGBKSA-N')
#' tric_mol
#' cs_inchikey_mol('BQJCRHHNABKAKU-KBQPJGBKSA-N',parse = FALSE)
#' }
cs_inchikey_mol <- function(inchikey, parse = TRUE, verbose = TRUE, ...){
  # inchikey <- 'BQJCRHHNABKAKU-KBQPJGBKSA-N'
  if (length(inchikey) > 1) {
    stop('Cannot handle multiple input strings.')
  }
  baseurl <- 'http://www.chemspider.com/InChI.asmx/InChIKeyToMol?'
  qurl <- paste0(baseurl, 'inchi_key=', inchikey)
  if (verbose)
    message(qurl)
  Sys.sleep(0.1)
  h <- try(read_xml(qurl), silent = TRUE)
  if (inherits(h, "try-error")) {
    warning('inchikey not found... Returning NA.')
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


#' Convert a InChI to CSID
#' @import xml2 httr
#'
#' @param inchi character,  InChI
#' @param verbose logical; should a verbose output be printed on the console?
#' @param ... currently not used.
#'
#' @return A CSID.
#'
#' @author Eduard Szoecs, \email{eduardszoecs@@gmail.com}
#' @export
#' @examples
#' \donttest{
#' # might fail if API is not available
#' inchi <-  "InChI=1S/C17H19NO3/c1-18-7-6-17-10-3-5-13(20)16(17)21-15-12(19)4-
#' 2-9(14(15)17)8-11(10)18/h2-5,10-11,13,16,19-20H,6-8H2,1H3/t10-,11+,13-,16-,17-/m0/s1"
#' # convert InChI to CSID
#' cs_inchi_csid(inchi)
#' }
cs_inchi_csid <- function(inchi, verbose = TRUE, ...){
  if (length(inchi) > 1) {
    stop('Cannot handle multiple input strings.')
  }
  baseurl <- 'http://www.chemspider.com/InChI.asmx/InChIToCSID'
  if (verbose)
    message('Querrying ', baseurl)
  Sys.sleep(0.1)
  res <- try(POST(baseurl, body = list(inchi = inchi), encode = 'form'),
             silent = TRUE)
  if (inherits(res, "try-error")) {
    warning('Problem with service... Returning NA.')
    out <- NA
  } else {
    out <- try(read_xml(content(res, 'raw')), silent = TRUE)
    if (inherits(out, "try-error")) {
      warning('inchi not found... Returning NA.')
      out <- NA
    } else {
      out <- xml_text(out)
    }
  }
  return(out)
}


#' Convert a InChI to InChiKey
#' @import xml2 httr
#'
#' @param inchi character,  InChI
#' @param verbose logical; should a verbose output be printed on the console?
#' @param ... currently not used.
#'
#' @return A InChiKey.
#'
#' @author Eduard Szoecs, \email{eduardszoecs@@gmail.com}
#' @export
#' @examples
#' \donttest{
#' # might fail if API is not available
#' inchi <-  "InChI=1S/C17H19NO3/c1-18-7-6-17-10-3-5-13(20)16(17)21-15-12(19)4-
#' 2-9(14(15)17)8-11(10)18/h2-5,10-11,13,16,19-20H,6-8H2,1H3/t10-,11+,13-,16-,17-/m0/s1"
#' # convert InChI to CSID
#' cs_inchi_inchikey(inchi)
#' }
cs_inchi_inchikey <- function(inchi, verbose = TRUE, ...){
  if (length(inchi) > 1) {
    stop('Cannot handle multiple input strings.')
  }
  baseurl <- 'http://www.chemspider.com/InChI.asmx/InChIToInChIKey'
  if (verbose)
    message('Querrying ', baseurl)
  Sys.sleep(0.1)
  res <- try(POST(baseurl, body = list(inchi = inchi), encode = 'form'),
             silent = TRUE)
  if (inherits(res, "try-error")) {
    warning('Problem with service... Returning NA.')
    out <- NA
  } else {
    out <- try(read_xml(content(res, 'raw')), silent = TRUE)
    if (inherits(out, "try-error")) {
      warning('inchi not found... Returning NA.')
      out <- NA
    } else {
      out <- xml_text(out)
    }
  }
  return(out)
}



#' Convert a InChI to Molfile
#' @import xml2 httr
#'
#' @param inchi character,  InChI
#' @param parse should the molfile be parsed to a R object?
#' If \code{FALSE} the raw mol is returned as string.
#' @param verbose logical; should a verbose output be printed on the console?
#' @param ... currently not used.
#'
#' @return If parse = FALSE then a charactersting,
#'   else a RMol-object (from \code{\link{parse_mol}})
#'
#' @author Eduard Szoecs, \email{eduardszoecs@@gmail.com}
#' @export
#' @examples
#' \donttest{
#' # might fail if API is not available
#' inchi <-  paste0("InChI=1S/C17H19NO3/c1-18-7-6-17-10-3-5-13(20)16(17)21-15-12(19)4-",
#' "2-9(14(15)17)8-11(10)18/h2-5,10-11,13,16,19-20H,6-8H2,1H3/t10-,11+,13-,16-,17-/m0/s1")
#' # convert InChI to CSID
#' cs_inchi_mol(inchi)
#' cs_inchi_mol(inchi, parse = FALSE)
#' }
cs_inchi_mol <- function(inchi, parse = TRUE, verbose = TRUE, ...){
  if (length(inchi) > 1) {
    stop('Cannot handle multiple input strings.')
  }
  baseurl <- 'http://www.chemspider.com/InChI.asmx/InChIToMol'
  if (verbose)
    message('Querrying ', baseurl)
  Sys.sleep(0.1)
  res <- try(POST(baseurl, body = list(inchi = inchi), encode = 'form'),
             silent = TRUE)
  if (inherits(res, "try-error")) {
    warning('Problem with service... Returning NA.')
    out <- NA
  } else {
    out <- try(read_xml(content(res, 'raw')), silent = TRUE)
    if (inherits(out, "try-error")) {
      warning('inchi not found... Returning NA.')
      out <- NA
    } else {
      out <- xml_text(out)
      if (parse) {
        out <- parse_mol(out)
      }
    }
  }
  return(out)
}


#' Convert a InChI to SMILES
#' @import xml2 httr
#'
#' @param inchi character,  InChI
#' @param verbose logical; should a verbose output be printed on the console?
#' @param ... currently not used.
#'
#' @return A SMILES string
#'
#' @author Eduard Szoecs, \email{eduardszoecs@@gmail.com}
#' @export
#' @examples
#' \donttest{
#' # might fail if API is not available
#' inchi <-  "InChI=1S/C17H19NO3/c1-18-7-6-17-10-3-5-13(20)16(17)21-15-12(19)4-
#' 2-9(14(15)17)8-11(10)18/h2-5,10-11,13,16,19-20H,6-8H2,1H3/t10-,11+,13-,16-,17-/m0/s1"
#' # convert InChI to CSID
#' cs_inchi_smiles(inchi)
#' }
cs_inchi_smiles <- function(inchi, verbose = TRUE, ...){
  if (length(inchi) > 1) {
    stop('Cannot handle multiple input strings.')
  }
  baseurl <- 'http://www.chemspider.com/InChI.asmx/InChIToSMILES'
  if (verbose)
    message('Querrying ', baseurl)
  Sys.sleep(0.1)
  res <- try(POST(baseurl, body = list(inchi = inchi), encode = 'form'),
             silent = TRUE)
  if (inherits(res, "try-error")) {
    warning('Problem with service... Returning NA.')
    out <- NA
  } else {
    out <- try(read_xml(content(res, 'raw')), silent = TRUE)
    if (inherits(out, "try-error")) {
      warning('inchi not found... Returning NA.')
      out <- NA
    } else {
      out <- xml_text(out)
    }
  }
  return(out)
}



#' Convert a SMILES to InChI
#' @import xml2 httr
#'
#' @param smiles character, A SMILES string
#' @param verbose logical; should a verbose output be printed on the console?
#' @param ... currently not used.
#'
#' @return A SMILES string
#'
#' @author Eduard Szoecs, \email{eduardszoecs@@gmail.com}
#' @export
#' @examples
#' \donttest{
#' # might fail if API is not available
#' smiles <- "CN1CC[C@]23[C@H]4C=C[C@@H]([C@@H]3Oc3c(ccc(C[C@@H]14)c23)O)O"
#' # convert smiles to inchi
#' cs_smiles_inchi(smiles)
#' }
cs_smiles_inchi <- function(smiles, verbose = TRUE, ...){
  if (length(smiles) > 1) {
    stop('Cannot handle multiple input strings.')
  }
  baseurl <- 'http://www.chemspider.com/InChI.asmx/SMILESToInChI'
  if (verbose)
    message('Querrying ', baseurl)
  Sys.sleep(0.1)
  res <- try(POST(baseurl, body = list(smiles = smiles), encode = 'form'),
             silent = TRUE)
  if (inherits(res, "try-error")) {
    warning('Problem with service... Returning NA.')
    out <- NA
  } else {
    out <- try(read_xml(content(res, 'raw')), silent = TRUE)
    if (inherits(out, "try-error")) {
      warning('inchi not found... Returning NA.')
      out <- NA
    } else {
      out <- xml_text(out)
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
#' \donttest{
#' # might fail if API is not available
#' is.inchikey_cs('BQJCRHHNABKAKU-KBQPJGBKSA-N')
#' is.inchikey_cs('BQJCRHHNABKAKU-KBQPJGBKSA')
#' is.inchikey_cs('BQJCRHHNABKAKU-KBQPJGBKSA-5')
#' is.inchikey_cs('BQJCRHHNABKAKU-KBQPJGBKSA-n')
#' is.inchikey_cs('BQJCRHHNABKAKU/KBQPJGBKSA/N')
#' is.inchikey_cs('BQJCRHHNABKAKU-KBQPJGBKXA-N')
#' is.inchikey_cs('BQJCRHHNABKAKU-KBQPJGBKSB-N')
#' }
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
