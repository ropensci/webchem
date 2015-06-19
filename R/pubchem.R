#' Retrieve Pubchem Id (CID)
#'
#' Return CompoundID (CID) for a search query, see \url{https://pubchem.ncbi.nlm.nih.gov/}.
#' @import XML RCurl
#'
#' @param query charachter; search term.
#' @param first logical; If TRUE return only first result.
#' @param verbose logical; should a verbose output be printed on the console?
#' @param ... currently not used.
#' @return a character vector.
#'
#' @author Eduard Szoecs, \email{eduardszoecs@@gmail.com}
#' @export
#' @examples
#' \donttest{
#' # might fail if API is not available
#' get_cid('Triclosan')
#'
#' # multiple inputs
#' comp <- c('Triclosan', 'Aspirin')
#' sapply(comp, function(x) get_cid(x))
#' sapply(comp, function(x) get_cid(x, first = TRUE))
#' }
get_cid <- function(query, first = FALSE, verbose = TRUE, ...){
  if (length(query) > 1) {
    stop('Cannot handle multiple input strings.')
  }
  if (is.na(query)) {
    warning('Identifier is NA... Returning NA.')
    return(NA)
  }
  qurl <- paste("http://eutils.ncbi.nlm.nih.gov/entrez/eutils/esearch.fcgi?retmax=100000&db=pccompound&term=",
                query, sep = "")
  if (verbose)
    message(qurl)
  Sys.sleep(0.3)
  cont <- try(getURLContent(qurl, .opts = list(timeout = 3)), silent = TRUE)
  if (!inherits(cont, "try-error")) {
    h <- xmlParse(cont, useInternalNodes = TRUE)
    out <- rev(xpathSApply(h, "//IdList/Id", xmlValue))
  } else{
    warning('Problem with web service encountered... Returning NA.')
    return(NA)
  }
  # not found on ncbi
  if (length(out) == 0) {
    message("Not found. Returning NA.")
    return(NA)
  }
  if (first)
    out <- out[1]
  names(out) <- NULL
  return(out)
}



#' Retrieve compound information from pubchem CID
#'
#' Retrieve compound information from pubchem CID, see \url{https://pubchem.ncbi.nlm.nih.gov/}
#' @import RCurl XML
#' @param cid character; Pubchem ID (CID).
#' @param first logical; return only first list items?
#' That is: a list with entries of lenght 1 (for easy conversion in a data.frame)
#' @param verbose logical; should a verbose output be printed on the console?
#' @param ... currently not used.
#' @return a list with entries: CID (Pubchem ID), InChIKey, InChI,
#' synonyms, IUPACName, Canonical SMILES, Isomeric SMILES, MolecularFormula,
#' MolecularWeight, TotalFormalCharge, XlogP, HydrogenBondDonorCount,
#' HydrogenBondAcceptorCount, Complexity,  HeavyAtomCount, AtomChiralCount,
#' AtomChiralDefCount, AtomChiralUndefCount, BondChiralCount, BondChiralDefCount,
#' BondChiralUndefCount, IsotopeAtomCount, CovalentUnitCount, TautomerCount
#' @author Eduard Szoecs, \email{eduardszoecs@@gmail.com}
#' @seealso \code{\link{get_cid}} to retrieve Pubchem IDs.
#' @export
#' @examples
#' \donttest{
#' # might fail if API is not available
#' cid <- get_cid('Triclosan')
#' cid_compinfo(cid[1])
#'
#' ###
#' # multiple CIDS
#' comp <- c('Triclosan', 'Aspirin')
#' cids <- sapply(comp, function(x) get_cid(x, first = TRUE))
#' (ll <- lapply(cids, cid_compinfo, first = TRUE))
#' # as mtrix
#' do.call(rbind, ll)
#' }
cid_compinfo <- function(cid, first = FALSE, verbose = TRUE, ...){
  if (length(cid) > 1) {
    stop('Cannot handle multiple input strings.')
  }
  baseurl <- "http://eutils.ncbi.nlm.nih.gov/entrez/eutils/esummary.fcgi?retmax=100000&db=pccompound"
  qurl <- paste0(baseurl, '&ID=', cid)
  if (verbose)
    message(qurl)
  Sys.sleep(0.3)
  h <- try(xmlParse(qurl, isURL = TRUE), silent = TRUE)
  if (!inherits(h, "try-error")) {
    CID <- xpathSApply(h, '//Id', xmlValue)
    InChIKey <-  xpathSApply(h, "//Item[@Name='InChIKey']", xmlValue)
    InChI <- xpathSApply(h, "//Item[@Name='InChI']", xmlValue)
    synonyms <- xpathSApply(h, "//Item[@Name='SynonymList']/Item", xmlValue)
    IUPACName <- xpathSApply(h, "//Item[@Name='IUPACName']", xmlValue)
    CanonicalSmiles <- xpathSApply(h, "//Item[@Name='CanonicalSmiles']", xmlValue)
    IsomericSmiles <- xpathSApply(h, "//Item[@Name='IsomericSmiles']", xmlValue)
    RotatableBondCount <- xpathSApply(h, "//Item[@Name='RotatableBondCount']", xmlValue)
    MolecularFormula <- xpathSApply(h, "//Item[@Name='MolecularFormula']", xmlValue)
    MolecularWeight <- xpathSApply(h, "//Item[@Name='MolecularWeight']", xmlValue)
    TotalFormalCharge <- xpathSApply(h, "//Item[@Name='TotalFormalCharge']", xmlValue)
    XLogP <- xpathSApply(h, "//Item[@Name='XLogP']", xmlValue)
    HydrogenBondDonorCount <- xpathSApply(h, "//Item[@Name='HydrogenBondDonorCount']", xmlValue)
    HydrogenBondAcceptorCount <- xpathSApply(h, "//Item[@Name='HydrogenBondAcceptorCount']", xmlValue)
    Complexity <- xpathSApply(h, "//Item[@Name='Complexity']", xmlValue)
    HeavyAtomCount <- xpathSApply(h, "//Item[@Name='HeavyAtomCount']", xmlValue)
    AtomChiralCount <- xpathSApply(h, "//Item[@Name='AtomChiralCount']", xmlValue)
    AtomChiralDefCount <- xpathSApply(h, "//Item[@Name='AtomChiralDefCount']", xmlValue)
    AtomChiralUndefCount <- xpathSApply(h, "//Item[@Name='AtomChiralUndefCount']", xmlValue)
    BondChiralCount <- xpathSApply(h, "//Item[@Name='BondChiralCount']", xmlValue)
    BondChiralDefCount <- xpathSApply(h, "//Item[@Name='BondChiralDefCount']", xmlValue)
    BondChiralUndefCount <- xpathSApply(h, "//Item[@Name='BondChiralUndefCount']", xmlValue)
    IsotopeAtomCount <- xpathSApply(h, "//Item[@Name='IsotopeAtomCount']", xmlValue)
    CovalentUnitCount <- xpathSApply(h, "//Item[@Name='CovalentUnitCount']", xmlValue)
    TautomerCount <- xpathSApply(h, "//Item[@Name='TautomerCount']", xmlValue)
    out <- list(CID = CID, InChIKey = InChIKey, InChI = InChI, synonyms = synonyms,
                IUPACName = IUPACName, CanonicalSmiles = CanonicalSmiles,
                IsomericSmiles = IsomericSmiles, RotatableBondCount = RotatableBondCount,
                MolecularFormula = MolecularFormula, MolecularWeight = MolecularWeight,
                TotalFormalCharge = TotalFormalCharge, XLogP = XLogP,
                HydrogenBondDonorCount = HydrogenBondDonorCount,
                HydrogenBondAcceptorCount = HydrogenBondAcceptorCount,
                Complexity = Complexity, HeavyAtomCount = HeavyAtomCount,
                AtomChiralCount = AtomChiralCount, AtomChiralDefCount = AtomChiralDefCount,
                AtomChiralUndefCount = AtomChiralUndefCount, BondChiralCount = BondChiralCount,
                BondChiralDefCount = BondChiralDefCount, BondChiralUndefCount = BondChiralUndefCount,
                IsotopeAtomCount = IsotopeAtomCount, CovalentUnitCount = CovalentUnitCount,
                TautomerCount = TautomerCount)
    if (first)
      out <- lapply(out, function(x) x[1])
  } else {
    warning('CID not found... Returning NA.')
    return(NA)
  }
  return(out)
}




