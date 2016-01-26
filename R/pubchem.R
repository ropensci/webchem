#' Retrieve Pubchem Id (CID)
#'
#' Return CompoundID (CID) for a search query, see \url{https://pubchem.ncbi.nlm.nih.gov/}.
#' @import xml2
#'
#' @param query charachter; search term.
#' @param first logical; If TRUE return only first result.
#' @param verbose logical; should a verbose output be printed on the console?
#' @param ... currently not used.
#' @return a character vector.
#'
#' @references Wang, Y., J. Xiao, T. O. Suzek, et al. 2009 PubChem: A Public Information System for
#' Analyzing Bioactivities of Small Molecules. Nucleic Acids Research 37: 623–633.
#'
#' Kim, Sunghwan, Paul A. Thiessen, Evan E. Bolton, et al. 2016
#' PubChem Substance and Compound Databases. Nucleic Acids Research 44(D1): D1202–D1213.
#' @author Eduard Szoecs, \email{eduardszoecs@@gmail.com}
#' @export
#' @examples
#' \donttest{
#' # might fail if API is not available
#' get_pcid('Triclosan')
#'
#' # multiple inputs
#' comp <- c('Triclosan', 'Aspirin')
#' sapply(comp, function(x) get_pcid(x))
#' sapply(comp, function(x) get_pcid(x, first = TRUE))
#' }
get_pcid <- function(query, first = FALSE, verbose = TRUE, ...){
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
  Sys.sleep( rgamma(1, shape = 15, scale = 1/10))
  h <- try(read_xml(qurl), silent = TRUE)
  if (inherits(h, "try-error")) {
    warning('Problem with web service encountered... Returning NA.')
    return(NA)
  }
  out <- rev(xml_text(xml_find_all(h, "//IdList/Id")))
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
#' @import xml2
#' @importFrom stats rgamma
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
#' BondChiralUndefCount, IsotopeAtomCount, CovalentUnitCount, TautomerCount and source_url
#' @author Eduard Szoecs, \email{eduardszoecs@@gmail.com}
#' @seealso \code{\link{get_pcid}} to retrieve Pubchem IDs.
#' @references Wang, Y., J. Xiao, T. O. Suzek, et al. 2009 PubChem: A Public Information System for
#' Analyzing Bioactivities of Small Molecules. Nucleic Acids Research 37: 623–633.
#'
#' Kim, Sunghwan, Paul A. Thiessen, Evan E. Bolton, et al. 2016
#' PubChem Substance and Compound Databases. Nucleic Acids Research 44(D1): D1202–D1213.
#' @export
#' @examples
#' \donttest{
#' # might fail if API is not available
#' cid <- get_pcid('Triclosan')
#' pc_compinfo(cid[1])
#'
#' ###
#' # multiple CIDS
#' comp <- c('Triclosan', 'Aspirin')
#' cids <- sapply(comp, function(x) get_pcid(x, first = TRUE))
#' (ll <- lapply(cids, pc_compinfo, first = TRUE))
#' # as mtrix
#' do.call(rbind, ll)
#' }
pc_compinfo <- function(cid, first = FALSE, verbose = TRUE, ...){
  # cid <- '5564'
  if (length(cid) > 1) {
    stop('Cannot handle multiple input strings.')
  }
  baseurl <- "http://eutils.ncbi.nlm.nih.gov/entrez/eutils/esummary.fcgi?retmax=100000&db=pccompound"
  qurl <- paste0(baseurl, '&ID=', cid)
  if (verbose)
    message(qurl)
  Sys.sleep( rgamma(1, shape = 15, scale = 1/10))
  h <- try(read_xml(qurl), silent = TRUE)
  if (inherits(h, "try-error")) {
    warning('Problem with web service encountered... Returning NA.')
    return(NA)
  }
  if (length(xml_find_all(h, '//ERROR')) > 0) {
    if (verbose)
      warning("Problem encountered : '", xml_text(xml_find_all(h, '//ERROR')),
              "'.\n Returning NA.")
    return(NA)
  }
  CID <- xml_text(xml_find_all(h, '//Id'))
  InChIKey <-  xml_text(xml_find_all(h, "//Item[@Name='InChIKey']"))
  InChI <- xml_text(xml_find_all(h, "//Item[@Name='InChI']"))
  synonyms <- xml_text(xml_find_all(h, "//Item[@Name='SynonymList']/Item"))
  IUPACName <- xml_text(xml_find_all(h, "//Item[@Name='IUPACName']"))
  CanonicalSmiles <- xml_text(xml_find_all(h, "//Item[@Name='CanonicalSmiles']"))
  IsomericSmiles <- xml_text(xml_find_all(h, "//Item[@Name='IsomericSmiles']"))
  RotatableBondCount <- xml_text(xml_find_all(h, "//Item[@Name='RotatableBondCount']"))
  MolecularFormula <- xml_text(xml_find_all(h, "//Item[@Name='MolecularFormula']"))
  MolecularWeight <- xml_text(xml_find_all(h, "//Item[@Name='MolecularWeight']"))
  TotalFormalCharge <- xml_text(xml_find_all(h, "//Item[@Name='TotalFormalCharge']"))
  XLogP <- xml_text(xml_find_all(h, "//Item[@Name='XLogP']"))
  HydrogenBondDonorCount <- xml_text(xml_find_all(h, "//Item[@Name='HydrogenBondDonorCount']"))
  HydrogenBondAcceptorCount <- xml_text(xml_find_all(h, "//Item[@Name='HydrogenBondAcceptorCount']"))
  Complexity <- xml_text(xml_find_all(h, "//Item[@Name='Complexity']"))
  HeavyAtomCount <- xml_text(xml_find_all(h, "//Item[@Name='HeavyAtomCount']"))
  AtomChiralCount <- xml_text(xml_find_all(h, "//Item[@Name='AtomChiralCount']"))
  AtomChiralDefCount <- xml_text(xml_find_all(h, "//Item[@Name='AtomChiralDefCount']"))
  AtomChiralUndefCount <- xml_text(xml_find_all(h, "//Item[@Name='AtomChiralUndefCount']"))
  BondChiralCount <- xml_text(xml_find_all(h, "//Item[@Name='BondChiralCount']"))
  BondChiralDefCount <- xml_text(xml_find_all(h, "//Item[@Name='BondChiralDefCount']"))
  BondChiralUndefCount <- xml_text(xml_find_all(h, "//Item[@Name='BondChiralUndefCount']"))
  IsotopeAtomCount <- xml_text(xml_find_all(h, "//Item[@Name='IsotopeAtomCount']"))
  CovalentUnitCount <- xml_text(xml_find_all(h, "//Item[@Name='CovalentUnitCount']"))
  TautomerCount <- xml_text(xml_find_all(h, "//Item[@Name='TautomerCount']"))

  source_url = paste0("https://pubchem.ncbi.nlm.nih.gov/compound/", CID)
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
              TautomerCount = TautomerCount, source_url = source_url)
  if (first)
    out <- lapply(out, function(x) x[1])
  return(out)
}




