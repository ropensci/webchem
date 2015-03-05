#' Query Chemical Identifier Resolver
#' @import XML RCurl
#'
#' @param identifier character; chemical identifier.
#' @param representation character; what representation of the identifier should
#'  be returned. See details for possible representations.
#' @param resolver character; what resolver should be used? If NULL (default)
#'  the identifier type is detected and the different resolvers are used in turn.
#'  See details for possible resolvers.
#' @param first logical; If TRUE return only first result.
#' @param verbose logical; should a verbose output be printed on the console?
#' @param ... currently not used.
#' @return A character vector.
#' @details A interface to the Chemical Identifier Resolver (CIR).
#'  (\url{http://cactus.nci.nih.gov/chemical/structure_documentation}).
#'
#'  CIR can resolve can be of the following \code{identifier}: Chemical Names,
#'  IUPAC names,
#'  CAS Numbers, SMILES strings, IUPAC InChI/InChIKeys, NCI/CADD Identifiers,
#'  CACTVS HASHISY, NSC number, PubChem SID, ZINC Code, ChemSpider ID,
#'  ChemNavigator SID, eMolecule VID.
#'
#'  \code{cir_query()} can handle only a part of all possible conversions of CIR.
#'  Possible \code{representations} are:
#'  \itemize{
#'      \item \code{'smiles'}(SMILES strings),
#'      \item \code{'names'} (Names),
#'      \item \code{'cas'} (CAS numbers),
#'      \item \code{'stdinchikey'} (Standard InChIKey),
#'      \item \code{'stdinchi'} (Standard InChI),
#'      \item \code{'ficts'} (FICTS Identifier),
#'      \item \code{'ficus'} (FICuS Indetifier),
#'      \item \code{'uuuuu'} (uuuuu Identifier),
#'      \item \code{'mw'} (Molecular weight),
#'      \item \code{'monoisotopic_mass'} (Monoisotopic Mass),
#'      \item \code{'formula'} (Chemical Formula),
#'      \item \code{'chemspider_id'} (ChemSpider ID),
#'      \item \code{'pubchem_sid'} (PubChem SID),
#'      \item \code{'chemnavigator_sid'} (ChemNavigator SID),
#'      \item \code{'h_bond_donor_count'} (Number of Hydrogen Bond Donors),
#'      \item \code{'h_bond_acceptor_count'} (Number of Hydrogen Bond Acceptors),
#'      \item \code{'h_bond_center_count'} (Number of Hydrogen Bond Centers),
#'      \item \code{'rule_of_5_violation_count'} (Number of Rule of 5 Violations),
#'      \item \code{'rotor_count'} (Number of Freely Rotatable Bonds),
#'      \item \code{'effective_rotor_count'} (Number of Effectively Rotatable Bonds),
#'      \item \code{'ring_count'} (Number of Rings),
#'      \item \code{'ringsys_count'} (Number of Ring Systems),
#'      \item \code{'xlogp2'} (octanol-water partition coefficient),
#'      \item \code{'aromatic'} (is the compound aromatic),
#'      \item \code{'macrocyclic'} (is the compound macrocyclic),
#'      \item \code{'heteroatom_count'} (heteroatom count),
#'      \item \code{'hydrogen_atom_count'} (H atom count),
#'      \item \code{'heavy_atom_count'} ( Heavy atom count),
#'      \item \code{'deprotonable_group_count'} (Number of deprotonable groups),
#'      \item \code{'protonable_group_count'} (Number of protonable groups).
#'  }
#'
#'  CIR first tries to determine the indetifier type submitted and then
#'  uses 'resolvers' to look up the data.
#'  If no \code{resolver} is supplied, CIR tries different resolvers in
#'  turn till a hit is found.
#'  E.g. for names CIR tries first to look up in OPSIN and if this fails
#'  the local name index of CIR.
#'  However, it can be also specified which resolvers to use
#'  (if you know e.g. know your indentifier type)
#'  Possible \code{resolvers} are:
#'  \itemize{
#'    \item \code{'name_by_cir'} (Lookup in name index of CIR),
#'    \item \code{'name_by_opsin'} (Lookup in OPSIN),
#'    \item \code{'name_by_chemspider'} (Lookup in ChemSpider,
#'    \url{http://cactus.nci.nih.gov/blog/?p=1386}),
#'    \item \code{'smiles'} (Lookup SMILES),
#'    \item \code{'stdinchikey'}, \code{'stdinchi'} (InChI),
#'    \item \code{'cas_number'} (CAS Number),
#'    \item \code{'name_pattern'} (Google-like pattern search
#'    (\url{http://cactus.nci.nih.gov/blog/?p=1456})
#'    Note, that the pattern search can be combined with other resolvers,
#'    e.g. \code{resolver = 'name_by_chemspider,name_pattern'}.
#'
#'  }
#'
#' @references
#' \code{cir_query} relies on the great CIR web service created by the CADD
#' Group at NCI/NIH! \cr
#' \url{http://cactus.nci.nih.gov/chemical/structure_documentation}, \cr
#' \url{http://cactus.nci.nih.gov/blog/?cat=10}, \cr
#' \url{http://cactus.nci.nih.gov/blog/?p=1386}, \cr
#' \url{http://cactus.nci.nih.gov/blog/?p=1456}, \cr
#'
#' @author Eduard Szoecs, \email{eduardszoecs@@gmail.com}
#'
#' @examples
#' \dontrun{
#' cir_query('Triclosan', 'cas')
#' cir_query("3380-34-5", 'cas')
#' cir_query("3380-34-5", 'cas', resolver = 'cas_number')
#' cir_query("3380-34-5", 'smiles')
#' cir_query('Triclosan', 'mw')
#' cir_query('Triclosan', 'aromatic')
#' cir_query('Triclosan', 'xlogp2')
#'
#' reps <- c('smiles', 'cas', 'stdinchikey', 'mw', 'formula', 'xlogp2')
#' sapply(reps, function(x) cir_query('Triclosan', x, first = TRUE))
#'}
#' @export
cir_query <- function(identifier, representation = 'smiles', resolver = NULL,
                      first = FALSE, verbose = TRUE, ...){
  if(length(identifier) > 1){
    stop('Cannot handle multiple input strings.')
  }
  baseurl <- "http://cactus.nci.nih.gov/chemical/structure"
  qurl <- paste(baseurl, identifier, representation, 'xml', sep = '/')
  if(!is.null(resolver)){
    qurl <- paste0(qurl, '?resolver=', resolver)
  }
  if(verbose)
    message(qurl)
  # Sys.sleep(0.1)
  h <- try(xmlParse(qurl, isURL = TRUE))
  if(!inherits(h, "try-error")){
    out <- xpathSApply(h, "//data/item", xmlValue)
  } else{
    warning('Problem with web service encountered... Returning NA.')
    out < NA
  }
  if(length(out) == 0){
    warning('No representation found... Returning NA.')
    out <- NA
  }
  if(first)
    out <- out[1]
  return(out)
}