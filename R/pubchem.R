#' Retrieve Pubchem Id (CID)
#'
#' Return CompoundID (CID) for a search query using PUG-REST,
#' see \url{https://pubchem.ncbi.nlm.nih.gov/}.
#' @import httr
#' @importFrom stats rgamma
#' @param query character; search term.
#' @param from character; type of input, can be one of "name" (default), "cid",
#' "sid", "aid", "smiles", "inchi", "inchikey"
#' @param first logical; If TRUE return only first result.
#' @param search_substances logical; If TRUE also searches PubChem SIDs
#' @param verbose logical; should a verbose output be printed on the console?
#' @param arg character; optinal arguments like "name_type=word" to match
#' individual words.
#' @param ... optional arguments
#' @return a list of cids. If first = TRUE a vector.
#'
#' @references Wang, Y., J. Xiao, T. O. Suzek, et al. 2009 PubChem: A Public
#' Information System for
#' Analyzing Bioactivities of Small Molecules. Nucleic Acids Research 37:
#' 623–633.
#'
#' Kim, Sunghwan, Paul A. Thiessen, Evan E. Bolton, et al. 2016
#' PubChem Substance and Compound Databases. Nucleic Acids Research 44(D1):
#' D1202–D1213.
#'
#' Kim, S., Thiessen, P. A., Bolton, E. E., & Bryant, S. H. (2015).
#' PUG-SOAP and PUG-REST: web services for programmatic access to chemical
#' information in PubChem. Nucleic acids research, gkv396.
#' @note Please respect the Terms and Conditions of the National Library of
#' Medicine, \url{https://www.nlm.nih.gov/databases/download.html} and the data
#' usage policies of National Center for Biotechnology Information,
#' \url{https://www.ncbi.nlm.nih.gov/home/about/policies/},
#' \url{https://pubchemdocs.ncbi.nlm.nih.gov/programmatic-access}.
#' @author Eduard Szoecs, \email{eduardszoecs@@gmail.com}
#' @importFrom purrr map2
#' @export
#' @examples
#' \donttest{
#' # might fail if API is not available
#' get_cid("Triclosan")
#' get_cid("Triclosan", arg = "name_type=word")
#' get_cid("BPGDAMSIGCZZLK-UHFFFAOYSA-N", from = "inchikey")
#' get_cid("CCCC", from = "smiles")
#'
#' # multiple inputs
#' comp <- c("Triclosan", "Aspirin")
#' get_cid(comp)
#'
#' }
get_cid <- function(query, from = "name", first = FALSE,
                    search_substances = FALSE, verbose = TRUE,
                    arg = NULL, ...) {
  # from can be cid | name | smiles | inchi | sdf | inchikey | formula
  # query <- c("Aspirin")
  # from = "name"

  foo <- function(query, from, first, scope = "compound", verbose, ...) {
    if (is.na(query)) return(NA)
    prolog <- "https://pubchem.ncbi.nlm.nih.gov/rest/pug"
    input <- paste0("/", scope, "/", from)
    output <- "/cids/JSON"
    if (!is.null(arg))
      arg <- paste0("?", arg)
    qurl <- paste0(prolog, input, output, arg)
    if (verbose)
      message(qurl)
    Sys.sleep(rgamma(1, shape = 15, scale = 1 / 10))
    cont <- try(content(POST(qurl,
                             body = paste0(from, "=", query)
                             ), type = "text", encoding = "UTF-8"),
                silent = TRUE
    )
    if (inherits(cont, "try-error")) {
      warning("Problem with web service encountered... Returning NA.")
      return(NA)
    }
    cont <- fromJSON(cont)
    if (names(cont) == "Fault") {
      warning(cont$Fault$Details, ". Returning NA.")
      return(NA)
    }

    if (scope == "substance") {
      cont <- cont$InformationList$Information$CID
    }

    out <- unique(unlist(cont))


    if (first)
      out <- out[1]
    names(out) <- NULL
    return(out)
  }

  out <- lapply(query, foo, from = from, first = first, verbose = verbose)
  out <- setNames(out, query)

  if (search_substances) {
  out2 <- lapply(query, foo, from = from, first = first, scope = "substance",
                 verbose = verbose)
  out2 <- setNames(out2, query)

  out <- map2(out, out2, c)
  out <- lapply(out, unique)
  }


  if (first) {
    out <- unlist(out)
  }

  return(out)
}



#' Retrieve compound properties from a pubchem CID
#'
#' Retrieve compound information from pubchem CID, see
#' \url{https://pubchem.ncbi.nlm.nih.gov/}
#' @import httr jsonlite
#'
#' @param cid character; Pubchem ID (CID).
#' @param properties character vector; properties to retrieve, e.g.
#' c("MolecularFormula", "MolecularWeight"). If NULL (default) all available
#' properties are retrieved. See
#' \url{https://pubchem.ncbi.nlm.nih.gov/pug_rest/PUG_REST.html#_Toc409516770}
#' for a list of all available properties.
#' @param verbose logical; should a verbose output be printed to the console?
#' @param ... currently not used.
#'
#' @return a data.frame
#' @author Eduard Szoecs, \email{eduardszoecs@@gmail.com}
#' @seealso \code{\link{get_cid}} to retrieve Pubchem IDs.
#' @references Wang, Y., J. Xiao, T. O. Suzek, et al. 2009 PubChem: A Public
#' Information System for
#' Analyzing Bioactivities of Small Molecules. Nucleic Acids Research 37:
#' 623–633.
#'
#' Kim, Sunghwan, Paul A. Thiessen, Evan E. Bolton, et al. 2016
#' PubChem Substance and Compound Databases. Nucleic Acids Research 44(D1):
#' D1202–D1213.
#'
#' Kim, S., Thiessen, P. A., Bolton, E. E., & Bryant, S. H. (2015).
#' PUG-SOAP and PUG-REST: web services for programmatic access to chemical
#' information in PubChem. Nucleic acids research, gkv396.
#' @note Please respect the Terms and Conditions of the National Library of
#' Medicine, \url{https://www.nlm.nih.gov/databases/download.html} and the data
#' usage policies of National Center for Biotechnology Information,
#' \url{https://www.ncbi.nlm.nih.gov/home/about/policies/},
#' \url{https://pubchemdocs.ncbi.nlm.nih.gov/programmatic-access}.
#' @export
#' @examples
#' \donttest{
#' # might fail if API is not available
#' pc_prop(5564)
#'
#' ###
#' # multiple CIDS
#' comp <- c("Triclosan", "Aspirin")
#' cids <- unlist(get_cid(comp))
#' pc_prop(cids, properties = c("MolecularFormula", "MolecularWeight",
#' "CanonicalSMILES"))
#' }
pc_prop <- function(cid, properties = NULL, verbose = TRUE, ...) {
  # cid <- c("5564", "7843")
  napos <- which(is.na(cid))
  cid_o <- cid
  cid <- cid[!is.na(cid)]
  prolog <- "https://pubchem.ncbi.nlm.nih.gov/rest/pug"
  input <- "/compound/cid"
  if (is.null(properties))
    properties <- c("MolecularFormula", "MolecularWeight", "CanonicalSMILES",
                  "IsomericSMILES", "InChI", "InChIKey", "IUPACName",
                  "XLogP", "ExactMass", "MonoisotopicMass", "TPSA",
                  "Complexity", "Charge", "HBondDonorCount",
                  "HBondAcceptorCount", "RotatableBondCount", "HeavyAtomCount",
                  "IsotopeAtomCount", "AtomStereoCount",
                  "DefinedAtomStereoCount", "UndefinedAtomStereoCount",
                  "BondStereoCount", "DefinedBondStereoCount",
                  "UndefinedBondStereoCount", "CovalentUnitCount", "Volume3D",
                  "XStericQuadrupole3D", "YStericQuadrupole3D",
                  "ZStericQuadrupole3D", "FeatureCount3D",
                  "FeatureAcceptorCount3D", "FeatureDonorCount3D",
                  "FeatureAnionCount3D", "FeatureCationCount3D",
                  "FeatureRingCount3D", "FeatureHydrophobeCount3D",
                  "ConformerModelRMSD3D", "EffectiveRotorCount3D",
                  "ConformerCount3D", "Fingerprint2D")
  properties <- paste(properties, collapse = ",")
  output <- paste0("/property/", properties, "/JSON")

  qurl <- paste0(prolog, input, output)
  if (verbose)
    message(qurl)
  Sys.sleep(0.2)
  cont <- try(content(POST(qurl,
                           body = list("cid" = paste(cid, collapse = ",")
                                       )),
                      type = "text", encoding = "UTF-8"),
              silent = TRUE
  )
  if (inherits(cont, "try-error")) {
    warning("Problem with web service encountered... Returning NA.")
    return(NA)
  }
  cont <- fromJSON(cont)
  if (names(cont) == "Fault") {
    warning(cont$Fault$Message, ". ", cont$Fault$Details, ". Returning NA.")
    return(NA)
  }
  out <- cont$PropertyTable[[1]]
  # insert NA rows
  narow <- rep(NA, ncol(out))
  for (i in seq_along(napos)) {
    #capture NAs at beginning
    firstnna <- min(which(!is.na(cid_o)))
    if (napos[i] <  firstnna) {
      out <- rbind(narow, out)
    } else {
      # capture NAs at end
      if (napos[i] > nrow(out)) {
        # print(napos[i])
        out <- rbind(out, narow)
      } else {
        out <- rbind(out[1:(napos[i] - 1), ], narow, out[napos[i]:nrow(out), ])
      }
    }}
  rownames(out) <- NULL
  class(out) <- c("pc_prop", "data.frame")
  return(out)
}

#' Search synonyms in pubchem
#'
#' Search synonyms using PUG-REST,
#' see \url{https://pubchem.ncbi.nlm.nih.gov/}.
#' @import httr jsonlite
#' @importFrom utils menu
#'
#' @param query character; search term.
#' @param from character; type of input, can be one of "name" (default), "cid",
#'     "sid", "aid", "smiles", "inchi", "inchikey"
#' @param interactive deprecated.  Use the \code{choices} argument instead
#' @param choices to get only the first synonym, use \code{choices = 1}, to get
#' a number of synonyms to choose from in an interactive menu, provide the
#' number of choices you want or "all" to choose from all synonyms.
#' @param verbose logical; should a verbose output be printed on the console?
#' @param arg character; optinal arguments like "name_type=word" to match
#' individual words.
#' @param ... optional arguments
#' @return a character vector.
#'
#' @references Wang, Y., J. Xiao, T. O. Suzek, et al. 2009 PubChem: A Public
#' Information System for
#' Analyzing Bioactivities of Small Molecules. Nucleic Acids Research 37:
#' 623–633.
#'
#' Kim, Sunghwan, Paul A. Thiessen, Evan E. Bolton, et al. 2016
#' PubChem Substance and Compound Databases. Nucleic Acids Research 44(D1):
#' D1202–D1213.
#'
#' Kim, S., Thiessen, P. A., Bolton, E. E., & Bryant, S. H. (2015).
#' PUG-SOAP and PUG-REST: web services for programmatic access to chemical
#' information in PubChem. Nucleic acids research, gkv396.
#' @note Please respect the Terms and Conditions of the National Library of
#' Medicine, \url{https://www.nlm.nih.gov/databases/download.html} and the data
#' usage policies of National Center for Biotechnology Information,
#' \url{https://www.ncbi.nlm.nih.gov/home/about/policies/},
#' \url{https://pubchemdocs.ncbi.nlm.nih.gov/programmatic-access}.
#' @author Eduard Szoecs, \email{eduardszoecs@@gmail.com}
#' @export
#' @examples
#' \donttest{
#' pc_synonyms("Aspirin")
#' pc_synonyms(c("Aspirin", "Triclosan"))
#' pc_synonyms(5564, from = "cid")
#' pc_synonyms(c("Aspirin", "Triclosan"), choices = 10)
#' }
pc_synonyms <- function(query, from = "name", choices = NULL, verbose = TRUE,
                        arg = NULL, interactive = 0, ...) {
  # from can be cid | name | smiles | inchi | sdf | inchikey | formula
  # query <- c("Aspirin")
  # from = "name"
  if (!missing("interactive"))
    stop("'interactive' is deprecated. Use 'choices' instead.")
  foo <- function(query, from, verbose, ...) {
    prolog <- "https://pubchem.ncbi.nlm.nih.gov/rest/pug"
    input <- paste0("/compound/", from)
    output <- "/synonyms/JSON"
    if (!is.null(arg))
      arg <- paste0("?", arg)
    qurl <- paste0(prolog, input, output, arg)
    if (verbose)
      message(qurl)
    Sys.sleep(0.2)
    cont <- try(content(POST(qurl,
                             body = paste0(from, "=", query)
    )), silent = TRUE
    )
    if (inherits(cont, "try-error")) {
      warning("Problem with web service encountered... Returning NA.")
      return(NA)
    }
    if (names(cont) == "Fault") {
      warning(cont$Fault$Details, ". Returning NA.")
      return(NA)
    }
    out <- unlist(cont)
    names(out) <- NULL

    out <- chooser(out, choices)

  }
  out <- lapply(query, foo, from = from, verbose = verbose)
  out <- setNames(out, query)
  if (!is.null(choices)) #if only one choice is returned, convert list to vector
    out <- unlist(out)
  return(out)
}

#' Search PubChem through highly customizable requests
#'
#' @description This function gives you more control over your communications
#' with the PubChem PUG-REST webservice. This documentation page was created
#' from the original PubChem PUG-REST specification document which can be found
#' at \url{https://pubchemdocs.ncbi.nlm.nih.gov/pug-rest}.
#' @importFrom httr POST http_status
#' @param query character; search term. A comma-separated list of positive
#' integers (e.g. cid, sid, aid) or identifier strings (source, inchikey,
#' formula). In some cases only a single identifier string (name, smiles, xref;
#' inchi, sdf by POST only).
#' @param domain character; the domain of the search term. Can be one of
#' \code{"compound"}, \code{"substance"}, \code{"assay"},
#' \code{"sources/[substance, assay]"}, \code{"sourcetable"},
#' \code{"conformers"},
#' \code{"annotations/[sourcename/<source name>|heading/<heading>]"}. See
#' details for more information.
#' @param from character; the type of the search term, refered to as
#' \code{namespace} within the reference. Valid values depend on the domain.
#' See details for more information.
#' @param to character; refered to as \code{operations} within the reference.
#' Tells the service which part of the record to retrieve. Currently, if no
#' operation is specified at all, the default is to retrieve the entire record.
#' See details for more information.
#' @param output character; the format of the output. Can be one of
#' \code{"XML"}, \code{"ASNT"}, \code{"ASNB"}, \code{"JSON"},
#' \code{"JSONP[?callback=<callback name>]"}, \code{"SDF"}, \code{"CSV"},
#' \code{"PNG"}, \code{"TXT"}. Note that not all formats are applicable to the
#' results of all operations; one cannot, for example, retrieve a whole compound
#' record as CSV or a property table as SDF. TXT output is only available in a
#' restricted set of cases where all the information is the same – for example,
#' synonyms for a single CID where there is one synonym per line.
#' @return a html POST response.
#' @details Valid \code{from} values for \code{domain = "compound"} are:
#' \itemize{
#' \item \code{"cid"}, \code{"name"}, \code{"smiles"}, \code{"inchi"},
#' \code{"sdf"}, \code{"inchikey"}, \code{"formula"}, \code{<structure search>},
#' \code{<xref>}, \code{"listkey"}, \code{<fast search>}.
#' \item \code{<structure search>} is assembled as "\{\code{substructure},
#' \code{superstructure}, \code{similarity}, \code{identity}\}/\{\code{smiles},
#' \code{inchi}, \code{sdf}, \code{cid}\}", e.g.
#' \code{from = "substructure/smiles"}.
#' \item \code{<xref>} is assembled as "\code{xref}/\{\code{RegistryID},
#' \code{RN}, \code{PubMedID}, \code{MMDBID}, \code{ProteinGI},
#' \code{NucleotideGI}, \code{TaxonomyID}, \code{MIMID}, \code{GeneID},
#' \code{ProbeID}, \code{PatentID}\}, e.g. \code{from = "xref/RN"}. <xref> is
#' not case sensitive.
#' \item \code{<fast search>} is either \code{"fastformula"} or is assembled as
#' "\{\code{fastidentity}, \code{fastsimilarity_2d}, \code{fastsimilarity_3d},
#' \code{fastsubstructure}, \code{fastsuperstructure}\}/\{\code{smiles},
#' \code{smarts}, \code{inchi}, \code{sdf} or \code{cid}\}", e.g.
#' \code{from = "fastidentity/smiles"}.
#' }
#' @details Valid \code{from} values for \code{domain = "substance"} are:
#' \itemize{
#' \item \code{"sid"}, \code{"sourceid/<source id>"},
#' \code{"sourceall/<source name>"}, \code{name}, \code{<xref>} or
#' \code{listkey}.
#' \item \code{<source name>} is any valid PubChem depositor name. Valid names
#' can be found at \url{https://pubchem.ncbi.nlm.nih.gov/sources/}. Special
#' characters may need to be escaped, such as "&" should be replaced by "\%26".
#' }
#' @details Valid \code{from} values for \code{domain = "assay"} are:
#' \itemize{
#' \item \code{"aid"}, \code{"listkey"}, \code{"type/<assay type>"},
#' \code{"sourceall/<source name>"}, \code{"target/<assay target>"} or
#' \code{"activity/<activity column name>"}.
#' \item \code{<assay type>} can be \code{all}, \code{confirmatory},
#' \code{doseresponse}, \code{onhold}, \code{panel}, \code{rnai},
#' \code{screening}, \code{summary}, \code{cellbased}, \code{biochemical},
#' \code{invivo}, \code{invitro} or \code{activeconcentrationspecified}.
#' \item \code{<assay target>} can be \code{gi}, \code{proteinname},
#' \code{geneid}, \code{genesymbol} or \code{accession}.
#' }
#' @details \code{<other inputs>} for domain can be \code{"sources/[substance,
#' assay]"}, \code{"sourcetable"}, \code{"conformers"}, \code{"annotations/
#' [sourcename/<source name>, heading/<heading>]"}.
#' @details Valid \code{to} values for \code{domain = "compound"} are:
#' \itemize{
#' \item \code{"record"}, \code{<compound property>}, \code{"synonyms"},
#' \code{"sids"}, \code{"cids"}, \code{"aids"}, \code{"assaysummary"},
#' \code{"classification"}, \code{<xrefs>}, \code{"description"} or
#' \code{"conformers"}.
#' \item \code{<compound property>} is assembled as \code{"property/
#' [comma-separated list of <property tags>]"}.
#' \item Valid \code{<property tags>} are \code{"MolecularFormula"},
#' \code{"MolecularWeight"}, \code{"CanonicalSMILES"}, \code{"IsomericSMILES"},
#' \code{"InChI"}, \code{"InChIKey"}, \code{"IUPACname"}, \code{"XlogP"},
#' \code{"ExactMass"}, \code{"MonoisotopicMass"}, \code{"TPSA"},
#' \code{"Complexity"}, \code{"Charge"}, \code{"HBondDonorCount"},
#' \code{"HBondAcceptorCount"}, \code{"RotatableBondCount"},
#' \code{"HeavyAtomCount"}, \code{"IsotopeAtomCount"}, \code{"AtomStereoCount"},
#' \code{"DefinedAtomStereoCount"}, \code{"UndefinedAtomStereoCount"},
#' \code{"BondStereoCount"}, \code{"DefinedBondStereoCount"},
#' \code{"UndefinedBondStereoCount"}, \code{"CovalentUnitCount"},
#' \code{"Volume3D"}, \code{"XStericQuadrupole3D"},
#' \code{"YStericQuadrupole3D"}, \code{"ZStericQuadrupole3D"},
#' \code{"FeatureCount3D"}, \code{"FeatureAcceptorCount3D"},
#' \code{"FeatureDonorCount3D"}, \code{"FeatureAnionCount3D"},
#' \code{"FeatureCationCount3D"}, \code{"FeatureRingCount3D"},
#' \code{"FeatureHydrophobeCount3D"}, \code{"ConformerModelRMSD3D"},
#' \code{"EffectiveRotorCount3D"}, \code{"ConformerCount3D"},
#' \code{"Fingerprint2D"}. <property tags> are not case sensitive.
#' }
#' @details Valid \code{to} values for \code{domain = "substance"} are:
#' \itemize{
#' \item \code{"record"}, \code{"synonyms"}, \code{"sids"}, \code{"cids"},
#' \code{"aids"}, \code{"assaysummary"}, \code{"classification"},
#' \code{<xrefs>}, \code{"description"}.
#' \item \code{<xrefs>} is assembled as \code{"xrefs/
#' [comma-separated list of xrefs tags]"}.
#' }
#' @details Valid \code{to} values for \code{domain = "assay"} are:
#' \itemize{
#' \item \code{"record"}, \code{"concise"}, \code{"aids"}, \code{"sids"},
#' \code{"cids"}, \code{"description"}, \code{"targets/<target type>"},
#' \code{<doseresponse>}, \code{"summary"} or \code{"classification"}.
#' \item \code{<target type>}  can be \code{"ProteinGI"}, \code{"ProteinName"},
#' \code{"GeneID"} or \code{"GeneSymbol"}.
#' \item \code{<doseresponse>} is assembled as \code{"doseresponse/sid"}.
#' }
#' @references For more information, visit
#' \url{https://pubchemdocs.ncbi.nlm.nih.gov/pug-rest}
#' @author Tamás Stirling, \email{stirling.tamas@@gmail.com}
#' @export
pc_pugrest <- function(query, domain, from, to = "all", output) {
  request <- pc_validate(query, domain, from, to, output)
  Sys.sleep(rgamma(1, shape = 15, scale = 1/10))
  response <- try(POST(request$qurl, body = request$body), silent = TRUE)
  if (response$status_code != 200) {
    stop(httr::http_status(response)$message)
  }
  return(response)
}

#' Validate a PubChem PUG-REST API query and construct the query URL and body
#'
#' @description This function validates a PubChem PUG-REST API query according
#' to teh PubChem PUG-REST specification document, which can be found at
#' \url{https://pubchemdocs.ncbi.nlm.nih.gov/pug-rest}. If a query argument is
#' invalid the function prints an informative error message. If all query
#' arguments are valid, the function returns the query url and body that can
#' then be used in http requests. See \code{\link{pc_pugrest}} for lists of
#' valid values for each argument.
#' @param query character;
#' @param domain character; \code{"compound"}, \code{"substance"}, etc.
#' @param from character; mentioned as \code{"namespace"} in the PubChem
#' PUG-REST specification document.
#' @param to character; mentioned as \code{"operations"} in the PubChem
#' PUG-REST specification document.
#' @param output character;
#' @return a list of two elements, \code{url} and \code{body}.
#' @note The function cannot tell if a source id or a depositor
#' name is valid. If the entry starts with \code{"sourceid/"} or
#' \code{"sourceall/"} it will acept the entry.
#' @note When validating an entry for \code{domain = "assay"},
#' \code{type = "from"}, the function cannot tell if a depositor name or an
#' activity is valid. If the entry starts with \code{"sourceall/"} or
#' \code{"activity/"} it will acept the entry.
#' @note Some source names contain the "/" character, which which is
#' incompatible with the URL syntax, e.g. EU REGULATION (EC) No 1272/2008.
#' The functions attempts to convert "/" to "." as recommended by the
#' specification document.
#' @note It is possible to query multiple properties or multiple xrefs in the
#' same query. When validating an entry for \code{domain = "compound"},
#' \code{type = "to"} if the entry starts with \code{property/} or \code{xrefs/}
#' the function will also check the comma separated values after the forward
#' slash and evaluate whether the entry can be whitelisted.
#' @references \url{https://pubchemdocs.ncbi.nlm.nih.gov/pug-rest}
#' @author Tamás Stirling, \email{stirling.tamas@@gmail.com}
#' @seealso \code{\link{pc_pugrest}}
#' @noRd
pc_validate <- function(query, domain, from, to = "all", output) {
  domain <- tolower(gsub(" *", "", domain))
  from <- tolower(gsub(" *", "", from))
  to <- tolower(gsub(" *", "", to))
  output <- tolower(gsub(" *", "", output))
  xref <- paste(
    "xref",
    c("registryid", "rn", "pubmedid", "mmdbid", "proteingi", "nucleotidegi",
      "taxonomyid", "mimid", "geneid", "probeid", "patentid"),
    sep = "/"
  )
  xrefs <- c("registryid", "rn", "pubmedid", "mmdbid", "proteingi",
             "nucleotidegi", "taxonomyid", "mimid", "geneid", "probeid",
             "patentid", "dburl", "sburl", "sourcename", "sourcecategory")
  #from_choices for domain = "compound"
  if(domain == "compound") {
    structure_search <- expand.grid(
      c("substructure", "superstructure", "similarity", "identity"),
      c("smiles", "inchi", "sdf", "cid")
    )
    structure_search <- with(structure_search, paste(Var1, Var2, sep = "/"))
    fast_search <- expand.grid(
      c("fastidentity", "fastsimilarity_2d", "fastsimilarity_3d",
        "fastsubstructure", "fastsuperstructure"),
      c("smiles", "smarts", "inchi", "sdf", "cid")
    )
    fast_search <- c(with(fast_search, paste(Var1, Var2, sep = "/")),
                     "fastformula")
    from_choices <-c("cid", "name", "smiles", "inchi", "sdf", "inchikey",
                     "formula", structure_search, xref, "listkey", fast_search)
  }
  #from_choices for domain = "substance"
  if (domain == "substance") {
    #from = "sourceid/IBM", from = "sourceid/EU REGULATION (EC) No 1272/2008"
    pre <- strsplit(from, "/")[[1]][1]
    post <- paste(strsplit(from, "/")[[1]][-1], collapse =".")
    if (pre == "sourceid") {
      from <- paste(pre, post, sep = "/")
      from_choices <- from
    }
    else {
      from_choices <- c("sid", "name", "sourceall", xref, "listkey")
    }
  }
  # from_choices for domain = "assay"
  if (domain == "assay") {
    from_choices <- c("aid", "listkey", "type", "sourceall", "target",
                      "activity")
  }
  # query_choices
  if ("sourceall" %in% from_choices) {
    # how to validate against all valid source names?
    query <- gsub("/", ".", query) #ensure URL syntax
    query_choice <- query
  }
  if ("type" %in% from_choices){
    query_choices <- c("all", "confirmatory", "doseresponse", "onhold",
                       "panel", "rnai", "screening", "summary", "cellbased",
                       "biochemical", "invivo", "invitro",
                       "activeconcentrationspecified")
  }
  if ("target" %in% from_choices){
    query_choices <- c("gi", "proteinname", "geneid", "genesymbol", "accession")
  }
  if (mean(c("sourceall", "type", "target") %in%
           from_choices) == 0) {
    query_choices <- query
  }
  #to_choices for domain = "compound"
  if (domain == "compound") {
    properties <- c("molecularformula", "molecularweight", "canonicalsmiles",
                    "isomericsmiles", "inchi", "inchikey", "iupacname",
                    "xlogp", "exactmass", "monoisotopicmass", "tpsa",
                    "complexity", "charge", "hbonddonorcount",
                    "hbondacceptorcount", "rotatablebondcount", "heavyatomcount",
                    "isotopeatomcount", "atomstereocount",
                    "definedatomstereocount", "undefinedatomstereocount",
                    "bondstereocount", "definedbondstereocount",
                    "undefinedbondstereocount", "covalentunitcount", "volume3d",
                    "xstericquadrupole3d", "ystericquadrupole3d",
                    "zstericquadrupole3d", "featurecount3d",
                    "featureacceptorcount3d", "featuredonorcount3d",
                    "featureanioncount3d", "featurecationcount3d",
                    "featureringcount3d", "featurehydrophobecount3d",
                    "conformermodelrmsd3d", "effectiverotorcount3d",
                    "conformercount3d", "fingerprint2d")
    pre <- strsplit(to, "/")[[1]][1]
    post <- paste(strsplit(to, "/")[[1]][-1], collapse =",")
    if(pre == "property") {
      props <- strsplit(post, ",")[[1]]
      if (mean(props %in% properties) == 1) to_choices <- to
    }
    if (pre == "xrefs") {
      refs <- strsplit(post, ",")[[1]]
      if (mean(refs %in% xrefs) == 1) to_choices <- to
    }
    if (sum(pre %in% c("property", "xrefs")) == 0) {
      to_choices <- c("record", "synonyms", "sids", "cids", "aids",
                      "assaysummary", "classification", "description",
                      "conformers")
    }
  }
  #to_choices for domain = "substance"
  if (domain == "substance") {
    pre <- strsplit(to, "/")[[1]][1]
    post <- paste(strsplit(to, "/")[[1]][-1], collapse =",")
    if (pre == "xrefs") {
      refs <- strsplit(post, ",")[[1]]
      if (mean(refs %in% xrefs) == 1) to_choices <- to
    }
    else {
      to_choices <- c("record", "synonyms", "sids", "cids", "aids",
                      "assaysummary", "classification", "description")
    }
  }
  if (domain == "assay") {
    pre <- strsplit(to, "/")[[1]][1]
    post <- paste(strsplit(to, "/")[[1]][-1], collapse =",")
    if (pre == "targets") {
      tgts <- c("proteingi", "proteinname", "geneid", "genesymbol")
      targets <- strsplit(post, ",")[[1]]
      if (mean(targets %in% tgts) == 1) to_choices <- to
    }
    else {
      to_choices <- c("record", "concise", "aids", "sids", "cids",
                      "description", "doseresponse/sid", "summary",
                      "classification")
    }
  }
  if (strsplit(output, "?")[[1]][1] == "jsonp") output_choices <- output
  else  output_choices <- c("xml", "asnt", "asnb", "json", "sdf", "csv",
                            "png", "txt")
  for (i in query) {
    if (i %in% query_choices == FALSE) {
      stop(paste0("query = ", i, " is invalid."))
    }
  }
  if (from %in% from_choices == FALSE) {
    stop(paste0("from = ", from, " is invalid."))
  }
  if (to %in% to_choices == FALSE & to != "all") {
    stop(paste0("to = ", to, " is invalid."))
  }
  if (output %in% output_choices == FALSE) {
    stop(paste0("output = ", output, " is invalid."))
  }
  if (to == "all") {
    qurl <- paste("https://pubchem.ncbi.nlm.nih.gov/rest/pug", domain, from,
                  output, sep = "/")
  }
  else {
    qurl <- paste("https://pubchem.ncbi.nlm.nih.gov/rest/pug", domain, from, to,
                  output, sep = "/")
  }
  qurl <- URLencode(qurl)
  body <- paste0(strsplit(from, "/")[[1]][1], "=", paste(query, collapse = ","))
  return(list(qurl = qurl, body = body))
}
