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

#' Query PubChem for custom information
#' @importFrom jsonlite fromJSON
#'
#' @details The full list of queries can be found at
#' \url{https://pubchem.ncbi.nlm.nih.gov/classification/#hid=72}
#' @examples
#' \donttest{
#' pc_query(176, "pka")
#'
#' }
#'

pc_query <- function(cid, query) {
  query <- gsub(" +", "+", query)
  foo <- function(cid, query){
    if (is.na(cid)) {
      message("Invalid input. Returning NA.")
      return(NA)
    }
    qurl <- paste0(
      "https://pubchem.ncbi.nlm.nih.gov/rest/pug_view/data/compound/",
      cid,"/JSON?heading=",query)
    cont <- try(jsonlite::fromJSON(qurl), silent = TRUE)
    if (inherits(cont, "try-error")) {
      message(paste0("CID ",cid, ": Query not found. Returning NA."))
      return(NA)
    }
    return(cont)
  }
  cont <- lapply(cid, function(x) foo(x, query))

  #return(cont)

  info <- rbind(lapply(cont, function(x) {
    x$Record$Section$Section[[1]]$Section[[1]]$Information[[1]]
  }))



  #info <- cont$Record$Section$Section[[1]]$Section[[1]]$Information[[1]]
  #if("Reference" %in% names(info)) {
  #  info <- info[,-which(names(info) == "Reference")]
  #}
  xinfo <- cbind(cid, info)
  #ref <- cont$Record$Reference
  #ref <- lapply(split(ref, ref$ReferenceNumber),as.list)

  #out <- list(
  #  "Information" = info,
  #  "References" = ref
  #)
  #return(out)




  #names(out) <- cid


  return(info)

}