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
#' Medicine, \url{https://www.nlm.nih.gov/databases/download.html} the data
#' usage policies of National Center for Biotechnology Information,
#' \url{https://www.ncbi.nlm.nih.gov/home/about/policies/},
#' \url{https://pubchemdocs.ncbi.nlm.nih.gov/programmatic-access}, and the data
#' usage policies of the indicidual data sources
#' \url{https://pubchem.ncbi.nlm.nih.gov/sources/}.
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
#' Medicine, \url{https://www.nlm.nih.gov/databases/download.html} the data
#' usage policies of National Center for Biotechnology Information,
#' \url{https://www.ncbi.nlm.nih.gov/home/about/policies/},
#' \url{https://pubchemdocs.ncbi.nlm.nih.gov/programmatic-access}, and the data
#' usage policies of the indicidual data sources
#' \url{https://pubchem.ncbi.nlm.nih.gov/sources/}.
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
#' Medicine, \url{https://www.nlm.nih.gov/databases/download.html} the data
#' usage policies of National Center for Biotechnology Information,
#' \url{https://www.ncbi.nlm.nih.gov/home/about/policies/},
#' \url{https://pubchemdocs.ncbi.nlm.nih.gov/programmatic-access}, and the data
#' usage policies of the indicidual data sources
#' \url{https://pubchem.ncbi.nlm.nih.gov/sources/}.
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

#' Import PubChem content pages
#'
#' When you search for a compound at \url{https://pubchem.ncbi.nlm.nih.gov/},
#' and select the compound you are interested in, you will be forwarded to a
#' PubChem content page. This function is used to import a section of that page
#' into your workflow.
#' @importFrom jsonlite fromJSON
#' @param cid numeric; a vector of identifiers to search for.
#' @param section character; the section of the conent page to be imported.
#' @return a list of PubChem content pages.
#' @details \code{section} can be any section of a PubChem content page, e.g.
#' \code{section = "solubility"} will import the section on solubility, or
#' \code{section = "experimental properties"} will import all experimental
#' properties. The \code{section} argument is not case sensitive but it
#' is sensitive to typing errors and it requires the full name of the section as
#' it is printed on the content page. The PubChem Table of Contents Tree can
#' also be found at
#' \url{https://pubchem.ncbi.nlm.nih.gov/classification/#hid=72}.
#' @details The function will return a list of content pages where each
#' element is a list itself. While it is possible to navigate the returned
#' objects through list operations, it is more convenient to use
#' \code{pc_extract()} to access low level data.
#' @note Please respect the Terms and Conditions of the National Library of
#' Medicine, \url{https://www.nlm.nih.gov/databases/download.html} the data
#' usage policies of National Center for Biotechnology Information,
#' \url{https://www.ncbi.nlm.nih.gov/home/about/policies/},
#' \url{https://pubchemdocs.ncbi.nlm.nih.gov/programmatic-access}, and the data
#' usage policies of the indicidual data sources
#' \url{https://pubchem.ncbi.nlm.nih.gov/sources/}.
#' @author Tamas Stirling, \email{stirling.tamas@@gmail.com}
#' @examples
#' \donttest{
#' pka <- pc_page(176, "pKa")
#' ep <- pc_page(c(176, 311), "experimental properties")
#' tox <- pc_page(176, "toxicity")
#' }
#' @export
pc_page <- function(cid, section, verbose = TRUE) {
  section <- gsub(" +", "+", section)
  foo <- function(cid, section) {
    if (is.na(cid) | is.na(suppressWarnings(as.numeric(cid)))) {
      if (verbose == TRUE) {
        message("Querying ", cid, ": Invalid input. Returning NA.")
      }
      return(NA)
    }
    qurl <- paste0(
      "https://pubchem.ncbi.nlm.nih.gov/rest/pug_view/data/compound/",
      cid, "/JSON?heading=", section)
    if (verbose == TRUE) message("Querying ", cid, ": ", appendLF = FALSE)
    Sys.sleep(0.2)
    cont <- try(jsonlite::fromJSON(qurl, simplifyDataFrame = FALSE),
                silent = TRUE)
    if (inherits(cont, "try-error")) {
      if (verbose == TRUE) message("Section not found. Returning NA.")
      return(NA)
    }
    else{
      if (verbose == TRUE) message("Section found.")
    }
    return(cont)
  }
  cont <- lapply(cid, function(x) foo(x, section))
  return(cont)
}

#' Extract data from PubChem content pages

#' This function takes a list of PubChem content pages, and extracts the
#' required low level information from them.
#' @importFrom data.tree as.Node FindNode
#' @importFrom dplyr bind_rows as_tibble
#' @param query list; a list of PubChem content pages.
#' @param section character; a low level section of data to be accessed.
#' @return a tibble of chemical information with references.
#' @details When you look at a PubChem content page, you can see that chemical
#' information is organised into sections, subsections, etc. The chemical data
#' live at the low levels of these sections. Use this function to extract low
#' level information from PubChem content pages.
#' @details Contrary to \code{pc_page()}, the \code{section} argument in
#' \code{pc_extract} is case sensitive, sensitive to typing errors, and requires
#' the full name of the section as it is printed on the content page. The
#' PubChem Table of Contents Tree can also be found at
#' \url{https://pubchem.ncbi.nlm.nih.gov/classification/#hid=72}.
#' @note Please respect the Terms and Conditions of the National Library of
#' Medicine, \url{https://www.nlm.nih.gov/databases/download.html} the data
#' usage policies of National Center for Biotechnology Information,
#' \url{https://www.ncbi.nlm.nih.gov/home/about/policies/},
#' \url{https://pubchemdocs.ncbi.nlm.nih.gov/programmatic-access}, and the data
#' usage policies of the indicidual data sources
#' \url{https://pubchem.ncbi.nlm.nih.gov/sources/}.
#' @author Tamas Stirling, \email{stirling.tamas@@gmail.com}
#' @examples
#' \donttest{
#' # Import all experimental properties for acetic acid and citric acid
#' pages <- pc_page(c(176, 311), "experimental properties")
#' # Extract pKa data
#' pc_extract(pages, "pKa")
#'
#' # Import all toxicity data for benzene
#' pages <- pc_page(241, "toxicity")
#' # Extract exposure routes
#' pc_extract(pages, "Exposure Routes")$String
#'
#' # Import all hazards identification data for formaldehyde
#' pages <- pc_page(712, "hazards identification")
#' # Extract
#' pc_extract(pages, "EPA Hazardous Waste Number")$String
#' }
#' @export
pc_extract <- function(query, section) {
  foo <- function(x) {
    if (is.na(x)) return(data.frame(CID = NA))
    tree <- data.tree::as.Node(x, nameName = "TOCHeading")
    node <- data.tree::FindNode(tree, section)
    if (is.null(node)) return(data.frame(
      CID = x$Record$RecordNumber,
      Name = x$Record$RecordTitle,
      stringsAsFactors = FALSE))
    node <- FindNode(node, "Information")
    if (is.null(node)) return(data.frame(
      CID = x$Record$RecordNumber,
      Name = x$Record$RecordTitle,
      stringsAsFactors = FALSE))
    info <- lapply(node, function(y) {
      lownode <- data.tree::FindNode(data.tree::as.Node(y), "StringWithMarkup")
      if (is.null(lownode)) {
        info <- data.frame(String = paste(y$Value, collapse = " "),
                          ReferenceNumber = y$ReferenceNumber,
                          stringsAsFactors = FALSE)
        return(info)
      }
      else{
        info <- data.frame(String = sapply(lownode, function(z) z$String),
                           ReferenceNumber = y$ReferenceNumber,
                           stringsAsFactors = FALSE)
      }
    })
    info <- dplyr::bind_rows(info)
    info <- data.frame(CID = x$Record$RecordNumber,
                       Name = x$Record$RecordTitle,
                       info,
                       stringsAsFactors = FALSE)
    node <- FindNode(tree, "Reference")
    if (is.null(node)) return(data.frame(
      info,
      SourceName = NA,
      SourceID = NA))
    ref <- lapply(node, function(y) {
      ref <- data.frame(
        ReferenceNumber = y$ReferenceNumber,
        SourceName = y$SourceName,
        SourceID = y$SourceID,
        stringsAsFactors = FALSE
      )
      return(ref)
    })
    ref <- dplyr::bind_rows(ref)
    info$SourceName <- sapply(info$ReferenceNumber, function(x) {
      ref$SourceName[ref$ReferenceNumber == x]
    })
    info$SourceID <- sapply(info$ReferenceNumber, function(x) {
      ref$SourceID[ref$ReferenceNumber == x]
    })
    return(info)
  }
  info <- dplyr::as_tibble(dplyr::bind_rows(lapply(query, foo)))
  info <- info[, -which(names(info) == "ReferenceNumber")]
  return(info)
}
