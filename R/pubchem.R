#' Retrieve Pubchem Compound ID (CID)
#'
#' Retrieve compound IDs (CIDs) from PubChem.
#' @param query character; search term, one or more compounds.
#' @param from character; type of input, can be one of "name" (default), "cid",
#' "smiles", "inchi", "sdf", "inchikey", "formula", <structure search>, <xref>,
#' <fast search>. See details for more information.
#' @param match character; How should multiple hits be handled?, "all" all
#' matches are returned, "best" the best matching is returned, "ask" enters an
#' interactive mode and the user is asked for input, "na" returns NA if multiple
#' hits are found.
#' @param verbose logical; should a verbose output be printed on the console?
#' @param arg character; optinal arguments like "name_type=word" to match
#' individual words.
#' @param first deprecated. Use `match` instead.
#' @param ... currently unused.
#' @return a tibble.
#' @details <structure search> is assembled as "{\code{substructure} |
#' \code{superstructure} | \code{similarity} | \code{identity}} / {\code{smiles}
#'  | \code{inchi} | \code{sdf} | \code{cid}}", e.g.
#'  \code{from = "substructure/smiles"}.
#' @details \code{<xref>} is assembled as "\code{xref}/\{\code{RegistryID} |
#' \code{RN} | \code{PubMedID} | \code{MMDBID} | \code{ProteinGI},
#' \code{NucleotideGI} | \code{TaxonomyID} | \code{MIMID} | \code{GeneID} |
#' \code{ProbeID} | \code{PatentID}\}", e.g. \code{from = "xref/RN"} will query
#' by CAS RN.
#' @details <fast search> is either \code{fastformula} or it is assembled as
#' "{\code{fastidentity} | \code{fastsimilarity_2d} | \code{fastsimilarity_3d} |
#' \code{fastsubstructure} | \code{fastsuperstructure}}/{\code{smiles} |
#' \code{smarts} | \code{inchi} | \code{sdf} | \code{cid}}", e.g.
#' \code{from = "fastidentity/smiles"}.
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
#' @import httr
#' @importFrom purrr map map2
#' @importFrom jsonlite fromJSON
#' @importFrom stats rgamma
#' @importFrom tibble enframe
#' @export
#' @examples
#' \donttest{
#' # might fail if API is not available
#' get_cid("Triclosan")
#' get_cid("Triclosan", arg = "name_type=word")
#' get_cid("BPGDAMSIGCZZLK-UHFFFAOYSA-N", from = "inchikey")
#' get_cid("CCCC", from = "smiles")
#' get_cid("C26H52NO6P", from = "formula")
#' get_cid("56-40-6", from = "xref/rn")
#' get_cid(5564, from = "similarity/cid")
#' get_cid("CCO", from = "similarity/smiles")
#'
#' # multiple inputs
#' comp <- c("Triclosan", "Aspirin")
#' get_cid(comp)
#'
#' }
get_cid <-
  function(query,
           from = "name",
           match = c("all", "first", "ask", "na"),
           verbose = TRUE,
           arg = NULL,
           first = NULL,
           ...) {
  #deprecate `first`
  if (!is.null(first) && first == TRUE) {
    message("`first = TRUE` is deprecated. Use `match = 'first'` instead")
    match <- "first"
  } else if (!is.null(first) && first==FALSE) {
    message("`first = FALSE` is deprecated. Use `match = 'all'` instead")
    match <- "all"
  }
    #input validation
    xref <- paste(
      "xref",
      c("registryid", "rn", "pubmedid", "mmdbid", "proteingi", "nucleotidegi",
        "taxonomyid", "mimid", "geneid", "probeid", "patentid"),
      sep = "/"
    )
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
                     "formula", structure_search, xref, fast_search)
    from <- tolower(from)
    from <- match.arg(from, choices = from_choices)
    match <- match.arg(match)
    foo <- function(query, from, match, verbose, arg, ...) {
      if (is.na(query)) return(NA)
      if (from %in% structure_search) {
        qurl <- paste("https://pubchem.ncbi.nlm.nih.gov/rest/pug/compound",
                      from, query, "json", sep = "/")
      }
      else {
        qurl <- paste("https://pubchem.ncbi.nlm.nih.gov/rest/pug/compound",
                      from, query, "cids", "json", sep = "/")
      }
      if (!is.null(arg)) qurl <- paste0(qurl, "?", arg)
      if (verbose) {
        message(paste0("Querying ", query, ". "), appendLF = FALSE)
      }
      Sys.sleep(rgamma(1, shape = 15, scale = 1 / 10))
      res <- httr::POST(qurl, user_agent("webchem"), handle = handle(""))
      if (res$status_code != 200) {
        if (res$status_code == 202) {
          cont <- httr::content(res, type = "text", encoding = "UTF-8")
          listkey <- jsonlite::fromJSON(cont)$Waiting$ListKey
          qurl <- paste("https://pubchem.ncbi.nlm.nih.gov/rest/pug/compound",
                        "listkey", listkey, "cids", "json", sep = "/")
          while (res$status_code == 202) {
            Sys.sleep(5 + rgamma(1, shape = 15, scale = 1 / 10))
            res <- httr::POST(qurl, user_agent("webchem"), handle = handle(""))
          }
          if (res$status_code != 200) {
            if (verbose) message(httr::message_for_status(res))
            return(NA)
          }
        }
        else{
          if (verbose) message(httr::message_for_status(res))
          return(NA)
        }
      }
      if (verbose) message(httr::message_for_status(res))
      cont <- httr::content(res, type = "text", encoding = "UTF-8")
      cont <- jsonlite::fromJSON(cont)$IdentifierList$CID
      out <- unique(unlist(cont))
      out <- matcher(x = out, query = query, match = match, verbose = verbose)
      out <- as.character(out)
      names(out) <- NULL
      return(out)
      return(cont)
    }
    out <- map(query,
             ~foo(query = .x, from = from, match = match,
                  verbose = verbose, arg = arg))
    out <- setNames(out, query)
    out <-
    lapply(out, enframe, name = NULL, value = "cid") %>%
    bind_rows(.id = "query")
    return(out)
}

#' Retrieve PubChem Substance ID (SID)
#'
#' @description Retrieve substance IDs (SIDs) from PubChem based on substance
#' name, registry ID (e.g. CAS RN), or source ID. Alternatively, retrieve SIDs
#' of all substances provided by a PubChem depositor.
#' @param query character; search term, one ore more substances or depositors.
#' @param from character; type of input. Valid values are \code{"name"},
#' \code{<xref>}, \code{"sourceid/<source id>"} or \code{"sourceall"}. See
#' details for more information.
#' @param match character; How should multiple hits be handled?, "all" all
#' matches are returned, "best" the best matching is returned, "ask" enters an
#' interactive mode and the user is asked for input, "na" returns NA if multiple
#' hits are found.
#' @param verbose logical; should a verbose output be printed on the console?
#' @return Returns a tibble of substance id-s.
#' @param ... currently not used.
#' #' @details \code{<xref>} is assembled as "\code{xref}/\{\code{RegistryID} |
#' \code{RN} | \code{PubMedID} | \code{MMDBID} | \code{ProteinGI},
#' \code{NucleotideGI} | \code{TaxonomyID} | \code{MIMID} | \code{GeneID} |
#' \code{ProbeID} | \code{PatentID}\}", e.g. \code{from = "xref/RN"} will query by
#' CAS RN.
#' @details \code{<source id>} is any valid PubChem Data Source ID.
#' @details If \code{from = "sourceall"} the query is one or more valid Pubchem
#' depositor names. Depositor names are not case sensitive, but are sensitive to
#' spaces, and special characters may need to be escaped, such as "&" should be
#' replaced by "\%26".
#' @details Depositor names and Data Source IDs can be found at
#' \url{https://pubchem.ncbi.nlm.nih.gov/sources/}.
#' @references Wang, Y., J. Xiao, T. O. Suzek, et al. (2009). PubChem: A Public
#' Information System for Analyzing Bioactivities of Small Molecules. Nucleic
#' Acids Research 37: 623–633.
#' @references Kim, S., Thiessen, P. A., Bolton, E. E., & Bryant, S. H. (2015).
#' PUG-SOAP and PUG-REST: web services for programmatic access to chemical
#' information in PubChem. Nucleic acids research, gkv396.
#' @references Kim, Sunghwan, Paul A. Thiessen, Evan E. Bolton, et al. (2016).
#' PubChem Substance and Compound Databases. Nucleic Acids Research 44(D1):
#' D1202–D1213.
#' @references Sunghwan Kim, Paul A Thiessen, Tiejun Cheng, Bo Yu, Evan E Bolton
#' (2018) An update on PUG-REST: RESTful interface for programmatic access to
#' PubChem. Nucleic Acids Research 46(W1): W563–W570.
#' @note Please respect the Terms and Conditions of the National Library of
#' Medicine, \url{https://www.nlm.nih.gov/databases/download.html} and the data
#' usage policies of National Center for Biotechnology Information,
#' \url{https://www.ncbi.nlm.nih.gov/home/about/policies/},
#' \url{https://pubchemdocs.ncbi.nlm.nih.gov/programmatic-access}.
#' @author Tamás Stirling, \email{stirling.tamas@@gmail.com}
#' @importFrom httr POST content
#' @importFrom jsonlite fromJSON
#' @importFrom tibble as_tibble
#' @examples
#' # might fail if API is not available
#' \donttest{
#' get_sid("2-(Acetyloxy)benzoic acid", from = "name")
#' get_sid("57-27-2", from = "xref/rn")
#' get_sid(c("VCC957895", "VCC883692"), from = "sourceid/23706")
#' get_sid("Optopharma Ltd", from = "sourceall")
#' }
get_sid <- function(query,
                    from = "name",
                    match = c("all", "first", "ask", "na"),
                    verbose = TRUE,
                    ...) {
  from <- tolower(from)
  if (grepl("^sourceid/", from) == FALSE) {
    xref <- paste("xref", c("registryid", "rn", "pubmedid", "mmdbid",
                            "proteingi", "nucleotidegi", "taxonomyid", "mimid",
                            "geneid", "probeid", "patentid"), sep = "/")
    from <- match.arg(from, choices = c("name", xref, "sourceall"))
  }
  match <- match.arg(match)
  foo <- function(query, from, match, verbose, ...) {
    if (is.na(query)) return(NA)
    qurl <- paste("https://pubchem.ncbi.nlm.nih.gov/rest/pug/substance/",
                  from, query, "sids", "json", sep = "/")
    if (verbose) {
      message(paste0("Querying ", query, ". "), appendLF = FALSE)
    }
    Sys.sleep(rgamma(1, shape = 15, scale = 1 / 10))
    res <- httr::POST(qurl, user_agent("webchem"), handle = handle(""))
    if (res$status_code != 200) {
      if (res$status_code == 202) {
        cont <- httr::content(res, type = "text", encoding = "UTF-8")
        listkey <- jsonlite::fromJSON(cont)$Waiting$ListKey
        qurl <- paste("https://pubchem.ncbi.nlm.nih.gov/rest/pug/substance",
                      "listkey", listkey, "sids", "json", sep = "/")
        while (res$status_code == 202) {
          Sys.sleep(5 + rgamma(1, shape = 15, scale = 1 / 10))
          res <- httr::POST(qurl, user_agent("webchem"), handle = handle(""))
        }
        if (res$status_code != 200) {
          if (verbose) message(httr::message_for_status(res))
          return(NA)
        }
      }
      else{
        if (verbose) message(httr::message_for_status(res))
        return(NA)
      }
    }
    if (verbose) message(httr::message_for_status(res))
    cont <- httr::content(res, type = "text", encoding = "UTF-8")
    cont <- jsonlite::fromJSON(cont)$IdentifierList$SID
    out <- unique(unlist(cont))
    out <- matcher(x = out, match = match, verbose = verbose)
    out <- as.character(out)
    names(out) <- NULL
    return(out)
  }
  out <- map(query,
             ~foo(query = .x, from = from, match = match,
                  verbose = verbose, arg = arg))
  out <- setNames(out, query)
  out <-
    lapply(out, enframe, name = NULL, value = "sid") %>%
    bind_rows(.id = "query")
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
#' cids <- get_cid(comp)
#' pc_prop(cids$cid, properties = c("MolecularFormula", "MolecularWeight",
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
