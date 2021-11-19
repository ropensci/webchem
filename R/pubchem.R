#' Retrieve Pubchem Compound ID (CID)
#'
#' Retrieve compound IDs (CIDs) from PubChem.
#' @param query character; search term, one or more compounds.
#' @param from character; type of input. See details for more information.
#' @param domain character; query domain, can be one of \code{"compound"},
#' \code{"substance"}, \code{"assay"}.
#' @param match character; How should multiple hits be handled?, \code{"all"}
#' all matches are returned, \code{"first"} the first matching is returned,
#' \code{"ask"} enters an interactive mode and the user is asked for input,
#' \code{"na"} returns NA if multiple hits are found.
#' @param verbose logical; should a verbose output be printed on the console?
#' @param arg character; optinal arguments like "name_type=word" to match
#' individual words.
#' @param first deprecated. Use `match` instead.
#' @param ... currently unused.
#' @return a tibble.
#' @details Valid values for the \code{from} argument depend on the
#' \code{domain}:
#' \itemize{
#' \item{\code{compound}: \code{"name"}, \code{"smiles"}, \code{"inchi"},
#' \code{"inchikey"}, \code{"formula"}, \code{"sdf"}, <xref>,
#' <structure search>, <fast search>.}
#' \item{\code{substance}: \code{"name"}, \code{"sid"},
#' \code{<xref>}, \code{"sourceid/<source id>"} or \code{"sourceall"}.}
#' \item{\code{assay}: \code{"aid"}, \code{<assay target>}.}
#' }
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
#' @details \code{<source id>} is any valid PubChem Data Source ID. When
#' \code{from = "sourceid/<source id>"}, the query is the ID of the substance in
#' the depositor's database.
#' @details If \code{from = "sourceall"} the query is one or more valid Pubchem
#' depositor names. Depositor names are not case sensitive.
#' @details Depositor names and Data Source IDs can be found at
#' \url{https://pubchem.ncbi.nlm.nih.gov/sources/}.
#' @details \code{<assay target>} is assembled as "\code{target}/\{\code{gi} |
#' \code{proteinname} | \code{geneid} | \code{genesymbol} | \code{accession}\}",
#' e.g. \code{from = "target/geneid"} will query by GeneID.
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
#' @references Eduard Szöcs, Tamás Stirling, Eric R. Scott, Andreas Scharmüller,
#' Ralf B. Schäfer (2020). webchem: An R Package to Retrieve Chemical
#' Information from the Web. Journal of Statistical Software, 93(13).
#' \doi{10.18637/jss.v093.i13}.
#' @note Please respect the Terms and Conditions of the National Library of
#' Medicine, \url{https://www.nlm.nih.gov/databases/download.html} the data
#' usage policies of National Center for Biotechnology Information,
#' \url{https://www.ncbi.nlm.nih.gov/home/about/policies/},
#' \url{https://pubchemdocs.ncbi.nlm.nih.gov/programmatic-access}, and the data
#' usage policies of the indicidual data sources
#' \url{https://pubchem.ncbi.nlm.nih.gov/sources/}.
#' @import httr
#' @importFrom purrr map map2
#' @importFrom jsonlite fromJSON
#' @importFrom tibble enframe
#' @importFrom utils URLencode
#' @export
#' @examples
#' \donttest{
#' # might fail if API is not available
#' get_cid("Triclosan")
#' get_cid("Triclosan", arg = "name_type=word")
#' # from SMILES
#' get_cid("CCCC", from = "smiles")
#' # from InChI
#' get_cid("InChI=1S/CH5N/c1-2/h2H2,1H3", from = "inchi")
#' # from InChIKey
#' get_cid("BPGDAMSIGCZZLK-UHFFFAOYSA-N", from = "inchikey")
#' # from formula
#' get_cid("C26H52NO6P", from = "formula")
#' # from CAS RN
#' get_cid("56-40-6", from = "xref/rn")
#' # similarity
#' get_cid(5564, from = "similarity/cid")
#' get_cid("CCO", from = "similarity/smiles")
#' # from SID
#' get_cid("126534046", from = "sid", domain = "substance")
#' # sourceid
#' get_cid("VCC957895", from = "sourceid/23706", domain = "substance")
#' # sourceall
#' get_cid("Optopharma Ltd", from = "sourceall", domain = "substance")
#' # from AID (CIDs of substances tested in the assay)
#' get_cid(170004, from = "aid", domain = "assay")
#' # from GeneID (CIDs of substances tested on the gene)
#' get_cid(25086, from = "target/geneid", domain = "assay")
#'
#' # multiple inputs
#' get_cid(c("Triclosan", "Aspirin"))
#'
#' }
get_cid <-
  function(query,
           from = "name",
           domain = c("compound", "substance", "assay"),
           match = c("all", "first", "ask", "na"),
           verbose = getOption("verbose"),
           arg = NULL,
           first = NULL,
           ...) {

    if (!ping_service("pc")) stop(webchem_message("service_down"))

  #deprecate `first`
  if (!is.null(first) && first) {
    message("`first = TRUE` is deprecated. Use `match = 'first'` instead")
    match <- "first"
  } else if (!is.null(first) && !first) {
    message("`first = FALSE` is deprecated. Use `match = 'all'` instead")
    match <- "all"
  }
    #input validation
    from <- tolower(from)
    domain <- match.arg(domain)
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
    structure_search <- paste(structure_search$Var1, structure_search$Var2,
                              sep = "/")
    fast_search <- expand.grid(
      c("fastidentity", "fastsimilarity_2d", "fastsimilarity_3d",
        "fastsubstructure", "fastsuperstructure"),
      c("smiles", "smarts", "inchi", "sdf", "cid")
    )
    fast_search <- c(with(fast_search, paste(Var1, Var2, sep = "/")),
                     "fastformula")
    targets <- paste("target", c("gi", "proteinname", "geneid", "genesymbol",
                                 "accession"), sep = "/")
    if (domain == "compound") {
      from_choices <- c("cid", "name", "smiles", "inchi", "sdf", "inchikey",
                        "formula", structure_search, xref, fast_search)
      from <- match.arg(from, choices = from_choices)
    }
    if (domain == "substance") {
      if (!grepl("^sourceid/", from)) {
        from <- match.arg(from, choices = c("sid", "name", xref, "sourceall"))
      }
    }
    if (domain == "assay") {
      from <- match.arg(from, choices = c("aid", targets))
    }
    match <- match.arg(match)
    foo <- function(query, from, domain, match, verbose, arg, ...) {
      if (is.na(query)) {
        if (verbose) webchem_message("na")
        return(tibble::tibble("query" = NA, "cid" = NA))
      }
      if (verbose) webchem_message("query", query, appendLF = FALSE)
      if (from %in% structure_search) {
        qurl <- paste("https://pubchem.ncbi.nlm.nih.gov/rest/pug",
                      domain,
                      from,
                      URLencode(as.character(query), reserved = TRUE),
                      "json",
                      sep = "/")
      } else {
        qurl <- paste("https://pubchem.ncbi.nlm.nih.gov/rest/pug",
                      domain,
                      from,
                      URLencode(as.character(query), reserved = TRUE),
                      "cids",
                      "json",
                      sep = "/")
      }
      if (!is.null(arg)) qurl <- paste0(qurl, "?", arg)
      webchem_sleep(type = 'API')
      if (from == "inchi") {
        qurl <- paste("https://pubchem.ncbi.nlm.nih.gov/rest/pug",
                      domain, from, "cids", "json", sep = "/")
        res <- try(httr::RETRY("POST",
                               qurl,
                               user_agent(webchem_url()),
                               body = paste0("inchi=", query),
                               terminate_on = 404,
                               quiet = TRUE), silent = TRUE)
      } else {
        res <- try(httr::RETRY("POST",
                               qurl,
                               user_agent(webchem_url()),
                               terminate_on = c(202, 404),
                               quiet = TRUE), silent = TRUE)
      }
      if (inherits(res, "try-error")) {
        if (verbose) webchem_message("service_down")
        return(tibble::tibble("query" = query, "cid" = NA))
      }
      if (res$status_code != 200) {
        if (res$status_code == 202) {
          cont <- httr::content(res, type = "text", encoding = "UTF-8")
          listkey <- jsonlite::fromJSON(cont)$Waiting$ListKey
          qurl <- paste("https://pubchem.ncbi.nlm.nih.gov/rest/pug/", domain,
                        "listkey", listkey, "cids", "json", sep = "/")
          while (res$status_code == 202) {
            webchem_sleep(time = 5)
            res <- try(httr::RETRY("POST",
                                   qurl,
                                   user_agent(webchem_url()),
                                   terminate_on = 404,
                                   quiet = TRUE), silent = TRUE)
            if (inherits(res, "try-error")) {
              if (verbose) webchem_message("service_down")
              return(tibble::tibble("query" = query, "cid" = NA))
            }
          }
          if (res$status_code != 200) {
            if (verbose) message(httr::message_for_status(res))
            return(tibble::tibble("query" = query, "cid" = NA))
          }
        }
        else{
          if (verbose) message(httr::message_for_status(res))
          return(tibble::tibble("query" = query, "cid" = NA))
        }
      }
      if (verbose) message(httr::message_for_status(res))
      cont <- httr::content(res, type = "text", encoding = "UTF-8")
      if (domain == "compound") {
        cont <- jsonlite::fromJSON(cont)$IdentifierList$CID
      }
      if (domain == "substance") {
        cont <- jsonlite::fromJSON(cont)$InformationList$Information$CID
      }
      if (domain == "assay") {
        cont <- jsonlite::fromJSON(cont)$InformationList$Information$CID
      }
      out <- unique(unlist(cont))
      out <- matcher(x = out, query = query, match = match, from = from,
                     verbose = verbose)
      out <- as.character(out)
      return(tibble::tibble("query" = query, "cid" = out))
    }
    out <- map(query,
             ~foo(query = .x, from = from, domain = domain, match = match,
                  verbose = verbose, arg = arg))
    out <- dplyr::bind_rows(out)
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
#' \url{https://pubchemdocs.ncbi.nlm.nih.gov/pug-rest}
#' for a list of all available properties.
#' @param verbose logical; should a verbose output be printed to the console?
#' @param ... currently not used.
#'
#' @return a data.frame
#' @seealso \code{\link{get_cid}}, \code{\link{pc_sect}}
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
#' @references Eduard Szöcs, Tamás Stirling, Eric R. Scott, Andreas Scharmüller,
#' Ralf B. Schäfer (2020). webchem: An R Package to Retrieve Chemical
#' Information from the Web. Journal of Statistical Software, 93(13).
#' \doi{10.18637/jss.v093.i13}.
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
#' cids <- get_cid(comp)
#' pc_prop(cids$cid, properties = c("MolecularFormula", "MolecularWeight",
#' "CanonicalSMILES"))
#' }
pc_prop <- function(cid, properties = NULL, verbose = getOption("verbose"), ...) {

  if (!ping_service("pc")) stop(webchem_message("service_down"))

  if (mean(is.na(cid)) == 1) {
    if (verbose) webchem_message("na")
    return(NA)
  }
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
  if (verbose) webchem_message("query_all", appendLF = FALSE)
  webchem_sleep(type = 'API')
  res <- try(httr::RETRY("POST",
                         qurl,
                         httr::user_agent(webchem_url()),
                         body = list("cid" = paste(cid, collapse = ",")),
                         terminate_on = 404,
                         quiet = TRUE), silent = TRUE)
  if (inherits(res, "try-error")) {
    if (verbose) webchem_message("service_down")
    return(NA)
  }
  if (verbose) message(httr::message_for_status(res))
  if (res$status_code == 200) {
    cont <- jsonlite::fromJSON(rawToChar(res$content))
    if (names(cont) == "Fault") {
      if (verbose) {
        message(cont$Fault$Message, ". ", cont$Fault$Details, ". Returning NA.")
      }
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
  else {
    return(NA)
  }
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
#' @param match character; How should multiple hits be handled? \code{"all"}
#' returns all matches, \code{"first"} returns only the first result,
#' \code{"ask"} enters an interactive mode and the user is asked for input,
#' \code{"na"} returns \code{NA} if multiple hits are found.
#' @param choices deprecated.  Use the \code{match} argument instead.
#' @param verbose logical; should a verbose output be printed on the console?
#' @param arg character; optional arguments like "name_type=word" to match
#' individual words.
#' @param ... currently unused
#' @return a named list.
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
#' @export
#' @examples
#' \donttest{
#' pc_synonyms("Aspirin")
#' pc_synonyms(c("Aspirin", "Triclosan"))
#' pc_synonyms(5564, from = "cid")
#' pc_synonyms(c("Aspirin", "Triclosan"), match = "ask")
#' }
pc_synonyms <- function(query,
                        from = c("name", "cid", "sid", "aid", "smiles", "inchi", "inchikey"),
                        match = c("all", "first", "ask", "na"),
                        verbose = getOption("verbose"),
                        arg = NULL, choices = NULL, ...) {

  if (!ping_service("pc")) stop(webchem_message("service_down"))

  # from can be cid | name | smiles | inchi | sdf | inchikey | formula
  # query <- c("Aspirin")
  # from = "name"
  from <- match.arg(from)
  match <- match.arg(match)
  if (!missing("choices"))
    stop("'choices' is deprecated. Use 'match' instead.")
  foo <- function(query, from, verbose, ...) {
    if (is.na(query)) {
      if (verbose) webchem_message("na")
      return(NA)
    }
    prolog <- "https://pubchem.ncbi.nlm.nih.gov/rest/pug"
    input <- paste0("/compound/", from)
    output <- "/synonyms/JSON"
    if (!is.null(arg))
      arg <- paste0("?", arg)
    qurl <- paste0(prolog, input, output, arg)
    if (verbose) webchem_message("query", query, appendLF = FALSE)
    webchem_sleep(type = 'API')
    res <- try(httr::RETRY("POST",
                           qurl,
                           httr::user_agent(webchem_url()),
                           body = paste0(from, "=", query),
                           terminate_on = 404,
                           quiet = TRUE), silent = TRUE)
    if (inherits(res, "try-error")) {
      if (verbose) webchem_message("service_down")
      return(NA)
    }
    if (verbose) message(httr::message_for_status(res))
    if (res$status_code == 200){
      cont <- httr::content(res)
      if (names(cont) == "Fault") {
        message(cont$Fault$Details, ". Returning NA.")
        return(NA)
      }
      out <- unlist(cont)[-1] #first result is always an ID number
      names(out) <- NULL

      out <- matcher(out, query = query, match = match, from = from,
                     verbose = verbose)
    }
    else {
      return(NA)
    }
  }
  out <- lapply(query, foo, from = from, verbose = verbose)
  names(out) <- query
  if (!is.null(choices)) #if only one choice is returned, convert list to vector
    out <- unlist(out)
  return(out)
}

#' Retrieve data from PubChem content pages
#'
#' When you search for an entity at \url{https://pubchem.ncbi.nlm.nih.gov/},
#' e.g. a compound or a substance, and select the record you are interested in,
#' you will be forwarded to a PubChem content page. When you look at a PubChem
#' content page, you can see that chemical information is organised into
#' sections, subsections, etc. The chemical data live at the lowest levels of
#' these sections. Use this function to retrieve the lowest level information
#' from PubChem content pages.
#' @param id numeric or character; a vector of PubChem identifiers to search
#' for.
#' @param section character; the section of the content page to be imported.
#' @param domain character; the query domain. Can be one of \code{"compound"},
#' \code{"substance"}, \code{"assay"}, \code{"gene"}, \code{"protein"} or
#' \code{"patent"}.
#' @param verbose logical; should a verbose output be printed on the console?
#' @return Returns a tibble of query results. In the returned tibble,
#' \code{SourceName} is the name of the depositor, and \code{SourceID} is the
#' ID of the search term within the depositor's database. You can browse
#' \url{https://pubchem.ncbi.nlm.nih.gov/sources/} for more information about
#' the depositors.
#' @details \code{section} is not case sensitive but it is sensitive to typing
#' errors and it requires the full name of the section as it is printed on the
#' content page. The PubChem Table of Contents Tree can also be found at
#' \url{https://pubchem.ncbi.nlm.nih.gov/classification/#hid=72}.
#' @note Please respect the Terms and Conditions of the National Library of
#' Medicine, \url{https://www.nlm.nih.gov/databases/download.html} the data
#' usage policies of National Center for Biotechnology Information,
#' \url{https://www.ncbi.nlm.nih.gov/home/about/policies/},
#' \url{https://pubchemdocs.ncbi.nlm.nih.gov/programmatic-access}, and the data
#' usage policies of the individual data sources
#' \url{https://pubchem.ncbi.nlm.nih.gov/sources/}.
#' @references Kim, S., Thiessen, P.A., Cheng, T. et al. PUG-View: programmatic
#' access to chemical annotations integrated in PubChem. J Cheminform 11, 56
#' (2019). \doi{10.1186/s13321-019-0375-2}.
#' @seealso \code{\link{get_cid}}, \code{\link{pc_prop}}
#' @examples
#' # might fail if API is not available
#' \donttest{
#' pc_sect(176, "Dissociation Constants")
#' pc_sect(c(176, 311), "density")
#' pc_sect(2231, "depositor-supplied synonyms", "substance")
#' pc_sect(780286, "modify date", "assay")
#' pc_sect(9023, "Ensembl ID", "gene")
#' pc_sect("1ZHY_A", "Sequence", "protein")
#' }
#' @export
pc_sect <- function(id,
                    section,
                    domain = c("compound", "substance", "assay", "gene",
                                "protein", "patent"),
                    verbose = getOption("verbose")) {
  domain <- match.arg(domain)
  section <- tolower(gsub(" +", "+", section))
  if (section %in% c("standard non-polar",
                     "Semi-standard non-polar",
                     "Standard polar")) {
    stop("use nist_ri() to obtain more information on this.")
  }
  res <- pc_page(id, section, domain, verbose)
  out <- pc_extract(res, section)
  return(out)
}

#' Import PubChem content pages
#'
#' @importFrom jsonlite fromJSON
#' @importFrom data.tree as.Node Do
#' @param id numeric or character; a vector of identifiers to search for.
#' @param section character; the section of the content page to be imported.
#' @param domain character; the query domain. Can be one of \code{"compound"},
#' \code{"substance"}, \code{"assay"}, \code{"gene"}, \code{"protein"} or
#' \code{"patent"}.
#' @return A named list of content pages where each element is either a
#' data.tree or NA.
#' @details \code{section} can be any section of a PubChem content page, e.g.
#' \code{section = "solubility"} will import the section on solubility, or
#' \code{section = "experimental properties"} will import all experimental
#' properties. The \code{section} argument is not case sensitive but it
#' is sensitive to typing errors and it requires the full name of the section as
#' it is printed on the content page. The PubChem Table of Contents Tree can
#' also be found at
#' \url{https://pubchem.ncbi.nlm.nih.gov/classification/#hid=72}.
#' @references Kim, S., Thiessen, P.A., Cheng, T. et al. PUG-View: programmatic
#' access to chemical annotations integrated in PubChem. J Cheminform 11, 56
#' (2019). \doi{10.1186/s13321-019-0375-2}.
#' @examples
#' # might fail if API is not available
#' \donttest{
#' pc_page(c(176, 311), "Dissociation Constants")
#' pc_page(49854366, "external id", domain = "substance")
#' }
#' @noRd
pc_page <- function(id,
                    section,
                    domain = c("compound", "substance", "assay", "gene",
                               "protein", "patent"),
                    verbose = getOption("verbose")) {

  if (!ping_service("pc")) stop(webchem_message("service_down"))

  domain <- match.arg(domain)
  section <- tolower(gsub(" +", "+", section))
  foo <- function(id, section, domain) {
    if (is.na(id)) {
      if (verbose) webchem_message("na")
      return(NA)
    }
    qurl <- paste0("https://pubchem.ncbi.nlm.nih.gov/rest/pug_view/data/",
                   domain, "/", id, "/JSON?heading=", section)
    if (verbose) webchem_message("query", id, appendLF = FALSE)
    webchem_sleep(type = 'API')
    res <- try(httr::RETRY("POST",
                           qurl,
                           user_agent(webchem_url()),
                           terminate_on = 404,
                           quiet = TRUE), silent = TRUE)
    if (inherits(res, "try-error")) {
      if (verbose) webchem_message("service_down")
      return(NA)
    }
    if (verbose) message(httr::message_for_status(res))
    if (res$status_code == 200) {
      cont <- httr::content(res, type = "text", encoding = "UTF-8")
      cont <- jsonlite::fromJSON(cont, simplifyDataFrame = FALSE)
      tree <- data.tree::as.Node(cont, nameName = "TOCHeading")
      tree$Do(function(node) node$name <- tolower(node$name))
      return(tree)
    }
    else {
      return(NA)
    }
  }
  cont <- lapply(id, function(x) foo(x, section, domain))
  names(cont) <- id
  attr(cont, "domain") <- domain
  attr(cont, "id") <- switch(domain, compound = "CID", substance = "SID",
                             assay = "AID", gene = "GeneID", protein = "pdbID",
                             patent = "PatentID")
  return(cont)
}

#' Extract data from PubChem content pages
#'
#' This function takes a list of PubChem content pages, and extracts the
#' required information from them.
#' @importFrom data.tree FindNode
#' @importFrom dplyr bind_rows
#' @importFrom tibble as_tibble
#' @param pages list; a list of PubChem content pages.
#' @param section character; the lowest level section of the data to be
#' accessed.
#' @return A tibble of chemical information with references.
#' @details When you look at a PubChem content page, you can see that chemical
#' information is organised into sections, subsections, etc. The chemical data
#' live at the lowest levels of these sections. Use this function to extract the
#' lowest level information from PubChem content pages, e.g. IUPAC Name, Boiling
#' Point, Lower Explosive Limit (LEL).
#' @details The \code{section} argument is not case sensitive, but it is
#' sensitive to typing errors, and requires the full name of the section as it
#' is printed on the content page. The PubChem Table of Contents Tree can also
#' be found at \url{https://pubchem.ncbi.nlm.nih.gov/classification/#hid=72}.
#' @references Kim, S., Thiessen, P.A., Cheng, T. et al. PUG-View: programmatic
#' access to chemical annotations integrated in PubChem. J Cheminform 11, 56
#' (2019). \doi{10.1186/s13321-019-0375-2}.
#' @examples
#' # might fail if API is not available
#' \donttest{
#' comps <- pc_page(c(176, 311), "Dissociation Constants")
#' pc_extract(comps, "Dissociation Constants")
#' subs <- pc_page(49854366, "external id", domain = "substance")
#' pc_extract(subs, "external id")
#' }
#' @noRd
pc_extract <- function(page, section) {
  section <- tolower(section)
  ids <- names(page)
  foo <- function(i, section) {
    tree <- page[[i]]
    if (length(tree) == 1 && is.na(tree)) return(tibble(ID = ids[i]))
    node <- FindNode(tree, "information")
    if (is.null(node)) return(tibble(ID = ids[i],
                                     Name = tree$record$RecordTitle))
    info <- lapply(node, function(y) {
      lownode <- data.tree::FindNode(data.tree::as.Node(y), "stringwithmarkup")
      if (is.null(lownode)) {
        info <- tibble(Result = paste(y$value, collapse = " "),
                       ReferenceNumber = y$ReferenceNumber)
        return(info)
      }
      else{
        string <- sapply(lownode, function(z) z$String)
        info <- tibble(Result =  string,
                       ReferenceNumber = y$ReferenceNumber)
      }
    })
    info <- dplyr::bind_rows(info)
    info <- tibble(ID = ids[i],
                   Name = tree$record$RecordTitle,
                   info)
    node <- FindNode(tree, "reference")
    if (is.null(node)) return(tibble(info, SourceName = NA, SourceID = NA))
    ref <- lapply(node, function(y) {
      ref <- tibble(ReferenceNumber = y$ReferenceNumber,
                    SourceName = y$SourceName,
                    SourceID = y$SourceID)
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
  info <- lapply(seq_along(page), function(x) foo(x, section))
  info <- dplyr::bind_rows(info)
  info <- info[, -which(names(info) == "ReferenceNumber")]
  names(info)[1] <- attr(page, "id")
  return(info)
}
