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
#' \code{"inchikey"}, \code{"formula"}, \code{"sdf"}, \code{"cas"} (an alias for
#' \code{"xref/RN"}), <xref>, <structure search>, <fast search>.}
#' \item{\code{substance}: \code{"name"}, \code{"sid"},
#' \code{<xref>}, \code{"sourceid/<source id>"} or \code{"sourceall"}.}
#' \item{\code{assay}: \code{"aid"}, \code{<assay target>}.}
#' }
#' @details <structure search> is assembled as "(\code{substructure} |
#' \code{superstructure} | \code{similarity} | \code{identity}) / (\code{smiles}
#'  | \code{inchi} | \code{sdf} | \code{cid})", e.g.
#'  \code{from = "substructure/smiles"}.
#' @details \code{<xref>} is assembled as "\code{xref}/(\code{RegistryID} |
#' \code{RN} | \code{PubMedID} | \code{MMDBID} | \code{ProteinGI},
#' \code{NucleotideGI} | \code{TaxonomyID} | \code{MIMID} | \code{GeneID} |
#' \code{ProbeID} | \code{PatentID})", e.g. \code{from = "xref/RN"} will query
#' by CAS RN.
#' @details <fast search> is either \code{fastformula} or it is assembled as
#' "(\code{fastidentity} | \code{fastsimilarity_2d} | \code{fastsimilarity_3d} |
#' \code{fastsubstructure} | \code{fastsuperstructure})/(\code{smiles} |
#' \code{smarts} | \code{inchi} | \code{sdf} | \code{cid})", e.g.
#' \code{from = "fastidentity/smiles"}.
#' @details \code{<source id>} is any valid PubChem Data Source ID. When
#' \code{from = "sourceid/<source id>"}, the query is the ID of the substance in
#' the depositor's database.
#' @details If \code{from = "sourceall"} the query is one or more valid Pubchem
#' depositor names. Depositor names are not case sensitive.
#' @details Depositor names and Data Source IDs can be found at
#' \url{https://pubchem.ncbi.nlm.nih.gov/sources/}.
#' @details \code{<assay target>} is assembled as "\code{target}/(\code{gi} |
#' \code{proteinname} | \code{geneid} | \code{genesymbol} | \code{accession})",
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
#' \url{https://pubchem.ncbi.nlm.nih.gov/docs/programmatic-access}, and the data
#' usage policies of the indicidual data sources
#' \url{https://pubchem.ncbi.nlm.nih.gov/sources/}.
#' @import httr
#' @importFrom purrr map map2
#' @importFrom jsonlite fromJSON
#' @importFrom tibble enframe
#' @importFrom utils URLencode
#' @export
#' @examples
#' \dontrun{
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
    from <- ifelse(from == "cas", "xref/rn", from)
    if (from == "xref/rn"){
       query <- as.cas(query, verbose = verbose)
    }
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
        qurl <- paste(
          "https://pubchem.ncbi.nlm.nih.gov/rest/pug",
          domain,
          from,
          URLencode(as.character(query), reserved = TRUE),
          "json",
          sep = "/"
        )
      } else if (from == "smiles") {
        qurl <- paste0(
          "https://pubchem.ncbi.nlm.nih.gov/rest/pug/",
          domain, "/",
          from, "/",
          "cids/JSON?smiles=",
          URLencode(as.character(query), reserved = TRUE)
        )
      } else if (from == "inchi") {
        qurl <- paste(
          "https://pubchem.ncbi.nlm.nih.gov/rest/pug",
          domain,
          from,
          "cids",
          "json",
          sep = "/"
          )
      } else {
        qurl <- paste(
          "https://pubchem.ncbi.nlm.nih.gov/rest/pug",
          domain,
          from,
          URLencode(as.character(query), reserved = TRUE),
          "cids",
          "json",
          sep = "/"
        )
      }
      if (!is.null(arg)) qurl <- paste0(qurl, "?", arg)
      webchem_sleep(type = 'API')
      if (from == "inchi") {
        res <- try(httr::RETRY("POST",
                               qurl,
                               user_agent(webchem_url()),
                               body = paste0("inchi=", query),
                               terminate_on = 404,
                               quiet = TRUE), silent = TRUE)
      } else {
        res <- try(httr::RETRY("GET",
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
            res <- try(httr::RETRY("GET",
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
#' @param cid numeric; a vector of Pubchem IDs (CIDs). The input vector will be
#' coerced to a vector of positive integers. The function will return a row of
#' NAs for elements that cannot be coerced to positive integers.
#' @param properties character; a vector of properties to retrieve, e.g.
#' c("MolecularFormula", "MolecularWeight"). If NULL (default) all available
#' properties are retrieved. See
#' \url{https://pubchem.ncbi.nlm.nih.gov/docs/pug-rest#section=Compound-Property-Tables}
#' for a list of all available properties.
#' @param verbose logical; should a verbose output be printed to the console?
#' @param ... currently not used.
#'
#' @return a tibble; each row is a queried CID, each column is a requested
#' property.
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
#' \url{https://pubchem.ncbi.nlm.nih.gov/docs/programmatic-access}, and the data
#' usage policies of the indicidual data sources
#' \url{https://pubchem.ncbi.nlm.nih.gov/sources/}.
#' @export
#' @examples
#' \dontrun{
#' # might fail if API is not available
#' pc_prop(5564)
#'
#' ###
#' # multiple CIDS
#' comp <- c("Triclosan", "Aspirin")
#' cids <- get_cid(comp)
#' pc_prop(cids$cid, properties = c("MolecularFormula", "MolecularWeight",
#' "SMILES"))
#' }
pc_prop <- function(cid, properties = NULL, verbose = getOption("verbose"), ...) {

  if (!ping_service("pc")) stop(webchem_message("service_down"))

  cid_o <- cid
  cid <- suppressWarnings(as.integer(cid))

  invalid <- is.na(cid) | cid <= 0
  cid[invalid] <- NA_integer_

  if (verbose) {
    message("Coercing queries to positive integers. ", appendLF = FALSE)
    index <- which(invalid & !is.na(cid_o))
    if (length(index) > 0) {
      for (i in index) {
        message(paste0(cid_o[i], " coerced to NA. "), appendLF = FALSE)
      }
    }
    message("Done.")
  }

  vcids <- tibble::tibble(
    query = cid_o,
    cid = cid
  )

  if (mean(is.na(vcids$cid)) == 1) {
    if (verbose) webchem_message("na")
    return(NA)
  }

  cid <- vcids$cid[!is.na(vcids$cid)]
  prolog <- "https://pubchem.ncbi.nlm.nih.gov/rest/pug"
  input <- "/compound/cid/"
  all_properties <- c(
    "MolecularFormula",
    "MolecularWeight",
    "SMILES",
    "ConnectivitySMILES",
    "InChI",
    "InChIKey",
    "IUPACName",
    "Title",
    "XLogP",
    "ExactMass",
    "MonoisotopicMass",
    "TPSA",
    "Complexity",
    "Charge",
    "HBondDonorCount",
    "HBondAcceptorCount",
    "RotatableBondCount",
    "HeavyAtomCount",
    "IsotopeAtomCount",
    "AtomStereoCount",
    "DefinedAtomStereoCount",
    "UndefinedAtomStereoCount",
    "BondStereoCount",
    "DefinedBondStereoCount",
    "UndefinedBondStereoCount",
    "CovalentUnitCount",
    "PatentCount",
    "PatentFamilyCount",
    "AnnotationTypes",
    "AnnotationTypeCount",
    "SourceCategories",
    "LiteratureCount",
    "Volume3D",
    "XStericQuadrupole3D",
    "YStericQuadrupole3D",
    "ZStericQuadrupole3D",
    "FeatureCount3D",
    "FeatureAcceptorCount3D",
    "FeatureDonorCount3D",
    "FeatureAnionCount3D",
    "FeatureCationCount3D",
    "FeatureRingCount3D",
    "FeatureHydrophobeCount3D",
    "ConformerModelRMSD3D",
    "EffectiveRotorCount3D",
    "ConformerCount3D",
    "Fingerprint2D"
  )
  if (is.null(properties)) {
    properties <- all_properties
  } else {
    invalid_props <- setdiff(properties, all_properties)
    if (length(invalid_props) > 0) {
      stop(
        "Invalid properties: ",
        paste(invalid_props, collapse = ", "),
        ". Valid properties: ",
        all_properties |> sort() |> paste(collapse = ", "),
        call. = FALSE
      )
    }
  }
  properties <- paste(properties, collapse = ",")
  output <- paste0("/property/", properties, "/JSON")

  foo <- function(x) {
    qurl <- paste0(prolog, input, x, output)
    if (verbose) webchem_message("query", x, appendLF = FALSE)
    webchem_sleep(type = 'API')
    res <- try(httr::RETRY("GET",
                           qurl,
                           httr::user_agent(webchem_url()),
                           terminate_on = 404,
                           quiet = TRUE), silent = TRUE)
    if (inherits(res, "try-error")) {
      if (verbose) webchem_message("service_down")
      return(data.frame())
    }
    if (verbose) message(httr::message_for_status(res))
    if (res$status_code == 200) {
      cont <- jsonlite::fromJSON(rawToChar(res$content))
      if (names(cont) == "Fault") {
        if (verbose) {
          message(cont$Fault$Message, ". ", cont$Fault$Details, ". Returning NA.")
        }
        return(data.frame())
      }
      out <- cont$PropertyTable[[1]]
      out <- out |> dplyr::mutate("CID" = x) |> dplyr::relocate("CID")
      return(out)
    } else {
      return(data.frame())
    }
  }
  out <- lapply(cid, foo) |> dplyr::bind_rows()

  if (nrow(out) == 0) return(NA)

  na_row <- as.data.frame(as.list(rep(NA, ncol(out))))
  names(na_row) <- names(out)
  out_list <- lapply(seq_len(nrow(vcids)), function(i) {
    if (is.na(vcids$cid[i])) {
      na_row$CID <- vcids$query[i]
      return(na_row)
    } else {
      hit <- out[which(out$CID == vcids$query[i]),]
      if (nrow(hit) == 0) {
        na_row$CID <- vcids$query[i]
        return(na_row)
      } else {
        return(hit)
      }
    }
  })
  out <- do.call(rbind, out_list)
  out <- tibble::as_tibble(out)
  class(out) <- c("pc_prop", class(out))
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
#' \url{https://pubchem.ncbi.nlm.nih.gov/docs/programmatic-access}, and the data
#' usage policies of the indicidual data sources
#' \url{https://pubchem.ncbi.nlm.nih.gov/sources/}.
#' @export
#' @examples
#' \dontrun{
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
  names(query) <- query

  if (!missing("choices"))
    stop("'choices' is deprecated. Use 'match' instead.")
  foo <- function(x, from, verbose, ...) {
    if (is.na(x)) {
      if (verbose) webchem_message("na")
      return(NA)
    }
    prolog <- "https://pubchem.ncbi.nlm.nih.gov/rest/pug"
    input <- paste0("/compound/", from, "/")
    output <- "/synonyms/JSON"
    if (!is.null(arg))
      arg <- paste0("?", arg)
    qurl <- paste0(prolog, input, utils::URLencode(x), output, arg)
    if (verbose) webchem_message("query", x, appendLF = FALSE)
    webchem_sleep(type = 'API')
    res <- try(httr::RETRY("GET",
                           qurl,
                           httr::user_agent(webchem_url()),
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
  if (!is.null(choices)) #if only one choice is returned, convert list to vector
    out <- unlist(out)
  return(out)
}
