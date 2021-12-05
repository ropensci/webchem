#' Retrieve ChemSpider API key
#'
#' Look for and retrieve ChemSpider API key stored in .Renviron or .Rprofile.
#'
#' @details To use the any of the functions in \code{webchem} that access the
#' ChemSpider database, you'll need to obtain an API key. Register at
#' \url{https://developer.rsc.org/} for an API key. Please respect the Terms &
#' Conditions \url{https://developer.rsc.org/terms}.
#' @details You can store your API key as \code{CHEMSPIDER_KEY = <your key>} in
#' .Renviron or as \code{options(chemspider_key = <your key>)} in .Rprofile.
#' This will allow you to use ChemSpider without adding your API key in the
#' beginning of each session, and will also allow you to share your analysis
#' without sharing your API key. Keeping your API key hidden is good practice.
#' @seealso \code{\link[usethis]{edit_r_environ}}
#' \code{\link[usethis]{edit_r_profile}}
#' @return an API key
#' @export
#' @examples
#' \dontrun{
#' cs_check_key()
#' }
cs_check_key <- function() {
  x <- Sys.getenv("CHEMSPIDER_KEY", "")
  if (x == "") {
    x <- getOption("chemspider_key", "")
  }
  if (x == "")
    stop("no API key stored for ChemSpider.  See ?cs_check_key() for details")
  else x
}

#' Retrieve ChemSpider data sources
#'
#' The function returns a vector of available data sources used by ChemSpider.
#' Some ChemSpider functions allow you to restrict which sources are used to
#' lookup the requested query. Restricting the sources makes these queries
#' faster.
#' @param apikey character; your API key. If NULL (default),
#' \code{cs_check_key()} will look for it in .Renviron or .Rprofile.
#' @param verbose should a verbose output be printed on the console?
#' @return Returns a character vector.
#' @note An API key is needed. Register at \url{https://developer.rsc.org/}
#' for an API key. Please respect the Terms & Conditions. The Terms & Conditions
#' can be found at \url{https://developer.rsc.org/terms}.
#' @references \url{https://developer.rsc.org/docs/compounds-v1-trial/1/overview}
#' @export
#' @examples
#' \dontrun{
#' cs_datasources()
#' }
cs_datasources <- function(apikey = NULL, verbose = getOption("verbose")) {
  if (is.null(apikey)) {
    apikey <- cs_check_key()
  }
  headers <- c("Content-Type" = "", "apikey" = apikey)
  qurl <- "https://api.rsc.org/compounds/v1/lookups/datasources"
  if (verbose) message("Querying list of data sources. ", appendLF = FALSE)
  res <- try(httr::RETRY("GET",
                         url = qurl,
                         httr::add_headers(.headers = headers),
                         terminate_on = 404,
                         quiet = TRUE), silent = TRUE)
  if (inherits(res, "try-error")) {
    if (verbose) webchem_message("service_down")
    return(NA)
  }
  if (res$status_code == 200) {
    if (verbose) message(httr::message_for_status(res))
    out <- unlist(unname(jsonlite::fromJSON(rawToChar(res$content))))
    return(out)
  }
  else {
    if (verbose) message(httr::message_for_status(res))
    return(NA)
  }
}

#' Control ChemSpider API requests
#'
#' For some ChemSpider API requests, you can also specify various control
#' options. This function is used to set these control options.
#' @param datasources character; specifies the databases to query. Use
#' \code{cs_datasources()} to retrieve available ChemSpider data sources.
#' @param order_by character; specifies the sort order for the results.
#' Valid values are \code{"default"}, \code{"recordId"}, \code{"massDefect"},
#' \code{"molecularWeight"}, \code{"referenceCount"}, \code{"dataSourceCount"},
#' \code{"pubMedCount"}, \code{"rscCount"}.
#' @param order_direction character; specifies the sort order for the results.
#' Valid values are \code{"default"}, \code{"ascending"}, \code{"descending"}.
#' @param include_all logical; see details.
#' @param complexity character; see details.
#' Valid values are \code{"any"} \code{"single"}, \code{"multiple"}.
#' @param isotopic character; see details.
#' Valid values are \code{"any"}, \code{"labeled"}, \code{"unlabeled"}.
#' @details The only function that currently uses \code{databases} is
#' \code{get_csid()} and only when you query a CSID from a formula. This
#' parameter is disregarded in all other queries.
#' @details Setting \code{include_all} to \code{TRUE} will consider records
#' which contain all of the filter criteria specified in the request. Setting
#' it to \code{FALSE} will consider records which contain any of the filter
#' criteria.
#' @details A compound with a  \code{complexity} of \code{"multiple"} has more
#' than one disconnected system in it or a metal atom or ion.
#' @return Returns a list of specified control options.
#' @note This is a full list of all API control options.
#' However, not all of these options are used in all functions.
#' Each API uses a subset of these controls.
#' The controls that are available for a given function are indicated within the
#' documentation of the function.
#' @references \url{https://developer.rsc.org/docs/compounds-v1-trial/1/overview}
#' @seealso \code{\link{get_csid}}
#' @export
#' @examples
#' cs_control()
#' cs_control(order_direction = "descending")
cs_control <- function(datasources = vector(),
                       order_by = "default", order_direction = "default",
                       include_all = FALSE, complexity = "any",
                       isotopic = "any") {
  order_by <- match.arg(order_by, choices = c(
    "default", "recordId", "massDefect", "molecularWeight", "referenceCount",
    "dataSourceCount", "pubMedCount", "rscCount"
  ))
  order_direction <- match.arg(order_direction, choices = c(
    "default", "ascending", "descending"))
  include_all <- match.arg(as.character(include_all), choices = c(TRUE, FALSE))
  complexity <- match.arg(complexity, choices = c("any", "single", "multiple"))
  isotopic <- match.arg(isotopic, choices = c("any", "labeled", "unlabeled"))
  return(list(
    "datasources" = datasources,
    "order_by" = order_by, "order_direction" = order_direction,
    "include_all" = include_all, "complexity" = complexity,
    "isotopic" = isotopic
  ))
}

#' ChemSpider ID from compound name, formula, SMILES, InChI or InChIKey
#'
#' Query one or more compunds by name, formula, SMILES, InChI or InChIKey and
#' return a vector of ChemSpider IDs.
#'
#' @param query character; search term.
#' @param apikey character; your API key. If NULL (default),
#'   \code{cs_check_key()} will look for it in .Renviron or .Rprofile.
#' @param from character; the type of the identifier to convert from. Valid
#'   values are \code{"name"}, \code{"formula"}, \code{"smiles"},
#'   \code{"inchi"}, \code{"inchikey"}. The default value is \code{"name"}.
#' @param match character; How should multiple hits be handled?, "all" all
#'   matches are returned, "best" the best matching is returned, "ask" enters an
#'   interactive mode and the user is asked for input, "na" returns NA if
#'   multiple hits are found.
#' @param verbose logical; should a verbose output be printed on the console?
#' @param ... furthrer arguments passed to \code{\link{cs_control}}
#' @details Queries by SMILES, InChI or InChiKey do not use \code{cs_control}
#'   options. Queries by name use \code{order_by} and \code{order_direction}.
#'   Queries by formula also use \code{datasources}. See \code{cs_control()} for
#'   a full list of valid values for these control options.
#' @details \code{formula} can be expressed with and without LaTeX syntax.
#' @return Returns a tibble.
#' @note An API key is needed. Register at \url{https://developer.rsc.org/} for
#'   an API key. Please respect the Terms & conditions:
#'   \url{https://developer.rsc.org/terms}.
#' @references \url{https://developer.rsc.org/docs/compounds-v1-trial/1/overview}
#' @references Eduard Szöcs, Tamás Stirling, Eric R. Scott, Andreas Scharmüller,
#' Ralf B. Schäfer (2020). webchem: An R Package to Retrieve Chemical
#' Information from the Web. Journal of Statistical Software, 93(13).
#' \doi{10.18637/jss.v093.i13}.
#'
#' @export
#' @examples
#' \dontrun{
#' get_csid("triclosan")
#' get_csid(c("carbamazepine", "naproxene","oxygen"))
#' get_csid("C2H6O", from = "formula")
#' get_csid("C_{2}H_{6}O", from = "formula")
#' get_csid("CC(O)=O", from = "smiles")
#' get_csid("InChI=1S/C2H4O2/c1-2(3)4/h1H3,(H,3,4)", from = "inchi")
#' get_csid("QTBSBXVTEAMEQO-UHFFFAOYAR", from = "inchikey")
#' }
get_csid <- function(query,
                     from = c("name", "formula", "inchi", "inchikey", "smiles"),
                     match = c("all", "first", "ask", "na"),
                     verbose = getOption("verbose"),
                     apikey = NULL,
                     ...) {
  if (is.null(apikey)) {
    apikey <- cs_check_key()
  }
  from <- match.arg(from)
  match <- match.arg(match)
  if (!ping_service("cs")) stop(webchem_message("service_down"))
  foo <- function(x, from, match, verbose, apikey, ...) {
    if (is.na(x)) {
      if (verbose) webchem_message("na")
      return(NA_integer_)
    }
    headers <- c("Content-Type" = "", "apikey" = apikey)
    if (from == "name") {
      body <- list(
        "name" = x, "orderBy" = cs_control(...)$order_by,
        "orderDirection" = cs_control(...)$order_direction
      )
      body <- jsonlite::toJSON(body, auto_unbox = TRUE)
    }
    if (from == "formula") {
      body <- jsonlite::toJSON(list(
        "formula" = unbox(x),
        "dataSources" = cs_control(...)$datasources,
        "orderBy" = unbox(cs_control(...)$order_by),
        "orderDirection" = unbox(cs_control(...)$order_direction)),
        auto_unbox = FALSE)
    }
    if (from == "inchi") {
      body <- jsonlite::toJSON(list("inchi" = x), auto_unbox = TRUE)
    }
    if (from == "inchikey") {
      body <- jsonlite::toJSON(list("inchikey" = x), auto_unbox = TRUE)
    }
    if (from == "smiles") {
      body <- jsonlite::toJSON(list("smiles" = x), auto_unbox = TRUE)
    }
    if (verbose) webchem_message("query", x, appendLF = FALSE)
    qurl <- paste0("https://api.rsc.org/compounds/v1/filter/", from)
    postres <- try(httr::RETRY("POST",
                               url = qurl,
                               httr::add_headers(.headers = headers),
                               body = body,
                               terminate_on = 404,
                               quiet = TRUE), silent = TRUE)
    if (inherits(postres, "try-error")) {
      if (verbose) webchem_message("service_down")
      return(NA_integer_)
    }
    if (postres$status_code == 200) {
      query_id <- jsonlite::fromJSON(rawToChar(postres$content))$queryId
      qurl <- paste0("https://api.rsc.org/compounds/v1/filter/",
                     query_id,
                     "/status")
      getstatus <- try(httr::RETRY("GET",
                                   url = qurl,
                                   httr::add_headers(.headers = headers),
                                   terminate_on = 404,
                                   quiet = TRUE), silent = TRUE)
      if (inherits(getstatus, "try-error")) {
        if (verbose) webchem_message("service_down")
        return(NA_integer_)
      }
      if (getstatus$status_code == 200) {
        qurl <- paste0("https://api.rsc.org/compounds/v1/filter/",
                       query_id,
                       "/results")
        getres <- try(httr::RETRY("GET",
                                  url = qurl,
                                  httr::add_headers(.headers = headers),
                                  terminate_on = 404,
                                  quiet = TRUE), silent = TRUE)
        if (inherits(getres, "try-error")) {
          if (verbose) webchem_message("service_down")
          return(NA_integer_)
        }
        if (getres$status_code == 200) {
          if (verbose) message(httr::message_for_status(getres))
          res <- jsonlite::fromJSON(rawToChar(getres$content))$results
          if (length(res) > 1) {
            res <- matcher(res,
                           query = x,
                           match = match,
                           from = from,
                           verbose = verbose)
          }
          if (length(res) == 0) {
            if (verbose) webchem_message("not_found")
            res <- NA_integer_
          }
          return(res)
        }
        else {
          if (verbose) message(httr::message_for_status(getres))
          return(NA_integer_)
        }
      }
      else {
        if (verbose) message(httr::message_for_status(getstatus))
        return(NA_integer_)
      }
    }
    else {
      if (verbose) message(httr::message_for_status(postres))
      return(NA_integer_)
    }
  }
  out <-
    lapply(
      query,
      foo,
      from = from,
      match = match,
      verbose = verbose,
      apikey = apikey,
      ...
    )
  names(out) <- query
  out <-
    lapply(out, tibble::enframe, name = NULL, value = "csid") %>%
    bind_rows(.id = "query")
  return(out)
}

#' Convert identifiers using ChemSpider
#'
#' Submit one or more identifiers (CSID, SMILES, InChI, InChIKey or Mol) and
#' return one or more identifiers in another format (CSID, SMILES, InChI,
#' InChIKey or Mol).
#' @param query character; query ID.
#' @param from character; type of query ID.
#' @param to character; type to convert to.
#' @param verbose logical; should a verbose output be printed on the console?
#' @param apikey character; your API key. If NULL (default),
#' \code{cs_check_key()} will look for it in .Renviron or .Rprofile.
#' @details Not all conversions are supported. Allowed conversions:
#' \itemize{
#' \item CSID <-> InChI
#' \item CSID <-> InChIKey
#' \item CSID <-> SMILES
#' \item CSID -> Mol file
#' \item InChI <-> InChIKey
#' \item InChI <-> SMILES
#' \item InChI -> Mol file
#' \item InChIKey <-> Mol file
#' }
#' @return Returns a vector containing the converted identifier(s).
#' @note An API key is needed. Register at \url{https://developer.rsc.org/}
#' for an API key. Please respect the Terms & Conditions. The Terms & Conditions
#' can be found at \url{https://developer.rsc.org/terms}.
#' @references \url{https://developer.rsc.org/docs/compounds-v1-trial/1/overview}
#' @references Eduard Szöcs, Tamás Stirling, Eric R. Scott, Andreas Scharmüller,
#' Ralf B. Schäfer (2020). webchem: An R Package to Retrieve Chemical
#' Information from the Web. Journal of Statistical Software, 93(13).
#' \doi{10.18637/jss.v093.i13}.
#' @export
#' @examples
#' \dontrun{
#' cs_convert("BQJCRHHNABKAKU-KBQPJGBKSA-N",
#'   from = "inchikey", to = "csid"
#' )
#' cs_convert("BQJCRHHNABKAKU-KBQPJGBKSA-N",
#'   from = "inchikey", to = "inchi"
#' )
#' cs_convert("BQJCRHHNABKAKU-KBQPJGBKSA-N",
#'   from = "inchikey", to = "mol"
#' )
#' cs_convert(160, from = "csid", to = "smiles")
#' }
cs_convert <- function(query, from, to, verbose = getOption("verbose"),
                       apikey = NULL) {
  if (is.null(apikey)) {
    apikey <- cs_check_key()
  }
  valid <- c("csid", "inchikey", "inchi", "smiles", "mol")
  from <- match.arg(from, choices = valid)
  to <- match.arg(to, choices = valid)
  if (!ping_service("cs")) stop(webchem_message("service_down"))
  cs_compinfo_dict <- data.frame(
    "name" = c("inchi", "inchikey", "smiles", "mol"),
    "cs_compinfo" = c("InChI", "InChIKey", "SMILES", "Mol2D"),
    stringsAsFactors = FALSE
  )
  to2 <- cs_compinfo_dict[which(cs_compinfo_dict$name == to), 2]
  if (from == to) {
    return(query)
  }
  if (from == "csid") {
    out <- cs_compinfo(query, fields = to2, apikey = apikey)
    if (ncol(out) == 2) {
      out <- out[, 2]
    }
    else {
      out <- out[, 1]
    }
    return(out)
  }
  if (to == "csid") {
    if (from == "mol") {
      if (verbose) stop("Conversion not supported. Returning NA.")
      return(NA_integer_)
    }
    else {
      out <- get_csid(query, from = from, apikey = apikey)$csid
      return(out)
    }
  }
  if (from == "inchikey" & to == "smiles") {
    if (verbose) stop("Conversion not supported. Returning NA.")
    return(NA)
  }
  if (from == "smiles" & to %in% c("inchikey", "mol")) {
    if (verbose) stop("Conversion not supported. Returning NA.")
    return(NA)
  }
  if (from == "mol" & to %in% c("inchi", "smiles")) {
    if (verbose) stop("Conversion not supported. Returning NA.")
    return(NA)
  }
  foo <- function(x, from, to, verbose, apikey) {
    if (is.na(x)) {
      if (verbose) webchem_message("na")
      return(NA)
    }
    headers <- c(`Content-Type` = "", `apikey` = apikey)
    body <- list(
      "input" = x, "inputFormat" = from,
      "outputFormat" = to
    )
    if (verbose) webchem_message("query", x, appendLF = FALSE)
    body <- jsonlite::toJSON(body, auto_unbox = TRUE)
    qurl <- "https://api.rsc.org/compounds/v1/tools/convert"
    postres <- try(httr::RETRY("POST",
                               url = qurl,
                               httr::add_headers(.headers = headers),
                               body = body,
                               terminate_on = 404,
                               quiet = TRUE), silent = TRUE)
    if (inherits(postres, "try-error")) {
      if (verbose) webchem_message("service_down")
      return(NA)
    }
    if (postres$status_code == 200) {
      if (verbose) message(httr::message_for_status(postres))
      out <- jsonlite::fromJSON(rawToChar(postres$content))$output
      return(out)
    }
    else {
      if (verbose) message(httr::message_for_status(postres))
      return(NA)
    }
  }
  out <- unname(sapply(query,
                       foo,
                       from = from,
                       to = to,
                       verbose = verbose,
                       apikey = apikey))
  return(out)
}

#' Retrieve record details by ChemSpider ID
#'
#' Submit a ChemSpider ID (CSID) and the fields you are interested in, and
#' retrieve the record details for your query.
#' @param csid numeric; can be obtained using \code{\link{get_csid}}
#' @param fields character; see details.
#' @param verbose logical; should a verbose output be printed on the console?
#' @param apikey character; your API key. If NULL (default),
#' \code{cs_check_key()} will look for it in .Renviron or .Rprofile.
#' @details Valid values for \code{fields} are \code{"SMILES"},
#' \code{"Formula"}, \code{"InChI"}, \code{"InChIKey"}, \code{"StdInChI"},
#' \code{"StdInChIKey"}, \code{"AverageMass"}, \code{"MolecularWeight"},
#' \code{"MonoisotopicMass"}, \code{"NominalMass"}, \code{"CommonName"},
#' \code{"ReferenceCount"}, \code{"DataSourceCount"}, \code{"PubMedCount"},
#' \code{"RSCCount"}, \code{"Mol2D"}, \code{"Mol3D"}. You can specify any
#' number of fields.
#' @return Returns a data frame.
#' @note An API key is needed. Register at \url{https://developer.rsc.org/}
#' for an API key. Please respect the Terms & Conditions. The Terms & Conditions
#' can be found at \url{https://developer.rsc.org/terms}.
#' @references \url{https://developer.rsc.org/docs/compounds-v1-trial/1/overview}
#' @export
#' @examples
#' \dontrun{
#' cs_compinfo(171, c("SMILES", "CommonName"))
#' cs_compinfo(171:182, "SMILES")
#' }
cs_compinfo <- function(csid, fields, verbose = getOption("verbose"),
                        apikey = NULL) {
  if (mean(is.na(csid)) == 1) {
    if (verbose) webchem_message("na")
    return(NA)
  }
  if (is.null(apikey)) {
    apikey <- cs_check_key()
  }
  fields <- match.arg(fields,
    choices = c(
      "SMILES", "Formula", "InChI", "InChIKey",
      "StdInChI", "StdInChIKey", "AverageMass",
      "MolecularWeight", "MonoisotopicMass",
      "NominalMass", "CommonName", "ReferenceCount",
      "DataSourceCount", "PubMedCount", "RSCCount",
      "Mol2D", "Mol3D"
    ),
    several.ok = TRUE
  )
  headers <- c("Content-Type" = "", "apikey" = apikey)
  body <- list(
    "recordIds" = csid[!is.na(csid)], "fields" = fields
  )
  body <- jsonlite::toJSON(body)
  if (verbose) webchem_message("query_all", appendLF = FALSE)
  qurl <- "https://api.rsc.org/compounds/v1/records/batch"
  postres <- try(httr::RETRY("POST",
                             url = qurl,
                             httr::add_headers(.headers = headers),
                             body = body,
                             terminate_on = 404,
                             quiet = TRUE), silent = TRUE)
  if (inherits(postres, "try-error")) {
    if (verbose) webchem_message("service_down")
    return(NA)
  }
  if (postres$status_code == 200) {
    res <- jsonlite::fromJSON(rawToChar(postres$content))$records
    if (length(res) == 0) {
      if (verbose) webchem_message("not_found")
      return(NA)
    }
    if (verbose) message(httr::message_for_status(postres))
    out <- data.frame(id = csid)
    out <- dplyr::left_join(out, res, by = "id")
    return(out)
  }
  else {
    if (verbose) message(httr::message_for_status(postres))
    return(NA)
  }
}

#' Get extended record details by ChemSpider ID
#'
#' Get extended info from ChemSpider, see \url{https://www.chemspider.com/}
#' @import xml2
#' @param csid character,  ChemSpider ID.
#' @param token character; security token.
#' @param verbose logical; should a verbose output be printed on the console?
#' @param ... currently not used.
#' @return a data.frame with entries: 'csid', 'mf' (molecular formula),
#' 'smiles', 'inchi' (non-standard), 'inchikey' (non-standard), 'average_mass',
#' 'mw' (Molecular weight), 'monoiso_mass' (MonoisotopicMass), nominal_mass',
#' 'alogp', 'xlogp', 'common_name' and 'source_url'
#' @note A security token is needed. Please register at RSC
#' \url{https://www.rsc.org/rsc-id/register}
#' for a security token.
#' Please respect the Terms & conditions
#' \url{https://www.rsc.org/help-legal/legal/terms-conditions/}.
#' @seealso \code{\link{get_csid}} to retrieve ChemSpider IDs,
#' \code{\link{cs_compinfo}} for extended compound information.
#' @note use \code{\link{cs_compinfo}} to retrieve standard inchikey.
#' @export
#' @examples
#' \dontrun{
#' token <- "<redacted>"
#' csid <- get_csid("Triclosan")
#' cs_extcompinfo(csid, token)
#'
#' csids <- get_csid(c('Aspirin', 'Triclosan'))
#' cs_compinfo(csids)
#' }
cs_extcompinfo <- function(csid, token, verbose = getOption("verbose"), ...) {
  .Deprecated("cs_compinfo()", old = "cs_extcompinfo()",
              msg = "'cs_extcompinfo' is deprecated.
use 'cs_commpinfo()' instead.")
  if (!ping_service("cs")) stop(webchem_message("service_down"))
  foo <- function(csid, token, verbose) {
    if (is.na(csid)) {
      out <- as.list(rep(NA, 13))
      names(out) <- c('csid', 'mf', 'smiles', 'inchi', 'inchikey',
                      'average_mass', 'mw', 'monoiso_mass', 'nominal_mass',
                      'alogp', 'xlogp', 'common_name', 'source_url')
      return(out)
    }
    baseurl <-
      'https://www.chemspider.com/MassSpecAPI.asmx/GetExtendedCompoundInfo?'
    qurl <- paste0(baseurl, 'CSID=', csid, '&token=', token)
    if (verbose)
      message(qurl)
    webchem_sleep(type = 'API')
    h <- try(read_xml(qurl), silent = TRUE)
    if (inherits(h, "try-error")) {
      warning('CSID not found... Returning NA.')
      return(NA)
    }
    out <- as.list(xml_text(xml_children(h)))
    names(out) <- c('csid', 'mf', 'smiles', 'inchi', 'inchikey', 'average_mass',
                    'mw', 'monoiso_mass', 'nominal_mass', 'alogp', 'xlogp',
                    'common_name')
    # convert to numeric
    out[['average_mass']] <- as.numeric(out[['average_mass']])
    out[['mw']] <- as.numeric(out[['mw']])
    out[['monoiso_mass']] <- as.numeric(out[['monoiso_mass']])
    out[['nominal_mass']] <- as.numeric(out[['nominal_mass']])
    out[['alogp']] <- as.numeric(out[['alogp']])
    out[['xlogp']] <- as.numeric(out[['xlogp']])
    source_url <- paste0('https://www.chemspider.com/Chemical-Structure.', csid,
                         '.html')
    out[['source_url']] <- source_url
    return(out)
  }
  out <- sapply(csid, foo, token = token, verbose = verbose)
  out <- data.frame(t(out), row.names = seq_len(ncol(out)))
  out[['query']] <- rownames(out)
  out <- data.frame(t(apply(out, 1, unlist)), stringsAsFactors = FALSE)
  class(out) <- c('cs_extcompinfo', 'data.frame')
  return(out)
}

#' Download images from ChemSpider
#'
#' @description Retrieve images of substances from ChemSpider and export them
#' in PNG format.
#' @param csid numeric; the ChemSpider ID (CSID) of the substance. This will
#' also be the name of the image file.
#' @param dir character; the download directory. \code{dir} accepts both
#' absolute and relative paths.
#' @param overwrite logical; should existing files in the directory with the
#' same name be overwritten?
#' @param apikey character; your API key. If NULL (default),
#' \code{cs_check_key()} will look for it in .Renviron or .Rprofile.
#' @param verbose logical; should a verbose output be printed on the console?
#' @note An API key is needed. Register at \url{https://developer.rsc.org/}
#' for an API key. Please respect the Terms & Conditions. The Terms & Conditions
#' can be found at \url{https://developer.rsc.org/terms}.
#' @references \url{https://developer.rsc.org/docs/compounds-v1-trial/1/overview}
#' @seealso \code{\link{get_csid}}, \code{\link{cs_check_key}}
#' @export
#' @examples
#' \dontrun{
#' cs_img(c(582, 682), dir = tempdir())
#' }
cs_img <- function(csid,
                   dir,
                   overwrite = TRUE,
                   apikey = NULL,
                   verbose = getOption("verbose")) {
  overwrite <- match.arg(as.character(overwrite), choices = c(TRUE, FALSE))
  if (is.null(apikey)) {
    apikey <- cs_check_key()
  }
  verbose <- match.arg(as.character(verbose), choices = c(TRUE, FALSE))
  if (!ping_service("cs")) stop(webchem_message("service_down"))
  foo <- function(csid, dir = dir, overwrite = overwrite, apikey = apikey,
                  verbose = verbose) {
    if (verbose) webchem_message("query", csid, appendLF = FALSE)
    if (is.na(csid)) {
      if (verbose) webchem_message("na")
      return(tibble(query = csid, path = NA))
    }
    path <- paste0(dir, "/", csid, ".png")
    url <- paste0("https://api.rsc.org/compounds/v1/records/", csid, "/image")
    headers <- c("Content-Type" = "", "apikey" = apikey)
    res <- try(httr::RETRY("GET",
                           url,
                           httr::add_headers(.headers = headers),
                           terminate_on = 404,
                           quiet = TRUE), silent = FALSE)
    if (inherits(res, "try-error")) {
      if (verbose) webchem_message("service_down")
    }
    else {
      if (verbose) message(httr::message_for_status(res))
      if (res$status_code == 200) {
        cont <- httr::content(res, type = "image", encoding = "base64")
        cont <- unlist(jsonlite::fromJSON(rawToChar(cont)))
        cont <- base64enc::base64decode(cont)
        if (overwrite) {
          writeBin(cont, path)
        }
        else {
          if (!file.exists(path)) writeBin(cont, path)
        }
      }
    }
  }
  out <- lapply(csid, function(x) foo(x, dir, overwrite, apikey, verbose))
}
