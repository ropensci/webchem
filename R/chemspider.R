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
#' @importFrom httr GET add_headers
#' @importFrom jsonlite fromJSON
#' @param apikey character; your API key. If NULL (default),
#' \code{cs_check_key()} will look for it in .Renviron or .Rprofile.
#' @return Returns a character vector.
#' @note An API key is needed. Register at \url{https://developer.rsc.org/}
#' for an API key. Please respect the Terms & Conditions. The Terms & Conditions
#' can be found at \url{https://developer.rsc.org/terms}.
#' @references \url{https://developer.rsc.org/compounds-v1/apis}
#' @export
#' @examples
#' \dontrun{
#' cs_datasources()
#' }
cs_datasources <- function(apikey = NULL) {
  if (is.null(apikey)) {
    apikey <- cs_check_key()
  }
  headers <- c("Content-Type" = "", "apikey" = apikey)
  res <- httr::GET(
    url = "https://api.rsc.org/compounds/v1/lookups/datasources",
    httr::add_headers(.headers = headers)
  )
  if (res$status_code == 200) {
    out <- unlist(unname(jsonlite::fromJSON(rawToChar(res$content))))
    return(out)
  }
  else {
    stop(httr::http_status(res)$message)
  }
}

#' Control ChemSpider API requests
#'
#' For some ChemSpider API requests, you can also specify various control
#' options. This function is used to set these control options.
#' @param datasources character; specifies the databases to query. Use
#' \code{cs_datasources()} to retrieve available ChemSpider data sources.
#' @param order_by character; specifies the sort order for the results.
#' Valid values are \code{"recordId"}, \code{"massDefect"},
#' \code{"molecularWeight"}, \code{"referenceCount"}, \code{"dataSourceCount"},
#' \code{"pubMedCount"}, \code{"rscCount"}.
#' @param order_direction character; specifies the sort order for the results.
#' Valid values are \code{"ascending"}, \code{"descending"}.
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
#' @references \url{https://developer.rsc.org/compounds-v1/apis}
#' @seealso \code{\link{get_csid}}
#' @export
#' @examples
#' cs_control()
#' cs_control(order_direction = "descending")
cs_control <- function(datasources = vector(),
                       order_by = "recordId", order_direction = "ascending",
                       include_all = FALSE, complexity = "any",
                       isotopic = "any") {
  order_by <- match.arg(order_by, choices = c(
    "recordId", "massDefect",
    "molecularWeight", "referenceCount",
    "dataSourceCount", "pubMedCount",
    "rscCount"
  ))
  order_direction <- match.arg(order_direction, choices = c("ascending",
                                                          "descending"))
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
#' @references \url{https://developer.rsc.org/compounds-v1/apis}
#' @references Eduard Szöcs, Tamás Stirling, Eric R. Scott, Andreas Scharmüller,
#' Ralf B. Schäfer (2020). webchem: An R Package to Retrieve Chemical
#' Information from the Web. Journal of Statistical Software, 93(13).
#' <doi:10.18637/jss.v093.i13>.
#' @importFrom httr POST add_headers http_status
#' @importFrom jsonlite toJSON
#' @importFrom tibble enframe
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
                     verbose = TRUE,
                     apikey = NULL,
                     ...) {
  if (is.null(apikey)) {
    apikey <- cs_check_key()
  }
  from <- match.arg(from)
  match <- match.arg(match)

  foo <- function(x, from, match, verbose, apikey, ...) {
    if (is.na(x)) return(NA_integer_)
    res <- switch(from,
                  name = cs_name_csid(x, apikey = apikey,
                                      control = cs_control(...)),
                  formula = cs_formula_csid(x, apikey = apikey,
                                            control = cs_control(...)),
                  inchi = cs_inchi_csid(x, apikey = apikey),
                  inchikey = cs_inchikey_csid(x, apikey = apikey),
                  smiles = cs_smiles_csid(x, apikey = apikey))
    if(length(res) > 1) {
      res <- matcher(res, query = x, match = match, from = from, verbose = verbose)
    }
    if (length(res) == 0) res <- NA_integer_
    return(res)
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
    lapply(out, enframe, name = NULL, value = "csid") %>%
    bind_rows(.id = "query")
  return(out)
}

#' Retrieve ChemSpider ID from a queryId returned by a non-batch POST request
#'
#' ChemSpider IDs (CSIDs) are obtained in multiple steps.
#' First we assemble a POST request.
#' These requests can take multiple forms and are assembled separately in each
#' function that returns a CSID.
#' A dedicated API handles the POST request and returns a queryId.
#' This queryId is passed to another, more general API, that returns the CSID.
#' This function communicates with the second API.
#' @importFrom httr GET add_headers http_status
#' @importFrom jsonlite fromJSON
#' @param postres an object of class \code{\link{response}} containing the
#' queryId returned by the previous API.
#' @param headers list; contains the API key.
#' @return Returns a numeric vector of ChemSpider IDs.
#' @note An API key is needed. Register at RSC.
#' \url{https://developer.rsc.org/}
#' for an API key.
#' Please respect the Terms & conditions \url{https://developer.rsc.org/terms}.
#' @references \url{https://developer.rsc.org/compounds-v1/apis}
#' @noRd
cs_query_csid <- function(postres, headers) {
  query_id <- jsonlite::fromJSON(rawToChar(postres$content))$queryId
  getstatus <- httr::GET(
    url = paste0(
      "https://api.rsc.org/compounds/v1/filter/",
      query_id, "/status"
    ),
    httr::add_headers(.headers = headers)
  )
  apistatus <- jsonlite::fromJSON(rawToChar(getstatus$content))$status
  while (apistatus == "Processing") {
    message("Your request is being processed.")
    Sys.sleep(5)
    getstatus <- httr::GET(
      url = paste0(
        "https://api.rsc.org/compounds/v1/filter/",
        query_id, "/status"
      ),
      httr::add_headers(.headers = headers)
    )
    apistatus <- jsonlite::fromJSON(rawToChar(getstatus$content))$status
  }
  if (apistatus == "Complete") {
    getres <- httr::GET(
      url = paste0(
        "https://api.rsc.org/compounds/v1/filter/",
        query_id, "/results"
      ),
      httr::add_headers(.headers = headers)
    )
    if (getres$status_code == 200) {
      out <- jsonlite::fromJSON(rawToChar(getres$content))
      return(out)
    }
    else {
      stop(httr::http_status(getres)$message)
    }
  }
  else {
    dict <- data.frame(
      "Status" = c("Suspended", "Failed", "Not Found"),
      "Message" = c(
        "The results could not be compiled within reasonable amount of time. ",
        "The backend system could not compile the results. ",
        "The Query ID has not been registered or has expired. "
      ),
      stringsAsFactors = FALSE
    )
    stop(paste0(dict[which(dict[, 1] %in% apistatus), 2]))
  }
}

#' Retrieve ChemSpider ID from compound name
#'
#' Query a single compund by name and return a vector of ChemSpider IDs.
#'
#' @importFrom httr POST add_headers http_status
#' @importFrom jsonlite toJSON
#' @param name character; search term.
#' @param apikey character; your API key. If NULL (default),
#' \code{cs_check_key()} will look for it in .Renviron or .Rprofile.
#' @param control list; see details.
#' @return Returns a numeric vector of ChemSpider IDs.
#' @details Control options available for this function are \code{order_by},
#' \code{order_direction}. See \code{cs_control()} for a full list of valid
#' values for these control options.
#' @note An API key is needed. Register at RSC \url{https://developer.rsc.org/}
#' for an API key. Please respect the Terms & conditions
#' \url{https://developer.rsc.org/terms}.
#' @references https://developer.rsc.org/compounds-v1/apis
#' @note This is a low level function and is not exported.
#' @examples
#' \dontrun{
#' cs_name_csid("ethanol")
#' }
#' @noRd
cs_name_csid <- function(name, apikey = NULL, control = cs_control()) {
  if (is.null(apikey)) {
    apikey <- cs_check_key()
  }
  headers <- c("Content-Type" = "", "apikey" = apikey)
  body <- list(
    "name" = name, "orderBy" = control$order_by,
    "orderDirection" = control$order_direction
  )
  body <- jsonlite::toJSON(body, auto_unbox = TRUE)
  postres <- httr::POST(
    url = "https://api.rsc.org/compounds/v1/filter/name",
    httr::add_headers(.headers = headers), body = body
  )
  if (postres$status_code == 200) {
    out <- cs_query_csid(postres = postres, headers = headers)$results
    return(out)
  }
  else {
    stop(httr::http_status(postres)$message)
  }
}

#' Retrieve ChemSpider ID from compound formula
#'
#' Query a single compund by formula and return a vector of ChemSpider IDs.
#'
#' @importFrom httr POST add_headers http_status
#' @importFrom jsonlite toJSON
#' @param formula character; search term.
#' @param apikey character; your API key. If NULL (default),
#' \code{cs_check_key()} will look for it in .Renviron or .Rprofile.
#' @param control list; see details.
#' @return Returns a numeric vector of ChemSpider IDs.
#' @details \code{formula} can be expressed with and without LaTeX syntax.
#' @details Control options available for this function are \code{datasources},
#' \code{order_by}, \code{order_direction}. See \code{cs_control()} for a full
#' list of valid values for these control options.
#' @note An API key is needed. Register at RSC \url{https://developer.rsc.org/}
#' for an API key. Please respect the Terms & conditions
#' \url{https://developer.rsc.org/terms}.
#' @references \url{https://developer.rsc.org/compounds-v1/apis}
#' @note This is a low level function and is not exported.
#' @examples
#' \dontrun{
#' cs_formula_csid("C2H6O")
#' cs_formula_csid("C_{2}H_{6}O")
#' }
#' @noRd
cs_formula_csid <- function(formula, apikey = NULL, control = cs_control()) {
  if (is.null(apikey)) {
    apikey <- cs_check_key()
  }
  headers <- c("Content-Type" = "", "apikey" = apikey)
  body <- jsonlite::toJSON(list(
    "formula" = unbox(formula),
    "dataSources" = control$datasources,
    "orderBy" = unbox(control$order_by),
    "orderDirection" = unbox(control$order_direction)), auto_unbox = FALSE)
  postres <- httr::POST(
    url = "https://api.rsc.org/compounds/v1/filter/formula",
    httr::add_headers(.headers = headers), body = body
  )
  if (postres$status_code == 200) {
    out <- cs_query_csid(postres = postres, headers = headers)$results
    return(out)
  }
  else {
    stop(httr::http_status(postres)$message)
  }
}

#' Retrieve ChemSpider ID from SMILES
#'
#' Query a single compund by SMILES and return a vector of ChemSpider IDs.
#'
#' @importFrom httr POST add_headers http_status
#' @importFrom jsonlite toJSON
#' @param smiles character; search term.
#' @param apikey character; your API key. If NULL (default),
#' \code{cs_check_key()} will look for it in .Renviron or .Rprofile.
#' @return Returns a numeric vector of ChemSpider IDs.
#' @note An API key is needed. Register at RSC \url{https://developer.rsc.org/}
#' for an API key. Please respect the Terms & conditions
#' \url{https://developer.rsc.org/terms}.
#' @references \url{https://developer.rsc.org/compounds-v1/apis}
#' @note This is a low level function and is not exported.
#' @examples
#' \dontrun{
#' cs_smiles_csid("CC(O)=O")
#' cs_smiles_csid(c("CC(O)=O","CC=O"))
#' }
#' @noRd
cs_smiles_csid <- function(smiles, apikey = NULL) {
  if (is.null(apikey)) {
    apikey <- cs_check_key()
  }
  headers <- c("Content-Type" = "", "apikey" = apikey)
  body <- jsonlite::toJSON(list("smiles" = smiles), auto_unbox = TRUE)
  postres <- httr::POST(
    url = "https://api.rsc.org/compounds/v1/filter/smiles",
    httr::add_headers(.headers = headers), body = body
  ) # filter-smiles-post
  if (postres$status_code == 200) {
    out <- cs_query_csid(postres = postres, headers = headers)$results
    return(out)
  }
  else {
    stop(httr::http_status(postres)$message)
  }
  return(out)
}

#' Retrieve ChemSpider ID from InChI
#'
#' Query a single compund by InChI and return a vector of ChemSpider IDs.
#'
#' @importFrom httr POST add_headers http_status
#' @importFrom jsonlite toJSON
#' @param inchi character; search term.
#' @param apikey character; your API key. If NULL (default),
#' \code{cs_check_key()} will look for it in .Renviron or .Rprofile.
#' @return Returns a numeric vector of ChemSpider IDs.
#' @note An API key is needed. Register at RSC \url{https://developer.rsc.org/}
#' for an API key. Please respect the Terms & conditions
#' \url{https://developer.rsc.org/terms}.
#' @references \url{https://developer.rsc.org/compounds-v1/apis}
#' @note This is a low level function and is not exported.
#' @examples
#' \dontrun{
#' cs_inchi_csid(inchi = "InChI=1S/C2H4O2/c1-2(3)4/h1H3,(H,3,4)")
#' cs_inchi_csid(inchi = c(
#' "InChI=1S/C2H4O2/c1-2(3)4/h1H3,(H,3,4)",
#' "InChI=1/C2H4O/c1-2-3/h2H,1H3"))
#' }
#' @noRd
cs_inchi_csid <- function(inchi, apikey = NULL) {
  if (is.null(apikey)) {
    apikey <- cs_check_key()
  }
  headers <- c("Content-Type" = "", "apikey" = apikey)
  body <- jsonlite::toJSON(list("inchi" = inchi), auto_unbox = TRUE)
  postres <- httr::POST(
    url = "https://api.rsc.org/compounds/v1/filter/inchi",
    httr::add_headers(.headers = headers), body = body
  )
  if (postres$status_code == 200) {
    out <- cs_query_csid(postres = postres, headers = headers)$results
    return(out)
  }
  else {
    stop(httr::http_status(postres)$message)
  }
  return(out)
}

#' Retrieve ChemSpider ID from InChIKey
#'
#' Query a single compund by InChI and return a vector of ChemSpider IDs.
#'
#' @importFrom httr POST add_headers http_status
#' @importFrom jsonlite toJSON
#' @param inchikey character; search term.
#' @param apikey character; your API key. If NULL (default),
#' \code{cs_check_key()} will look for it in .Renviron or .Rprofile.
#' @return Returns a numeric vector of ChemSpider IDs.
#' @note An API key is needed. Register at RSC \url{https://developer.rsc.org/}
#' for an API key. Please respect the Terms & conditions
#' \url{https://developer.rsc.org/terms}.
#' @references \url{https://developer.rsc.org/compounds-v1/apis}
#' @note This is a low level function and is not exported.
#' @examples
#' \dontrun{
#' cs_inchikey_csid("QTBSBXVTEAMEQO-UHFFFAOYAR")
#' cs_inchikey_csid(c("QTBSBXVTEAMEQO-UHFFFAOYAR", "IKHGUXGNUITLKF-UHFFFAOYAB"))
#' }
#' @noRd
cs_inchikey_csid <- function(inchikey, apikey = NULL) {
  if (is.null(apikey)) {
    apikey <- cs_check_key()
  }
  headers <- c("Content-Type" = "", "apikey" = apikey)
  body <- jsonlite::toJSON(list("inchikey" = inchikey), auto_unbox = TRUE)
  postres <- httr::POST(
    url = "https://api.rsc.org/compounds/v1/filter/inchikey",
    httr::add_headers(.headers = headers), body = body
  )
  if (postres$status_code == 200) {
    out <- cs_query_csid(postres = postres, headers = headers)$results
    return(out)
  }
  else {
    stop(httr::http_status(postres)$message)
  }
  return(out)
}

#' Convert between SMILES, InChI, InChiKey, Mol identifiers using ChemSpider
#'
#' Submit a single identifier (SMILES, InChI, InChIKey or Mol) and return an
#' identifier in another format (SMILES, InChI, InChIKey or Mol). Not all
#' conversions are supported.
#' @importFrom jsonlite fromJSON toJSON
#' @importFrom httr POST add_headers http_status
#' @param input character; the identifier to be converted.
#' @param from character; the format to be converted from. Valid values are
#' \code{"smiles"}, \code{"inchi"}, \code{"inchikey"}, \code{"mol"}.
#' @param to character; the format to be converted to. Valid values are
#' \code{"smiles"}, \code{"inchi"}, \code{"inchikey"}, \code{"mol"}.
#' @param apikey character; your API key. If NULL (default),
#' \code{cs_check_key()} will look for it in .Renviron or .Rprofile.
#' @details Not all conversions are supported. Allowed conversions:
#' \itemize{
#' \item InChI <-> InChIKey
#' \item InChI <-> SMILES
#' \item InChI -> Mol file
#' \item InChIKey <-> Mol file
#' }
#' @return Returns a character vector of length one containing the converted
#' identifier.
#' @note An API key is needed. Register at \url{https://developer.rsc.org/}
#' for an API key. Please respect the Terms & Conditions. The Terms & Conditions
#' can be found at \url{https://developer.rsc.org/terms}.
#' @references \url{https://developer.rsc.org/compounds-v1/apis}
#' @seealso This is a low level function and is not exported. See
#' \code{\link{cs_convert}} for the top level function.
#' @seealso \code{\link{parse_mol}}
#' @examples
#' \dontrun{
#' cs_convert_multiple("CC(=O)O", "smiles", "inchi")
#' cs_convert_multiple("InChI=1S/C2H4O2/c1-2(3)4/h1H3,(H,3,4)", "inchi",
#' "inchikey")
#' cs_convert_multiple("QTBSBXVTEAMEQO-UHFFFAOYSA-N", "inchikey", "mol")
#' cs_convert_multiple("QTBSBXVTEAMEQO-UHFFFAOYSA-N", "inchikey", "mol",
#' parse = TRUE)
#' }
#' @noRd
cs_convert_multiple <- function(input, from, to, apikey = NULL) {
  if (is.null(apikey)) {
    apikey <- cs_check_key()
  }
  headers <- c(`Content-Type` = "", `apikey` = apikey)
  body <- list(
    "input" = input, "inputFormat" = from,
    "outputFormat" = to
  )
  body <- jsonlite::toJSON(body, auto_unbox = TRUE)
  postres <- httr::POST(
    url = "https://api.rsc.org/compounds/v1/tools/convert",
    httr::add_headers(.headers = headers), body = body
  )
  if (postres$status_code == 200) {
    out <- jsonlite::fromJSON(rawToChar(postres$content))$output
    return(out)
  }
  else {
    stop(httr::http_status(postres)$message)
  }
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
#' @references \url{https://developer.rsc.org/compounds-v1/apis}
#' @references Eduard Szöcs, Tamás Stirling, Eric R. Scott, Andreas Scharmüller,
#' Ralf B. Schäfer (2020). webchem: An R Package to Retrieve Chemical
#' Information from the Web. Journal of Statistical Software, 93(13).
#' <doi:10.18637/jss.v093.i13>.
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
cs_convert <- function(query, from, to, apikey = NULL) {
  if (is.null(apikey)) {
    apikey <- cs_check_key()
  }
  valid <- c("csid", "inchikey", "inchi", "smiles", "mol")
  from <- match.arg(from, choices = valid)
  to <- match.arg(to, choices = valid)
  cs_compinfo_dict <- data.frame(
    "name" = c("inchi", "inchikey", "smiles", "mol"),
    "cs_compinfo" = c("InChI", "InChIKey", "SMILES", "Mol2D"),
    stringsAsFactors = FALSE
  )
  to2 <- cs_compinfo_dict[which(cs_compinfo_dict$name == to), 2]
  cs_convert_router <- function(from, to) {
    if (from == to) {
      return("identity")
    }
    if (from == "csid") {
      return("cs_compinfo")
    }
    if (to == "csid") {
      return(paste("cs", from, to, sep = "_"))
    }
    if (from != "csid" & to != "csid") {
      return("cs_convert_multiple")
    }
  }
  if (from == "csid") {
    out <- cs_compinfo(query, fields = to2, apikey = apikey)
    if (ncol(out) == 2) {
      out <- out[, 2]
    }
    else {
      out <- out[, 1]
    }
  }
  else {
    out <- unname(sapply(query, function(x) {
      if (is.na(x)) return(NA)
      switch(cs_convert_router(from, to),
             identity = query,
             cs_convert_multiple = cs_convert_multiple(
               x, from = from, to = to, apikey = apikey
             ),
             cs_inchikey_csid = cs_inchikey_csid(x, apikey = apikey),
             cs_inchi_csid = cs_inchi_csid(x, apikey = apikey),
             cs_smiles_csid = cs_smiles_csid(x, apikey = apikey),
             cs_mol_csid = stop("Conversion not supported."))
      }))
    }
  return(out)
}

#' Retrieve record details by ChemSpider ID
#'
#' Submit a ChemSpider ID (CSID) and the fields you are interested in, and
#' retrieve the record details for your query.
#' @importFrom jsonlite toJSON
#' @importFrom httr POST add_headers
#' @importFrom dplyr left_join
#' @param csid numeric; can be obtained using \code{\link{get_csid}}
#' @param fields character; see details.
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
#' @references \url{https://developer.rsc.org/compounds-v1/apis}
#' @export
#' @examples
#' \dontrun{
#' cs_compinfo(171, c("SMILES", "CommonName"))
#' cs_compinfo(171:182, "SMILES")
#' }
cs_compinfo <- function(csid, fields, apikey = NULL) {
  if (mean(is.na(csid)) == 1) return(data.frame(id = NA))
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
  postres <- httr::POST(
    url = "https://api.rsc.org/compounds/v1/records/batch",
    httr::add_headers(.headers = headers), body = body
  )
  if (postres$status_code == 200) {
    res <- jsonlite::fromJSON(rawToChar(postres$content))$records
    if (length(res) == 0) return(data.frame(id = NA))
    out <- data.frame(id = csid)
    out <- dplyr::left_join(out, res, by = "id")
    return(out)
  }
  else {
    stop(httr::http_status(postres)$message)
  }
}

#' Get extended record details by ChemSpider ID
#'
#' Get extended info from ChemSpider, see \url{https://www.chemspider.com/}
#' @import xml2
#' @importFrom stats rgamma
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
cs_extcompinfo <- function(csid, token, verbose = TRUE, ...) {
  .Deprecated("cs_compinfo()", old = "cs_extcompinfo()",
              msg = "'cs_extcompinfo' is deprecated.
use 'cs_commpinfo()' instead.")
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
    Sys.sleep(rgamma(1, shape = 15, scale = 1/45))
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
#' @references \url{https://developer.rsc.org/compounds-v1/apis}
#' @seealso \code{\link{get_csid}}, \code{\link{cs_check_key}}
#' @importFrom httr GET add_headers message_for_status content
#' @importFrom jsonlite fromJSON
#' @importFrom base64enc base64decode
#' @export
#' @examples
#' \dontrun{
#' cs_img(c(582, 682), dir = tempdir())
#' }
cs_img <- function(csid,
                   dir,
                   overwrite = TRUE,
                   apikey = NULL,
                   verbose = TRUE) {
  overwrite <- match.arg(as.character(overwrite), choices = c(TRUE, FALSE))
  if (is.null(apikey)) {
    apikey <- cs_check_key()
  }
  verbose <- match.arg(as.character(verbose), choices = c(TRUE, FALSE))
  foo <- function(csid, dir = dir, overwrite = overwrite, apikey = apikey,
                  verbose = verbose) {
    if (verbose) message("Searching ", csid, ". ", appendLF = FALSE)
    if (is.na(csid)) {
      if (verbose) {
        message("Invalid input.")
      }
      return(tibble(query = csid, path = NA))
    }
    else {
      path <- paste0(dir, "/", csid, ".png")
      url <- paste0("https://api.rsc.org/compounds/v1/records/", csid, "/image")
      headers <- c("Content-Type" = "", "apikey" = apikey)
      res <- httr::GET(url, httr::add_headers(.headers = headers))
      if (verbose) message(httr::message_for_status(res))
      if (res$status_code < 300) {
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
