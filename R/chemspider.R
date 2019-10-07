#' Retrieve ChemSpider data sources
#'
#' The function returns a vector of available data sources used by ChemSpider.
#' Some ChemSpider functions allow you to restrict which sources are used to
#' lookup the requested query. Restricting the sources makes these queries faster.
#' @importFrom httr GET add_headers
#' @importFrom jsonlite fromJSON
#' @param apikey character; your API key.
#' @return Returns a character vector.
#' @note An API key is needed. Register at \url{https://developer.rsc.org/}
#' for an API key. Please respect the Terms & Conditions. The Terms & Conditions
#' can be found at \url{https://developer.rsc.org/terms}.
#' @references https://developer.rsc.org/compounds-v1/apis
#' @author Tamas Stirling, \email{stirling.tamas@@gmail.com}
#' @export
#' @examples
#' \dontrun{
#' apikey <- "<YOUR-API-KEY>"
#' cs_datasources(apikey = apikey)
#' }
cs_datasources <- function(apikey) {
  headers <- c(`Content-Type` = "", `apikey` = apikey)
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
#' For some ChemSpider API requests, you can also specify various control options.
#' This function is used to set these control options.
#' @param orderBy character; specifies the sort order for the results.
#' Valid values are \code{"recordId"}, \code{"massDefect"}, \code{"molecularWeight"},
#' \code{"referenceCount"}, \code{"dataSourceCount"}, \code{"pubMedCount"},
#' \code{"rscCount"}.
#' @param orderDirection character; specifies the sort order for the results.
#' Valid values are \code{"ascending"}, \code{"descending"}.
#' @param includeAll logical; see details.
#' @param complexity character; see details.
#' Valid values are \code{"any"} \code{"single"}, \code{"multiple"}.
#' @param isotopic character; see details.
#' Valid values are \code{"any"}, \code{"labeled"}, \code{"unlabeled"}.
#' @details Setting \code{includeAll} to \code{TRUE} will consider records which
#' contain all of the filter criteria specified in the request.
#' Setting it to \code{FALSE} will consider records which contain any of the
#' filter criteria.
#' @details A compound with a  \code{complexity} of \code{"multiple"} has more than one
#' disconnected system in it or a metal atom or ion.
#' @return Returns a list of specified control options.
#' @note This is a full list of all API control options.
#' However, not all of these options are used in all functions.
#' Each API uses a subset of these controls.
#' The controls that are available for a given function are indicated within the
#' documentation of the function.
#' @references https://developer.rsc.org/compounds-v1/apis
#' @author Tamas Stirling, \email{stirling.tamas@@gmail.com}
#' @seealso \code{\link{cs_name_csid}}, \code{\link{cs_element_csid}}
#' @export
#' @examples
#' cs_control()
#' cs_control(orderDirection = "descending")
cs_control <- function(orderBy = "recordId", orderDirection = "ascending",
                        includeAll = FALSE, complexity = "any", isotopic = "any"){
  orderBy <- match.arg(orderBy, choices = c("recordId", "massDefect",
                                            "molecularWeight","referenceCount",
                                            "dataSourceCount", "pubMedCount",
                                            "rscCount"))
  orderDirection <- match.arg(orderDirection, choices = c("ascending", "descending"))
  includeAll <- match.arg(as.character(includeAll), choices = c(TRUE, FALSE))
  complexity <- match.arg(complexity, choices = c("any","single","multiple"))
  isotopic <- match.arg(isotopic, choices = c("any","labeled","unlabeled"))
  return(list("orderBy" = orderBy, "orderDirection" = orderDirection,
              "includeAll" = includeAll, "complexity" = complexity,
              "isotopic" = isotopic))
}

#' ChemSpider ID from compound name
#'
#' Submit the name of a compund and retrieve the ChemSpider ID (CSID) of the
#' compound.
#' @importFrom httr POST add_headers http_status
#' @importFrom jsonlite toJSON
#' @param query character; search term.
#' @param apikey character; your API key.
#' @param control function; see details.
#' @details Control options available for this function are \code{orderBy},
#' \code{orderDirection}. See \code{cs_control()} for a full list of valid values
#' for these control options.
#' @return Returns a list of two elements.
#' @note An API key is needed. Register at RSC.
#' \url{https://developer.rsc.org/}
#' for an API key.
#' Please respect the Terms & conditions \url{https://developer.rsc.org/terms}.
#' @references https://developer.rsc.org/compounds-v1/apis
#' @author Eduard Szoecs, \email{eduardszoecs@@gmail.com}
#' @author Tamas Stirling, \email{stirling.tamas@@gmail.com}
#' @export
#' @examples
#' \dontrun{
#' apikey <- "<YOUR-API-KEY>"
#' cs_name_csid("Triclosan", apikey = apikey)
#' }
cs_name_csid <- function(query, apikey, control = cs_control()) {
  headers <- c("Content-Type" = "", "apikey" = apikey)
  body <- list(
    "name" = query, "orderBy" = control$orderBy,
    "orderDirection" = control$orderDirection
  )
  body <- jsonlite::toJSON(body, auto_unbox = TRUE)
  postres <- httr::POST(
    url = "https://api.rsc.org/compounds/v1/filter/name",
    httr::add_headers(.headers = headers), body = body
  )
  if (postres$status_code == 200) {
    out <- get_csid(postres = postres, headers = headers)
    return(out)
  }
  else {
    stop(httr::http_status(postres)$message)
  }
}

#' ChemSpider ID from element specifications
#'
#' Specify lists of elements to include and exclude from the query and retrieve
#' ChemSpider IDs (CSIDs) that match the given specifications.
#' @importFrom httr add_headers POST http_status
#' @importFrom jsonlite toJSON unbox
#' @param includeElements character; a vector of up to 15 elements to include
#'  in the query.
#' @param excludeElements character; a vector of up to 100 elements to exclude
#'  from the query.
#' @param apikey character; your API key.
#' @param control function; see details.
#' @details See \code{cs_control()} for a full list of control options.
#' @details Control options available for this function are \code{orderBy},
#' \code{orderDirection}, \code{includeAll}, \code{complexity}, \code{isotopic}.
#' @return Returns a list of two elements.
#' @note An API key is needed. Register at \url{https://developer.rsc.org/}
#' for an API key. Please respect the Terms & Conditions. The Terms & Conditions
#' can be found at \url{https://developer.rsc.org/terms}.
#' @references \url{https://developer.rsc.org/compounds-v1/apis}
#' @author Tamas Stirling, \email{stirling.tamas@@gmail.com}
#' @export
cs_element_csid <- function(includeElements, excludeElements, apikey,
                            control = cs_control()){
  headers <- c("Content-Type" = "", "apikey" = apikey)
  body <- jsonlite::toJSON(list(
    "includeElements" = includeElements,
    "excludeElements" = excludeElements,
    "options" = list(
      "includeAll" = jsonlite::unbox(control$includeAll),
      "complexity" = jsonlite::unbox(control$complexity),
      "isotopic" = jsonlite::unbox(control$isotopic)
    ),
    "orderBy" = jsonlite::unbox(control$orderBy),
    "orderDirection" = jsonlite::unbox(control$orderDirection)
  ))
  postres <- httr::POST(
    url = "https://api.rsc.org/compounds/v1/filter/element",
    httr::add_headers(.headers = headers), body = body
  )
  if (postres$status_code == 200) {
    out <- get_csid(postres = postres, headers = headers)
    return(out)
  }
  else {
    stop(httr::http_status(postres)$message)
  }
}

#' Retrieve ChemSpider ID from a queryId returned by a non-batch POST request
#'
#' Chemspider IDs (CSIDs) are obtained in multiple steps.
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
#' @return Returns a list of two elements.
#' @note An API key is needed. Register at RSC.
#' \url{https://developer.rsc.org/}
#' for an API key.
#' Please respect the Terms & conditions \url{https://developer.rsc.org/terms}.
#' @references \url{https://developer.rsc.org/compounds-v1/apis}
#' @author Tamas Stirling, \email{stirling.tamas@@gmail.com}
get_csid <- function(postres, headers){
  queryId <- jsonlite::fromJSON(rawToChar(postres$content))$queryId
  getstatus <- httr::GET(
    url = paste0(
      "https://api.rsc.org/compounds/v1/filter/",
      queryId, "/status"
    ),
    httr::add_headers(.headers = headers)
  )
  apistatus <- jsonlite::fromJSON(rawToChar(getstatus$content))$status
  while(apistatus == "Processing"){
    message("Your request is being processed.")
    Sys.sleep(5)
    getstatus <- httr::GET(
      url = paste0(
        "https://api.rsc.org/compounds/v1/filter/",
        queryId, "/status"
      ),
      httr::add_headers(.headers = headers)
    )
    apistatus <- jsonlite::fromJSON(rawToChar(getstatus$content))$status
  }
  if(apistatus == "Complete"){
    getres <- httr::GET(
      url = paste0(
        "https://api.rsc.org/compounds/v1/filter/",
        queryId, "/results"
      ),
      httr::add_headers(.headers = headers)
    )
    if(getres$status_code == 200){
      out <- jsonlite::fromJSON(rawToChar(getres$content))
      return(out)
    }
    else {
      stop(httr::http_status(getres)$message)
      }
  }
  else{
    dict <- data.frame(
      "Status" = c("Suspended", "Failed", "Not Found"),
      "Message" = c(
        "The results could not be compiled within a reasonable amount of time. ",
        "The backend system could not compile the results. ",
        "The Query ID has not been registered or has expired. "),
      stringsAsFactors = FALSE
    )
    stop(paste0(dict[which(dict[,1] %in% apistatus),2]))
  }
}

#' Retrieve ChemSpider ID from SMILES
#'
#' @importFrom httr POST add_headers http_status
#' @importFrom jsonlite toJSON
#' @param smiles character; search term.
#' @param apikey character; your API key.
#' @return Returns a list of two elements.
#' @note An API key is needed. Register at RSC
#' \url{https://developer.rsc.org/}
#' for an API key.
#' Please respect the Terms & conditions \url{https://developer.rsc.org/terms}.
#' @references https://developer.rsc.org/compounds-v1/apis
#' @author Tamas Stirling, \email{stirling.tamas@@gmail.com}
#' @seealso This is a low level function. See \code{\link{cs_convert}}
#' for the top level function.
#' @examples
#' \dontrun{
#' apikey <- "<YOUR-API-KEY>"
#' cs_smiles_csid("CC(O)=O", apikey = apikey)
#' }
cs_smiles_csid <- function(smiles, apikey){
  headers <- c("Content-Type" = "", "apikey" = apikey)
  body <- jsonlite::toJSON(list("smiles" = smiles), auto_unbox = TRUE)
  postres <- httr::POST(
    url = "https://api.rsc.org/compounds/v1/filter/smiles",
    httr::add_headers(.headers = headers), body = body
  )#filter-smiles-post
  if (postres$status_code == 200) {
    out <- get_csid(postres = postres, headers = headers)
    return(out)
  }
  else {
    stop(httr::http_status(postres)$message)
  }
}

#' Retrieve ChemSpider ID from InChI
#'
#' @importFrom httr POST add_headers http_status
#' @importFrom jsonlite toJSON
#' @param inchi character; search term.
#' @param apikey character; your API key.
#' @return Returns a list of two elements.
#' @note An API key is needed. Register at RSC
#' \url{https://developer.rsc.org/}
#' for an API key.
#' Please respect the Terms & conditions \url{https://developer.rsc.org/terms}.
#' @references https://developer.rsc.org/compounds-v1/apis
#' @author Tamas Stirling, \email{stirling.tamas@@gmail.com}
#' @seealso This is a low level function. See \code{\link{cs_convert}}
#' for the top level function.
#' @examples
#' \dontrun{
#' apikey <- "<YOUR-API-KEY>"
#' cs_inchi_csid(inchi = "InChI=1S/C2H4O2/c1-2(3)4/h1H3,(H,3,4)", apikey = apikey)
#' }
cs_inchi_csid <- function(inchi, apikey){
  headers <- c("Content-Type" = "", "apikey" = apikey)
  body <- jsonlite::toJSON(list("inchi" = inchi), auto_unbox = TRUE)
  postres <- httr::POST(
    url = "https://api.rsc.org/compounds/v1/filter/inchi",
    httr::add_headers(.headers = headers), body = body
  )
  if (postres$status_code == 200) {
    out <- get_csid(postres = postres, headers = headers)
    return(out)
  }
  else {
    stop(httr::http_status(postres)$message)
  }
}

#' Retrieve ChemSpider ID from InChIKey
#'
#' @importFrom httr POST add_headers http_status
#' @importFrom jsonlite toJSON
#' @param inchikey character; search term.
#' @param apikey character; your API key.
#' @return Returns a list of two elements.
#' @note An API key is needed. Register at RSC
#' \url{https://developer.rsc.org/}
#' for an API key.
#' Please respect the Terms & conditions \url{https://developer.rsc.org/terms}.
#' @references https://developer.rsc.org/compounds-v1/apis
#' @author Tamas Stirling, \email{stirling.tamas@@gmail.com}
#' @seealso This is a low level function. See \code{\link{cs_convert}}
#' for the top level function.
#' @examples
#' \dontrun{
#' apikey <- "<YOUR-API-KEY>"
#' cs_inchikey_csid("QTBSBXVTEAMEQO-UHFFFAOYSA-N", apikey = apikey)
#' }
cs_inchikey_csid <- function(inchikey, apikey){
  headers <- c("Content-Type" = "", "apikey" = apikey)
  body <- jsonlite::toJSON(list("inchikey" = inchikey), auto_unbox = TRUE)
  postres <- httr::POST(
    url = "https://api.rsc.org/compounds/v1/filter/inchikey",
    httr::add_headers(.headers = headers), body = body
  )
  if (postres$status_code == 200) {
    out <- get_csid(postres = postres, headers = headers)
    return(out)
  }
  else {
    stop(httr::http_status(postres)$message)
  }
}


#' Convert between SMILES, InChI, InChiKey, Mol identifiers using Chemspider
#'
#' Submit an identifier (SMILES, InChI, InChIKey or Mol) and return an identifier
#' in another format (SMILES, InChI, InChIKey or Mol). Not all conversions are
#' supported.
#' @importFrom jsonlite fromJSON toJSON
#' @importFrom httr POST add_headers http_status
#' @param input character; the identifier to be converted.
#' @param from character; the format to be converted from. Valid values are smiles,
#' inchi, inchikey, mol.
#' @param to character; the format to be converted to. Valid values are smiles,
#' inchi, inhikey, mol.
#' @param apikey character; your API key.
#' @details Not all conversions are supported. Allowed conversions:
#' \itemize{
#' \item InChI <-> InChIKey
#' \item InChI <-> SMILES
#' \item InChI <-> Mol file
#' \item InChIKey <-> Mol file
#' }
#' @return Returns a character vector of length one containing the converted identifier.
#' @note An API key is needed. Register at \url{https://developer.rsc.org/}
#' for an API key. Please respect the Terms & Conditions. The Terms & Conditions
#' can be found at \url{https://developer.rsc.org/terms}.
#' @references https://developer.rsc.org/compounds-v1/apis
#' @author Tamas Stirling, \email{stirling.tamas@@gmail.com}
#' @seealso This is a low level function. See \code{\link{cs_convert}}
#' for the top level function.
#' @seealso \code{\link{parse_mol}}
#' @examples
#' \dontrun{
#' apikey <- "<YOUR-API-KEY>"
#' cs_convert_multiple("CC(=O)O","smiles","inchi",apikey)
#' cs_convert_multiple("InChI=1S/C2H4O2/c1-2(3)4/h1H3,(H,3,4)","inchi","inchikey",apikey)
#' cs_convert_multiple("QTBSBXVTEAMEQO-UHFFFAOYSA-N","inchikey","mol",apikey)
#' cs_convert_multiple("QTBSBXVTEAMEQO-UHFFFAOYSA-N","inchikey","mol",apikey, parse = TRUE)
#' }
cs_convert_multiple <- function(input, from, to, apikey){
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
}

#' Convert identifiers using Chemspider
#'
#' Submit an identifier (CSID, SMILES, InChI, InChIKey or Mol) and return an
#' identifier in another format (CSID, SMILES, InChI, InChIKey or Mol).
#' @param query character; query ID.
#' @param from character; type of query ID.
#' @param to character; type to convert to.
#' @param apikey character; your API key.
#' @details Not all conversions are supported. Allowed conversions:
#' \itemize{
#' \item CSID <-> InChI
#' \item CSID <-> InChIKey
#' \item CSID <-> SMILES
#' \item CSID -> Mol file
#' \item InChI <-> InChIKey
#' \item InChI <-> SMILES
#' \item InChI <-> Mol file
#' \item InChIKey <-> Mol file
#' }
#' @return Returns a character vector of length one containing the converted identifier.
#' @note An API key is needed. Register at \url{https://developer.rsc.org/}
#' for an API key. Please respect the Terms & Conditions. The Terms & Conditions
#' can be found at \url{https://developer.rsc.org/terms}.
#' @references https://developer.rsc.org/compounds-v1/apis
#' @author Eduard Szoecs, \email{eduardszoecs@@gmail.com}
#' @author Tamas Stirling, \email{stirling.tamas@@gmail.com}
#' @export
#' @examples
#' \dontrun{
#' apikey <- "<YOUR-API-KEY>"
#' cs_convert('BQJCRHHNABKAKU-KBQPJGBKSA-N', from = 'inchikey', to = 'csid',
#' apikey = apikey)
#' cs_convert('BQJCRHHNABKAKU-KBQPJGBKSA-N', from = 'inchikey', to = 'inchi',
#' apikey = apikey)
#' cs_convert('BQJCRHHNABKAKU-KBQPJGBKSA-N', from = 'inchikey', to = 'mol',
#' apikey = apikey)
#'}
cs_convert <- function(query, from, to, apikey) {
  valid <- c('csid', 'inchikey', 'inchi', 'smiles','mol')
  from <- match.arg(from, choices = valid)
  to <- match.arg(to, choices = valid)
  cs_compinfo_dict <- data.frame(
    "name" = c("inchi", "inchikey", "smiles", "mol"),
    "cs_compinfo" = c("InChI", "InChIKey", "SMILES", "Mol2D"),
    stringsAsFactors = FALSE
  )
  to2=cs_compinfo_dict[which(cs_compinfo_dict$name == to),2]
  cs_convert_router <- function(from, to){
    if(from == to) return("identity")
    if(from == "csid") return("cs_compinfo")
    if(to == "csid") return(paste("cs",from,to,sep="_"))
    if(from != "csid" & to != "csid") return("cs_convert_multiple")
  }
  switch(cs_convert_router(from,to),
         identity = query,
         cs_compinfo = cs_compinfo(query, fields = to2, apikey = apikey)[1,2],
         cs_convert_multiple = cs_convert_multiple(query,from = from, to = to,
                                                   apikey = apikey),
         cs_inchikey_csid = cs_inchikey_csid(query, apikey = apikey),
         cs_inchi_csid = cs_inchi_csid(query, apikey = apikey),
         cs_smiles_csid = cs_smiles_csid(query, apikey = apikey),
         cs_mol_csid = stop("Conversion not supported.")
         )
}

#' Retrieve record details by ChemSpider ID
#'
#' Submit a Chemspider ID (CSID) and the fields you are interested in, and
#' retrieve the record details for your query.
#' @importFrom jsonlite toJSON
#' @importFrom httr POST add_headers
#' @param csid numeric
#' @param fields character; see details.
#' @param apikey character; your API key.
#' @details Valid values for \code{fields} are \code{"SMILES"}, \code{"Formula"},
#' \code{"InChI"}, \code{"InChIKey"}, \code{"StdInChI"}, \code{"StdInChIKey"},
#' \code{"AverageMass"}, \code{"MolecularWeight"}, \code{"MonoisotopicMass"},
#' \code{"NominalMass"}, \code{"CommonName"}, \code{"ReferenceCount"},
#' \code{"DataSourceCount"}, \code{"PubMedCount"}, \code{"RSCCount"},
#' \code{"Mol2D"}, \code{"Mol3D"}. You can specify any number of fields.
#' @return Returns a data frame.
#' @note An API key is needed. Register at \url{https://developer.rsc.org/}
#' for an API key. Please respect the Terms & Conditions. The Terms & Conditions
#' can be found at \url{https://developer.rsc.org/terms}.
#' @references https://developer.rsc.org/compounds-v1/apis
#' @author Tamas Stirling, \email{stirling.tamas@@gmail.com}
#' @export
#' @examples
#' \dontrun{
#' cs_compinfo(171, c("SMILES","CommonName"), apikey)
#' }
cs_compinfo <- function(csid, fields, apikey){
  fields <- match.arg(fields,
                      choices = c("SMILES", "Formula", "InChI", "InChIKey",
                                  "StdInChI", "StdInChIKey", "AverageMass",
                                  "MolecularWeight", "MonoisotopicMass",
                                  "NominalMass", "CommonName", "ReferenceCount",
                                  "DataSourceCount", "PubMedCount", "RSCCount",
                                  "Mol2D", "Mol3D"),
                      several.ok = TRUE)
  headers <- c("Content-Type" = "", "apikey" = apikey)
  body <- list(
    "recordIds" = csid, "fields" = fields
  )
  body <- jsonlite::toJSON(body)
  postres <- httr::POST(
    url = "https://api.rsc.org/compounds/v1/records/batch",
    httr::add_headers(.headers = headers), body = body
  )
  if (postres$status_code == 200) {
    out <- jsonlite::fromJSON(rawToChar(postres$content))$records
    return(out)
  }
  else {
    stop(httr::http_status(postres)$message)
  }
}

#' Get extended record details by ChemSpider ID
#'
#' Get extended info from Chemspider, see \url{https://www.chemspider.com/}
#' @import xml2
#' @importFrom stats rgamma
#' @param csid character,  ChemSpider ID.
#' @param token character; security token.
#' @param verbose logical; should a verbose output be printed on the console?
#' @param ... currently not used.
#' @return a data.frame with entries: 'csid', 'mf' (molecular formula), 'smiles', 'inchi' (non-standard),
#' 'inchikey' (non-standard), 'average_mass', 'mw' (Molecular weight), 'monoiso_mass' (MonoisotopicMass),
#' 'nominal_mass', 'alogp', 'xlogp', 'common_name' and 'source_url'
#' @note A security token is needed. Please register at RSC
#' \url{https://www.rsc.org/rsc-id/register}
#' for a security token.
#' Please respect the Terms & conditions \url{https://www.rsc.org/help-legal/legal/terms-conditions/}.
#' @author Eduard Szoecs, \email{eduardszoecs@@gmail.com}
#' @seealso \code{\link{get_csid}} to retrieve ChemSpider IDs,
#' \code{\link{cs_compinfo}} for extended compound information.
#' @note use \code{\link{cs_compinfo}} to retrieve standard inchikey.
#' @export
#' @examples
#' \dontrun{
#' # Fails because no TOKEN is included
#' token <- '<YOUR-SECURITY-TOKEN>'
#' csid <- get_csid("Triclosan", token = token)
#' cs_extcompinfo(csid, token)
#'
#' csids <- get_csid(c('Aspirin', 'Triclosan'), token = token)
#' cs_compinfo(csids, token = token)
#' }
cs_extcompinfo <- function(csid, token, verbose = TRUE, ...){
  # csid <- "5363"
  foo <- function(csid, token, verbose) {
    if (is.na(csid)) {
      out <- as.list(rep(NA, 13))
      names(out) <- c('csid', 'mf', 'smiles', 'inchi', 'inchikey', 'average_mass',
                      'mw', 'monoiso_mass', 'nominal_mass', 'alogp', 'xlogp', 'common_name', 'source_url')
      return(out)
    }
    baseurl <- 'https://www.chemspider.com/MassSpecAPI.asmx/GetExtendedCompoundInfo?'
    qurl <- paste0(baseurl, 'CSID=', csid, '&token=', token)
    if (verbose)
      message(qurl)
    Sys.sleep( rgamma(1, shape = 15, scale = 1/45))
    h <- try(read_xml(qurl), silent = TRUE)
    if (inherits(h, "try-error")) {
      warning('CSID not found... Returning NA.')
      return(NA)
    }
    out <- as.list(xml_text(xml_children(h)))
    names(out) <- c('csid', 'mf', 'smiles', 'inchi', 'inchikey', 'average_mass',
                    'mw', 'monoiso_mass', 'nominal_mass', 'alogp', 'xlogp', 'common_name')
    # convert to numeric
    out[['average_mass']] <- as.numeric(out[['average_mass']])
    out[['mw']] <- as.numeric(out[['mw']])
    out[['monoiso_mass']] <- as.numeric(out[['monoiso_mass']])
    out[['nominal_mass']] <- as.numeric(out[['nominal_mass']])
    out[['alogp']] <- as.numeric(out[['alogp']])
    out[['xlogp']] <- as.numeric(out[['xlogp']])
    source_url <- paste0('https://www.chemspider.com/Chemical-Structure.', csid, '.html')
    out[['source_url']] <- source_url
    return(out)
  }
  out <- sapply(csid, foo, token = token, verbose = verbose)
  out <- data.frame(t(out), row.names = seq_len(ncol(out)))
  out[['query']] <- rownames(out)
  out <- data.frame(t(apply(out, 1, unlist)), stringsAsFactors = FALSE)
  return(out)
}

#' Get predicted chemical properties from ChemSpider
#'
#' Get predicted (ACD/Labs and EPISuite) chemical properties from ChemSpider,
#' see \url{https://www.chemspider.com/}
#' @import xml2 stringr rvest
#' @importFrom stats rgamma
#'
#' @param csid character,  ChemSpider ID.
#' @param verbose logical; should a verbose output be printed on the console?
#' @param ... currently not used.
#'
#' @return A list of lists with of three: acd (data.frame), epi (data.frame) and source_url.
#'
#' @note Please respect the Terms & conditions \url{https://www.rsc.org/help-legal/legal/terms-conditions/}.
#' @author Eduard Szoecs, \email{eduardszoecs@@gmail.com}
#' @seealso \code{\link{get_csid}} to retrieve ChemSpider IDs,
#' \code{\link{cs_compinfo}} for extended compound information.
#' @export
#' @examples
#' \dontrun{
#' out <- cs_prop(5363)
#' out[[1]]$epi
#'
#' out2 <- cs_prop(c(5363, 2157))
#' # extract Log Octanol-Water Partition Coef from EPI
#' sapply(out2, function(y){
#'   y$epi$value_pred[y$epi$prop == 'Log Octanol-Water Partition Coef']
#' })
#' }
cs_prop <- function(csid, verbose = TRUE, ...){

  save_val <- function(x) {
    if (length(x) == 0) {
      return(NA)
    }
    if (length(x) > 1) {
      return(x[1])
    } else {
      return(x)
    }
  }

  foo <- function(csid, verbose){
    qurl <- paste0('https://www.chemspider.com/Chemical-Structure.', csid, '.html')
    if (verbose)
      message(qurl)
    Sys.sleep( rgamma(1, shape = 10, scale = 1/10))
    # readLines to catch html errors in CS
    doc <- try(readLines(qurl), silent = TRUE)
    if (inherits(doc, "try-error")) {
      warning('CSID not found... Returning NA.')
      return(NA)
    }

    doc <- paste(doc, collapse = "\n")
    # CS contains some invalid syntax that we try to fix here before parsing.
    doc <- gsub("MP  (exp database):  <", "MP  (exp database):  ", doc, fixed = TRUE)
    h <- read_html(doc)

    ### acd
    acd <- do.call(rbind, html_table(xml_find_all(h, '//div[@class="column two"]/table')))
    names(acd) <- c('variable', 'val')
    acd$variable <- gsub('^(.*)\\:$', '\\1', acd$variable)

    # ^ - Beginning of the line;
    # \\d* - 0 or more digits;
    # \\.? - An optional dot (escaped, because in regex, . is a special character);
    # \\d* - 0 or more digits (the decimal part);
    # $ - End of the line.
    acd$value <- as.numeric(gsub('^(\\d*\\.?\\d*).*$', '\\1', acd$val))
    acd$error <- as.numeric(ifelse(grepl('\u00B1' , acd$val), gsub('^\\d*\\.?\\d*\u00B1(\\d*\\.?\\d*)\\s.*$', '\\1', acd$val), NA))
    acd$unit <- ifelse(grepl('\\s.*\\d*$', acd$val),
           gsub('^.*\\d*\\s(.*\\d*)$', '\\1', acd$val),
           NA)
    acd$val <- NULL


    ### episuite
    epi <- data.frame(property = character(),
                      value_pred = numeric(),
                      unit_pred = character(),
                      source_pred = character(),
                      value_exp = numeric(),
                      unit_exp = character(),
                      source_exp = character())


    kow_raw <- xml_text(xml_find_all(h, '//div[@id="epiTab"]/pre'))
    if (length(kow_raw) != 0) {
      ll <- str_split(kow_raw, '\n')
      ll <- sapply(ll, str_trim)
      ll <- ll[!ll == '']

      # Log Octanol-Water Partition Coef
      prop <- 'Log Octanol-Water Partition Coef'
      value_pred <- save_val(as.numeric(gsub('.* = \\s(.*)','\\1', ll[grepl('^Log Kow \\(KOWW', ll)])))
      unit_pred <- NA
      source_pred <- save_val(gsub('(.*) = \\s(.*)','\\1', ll[grepl('^Log Kow \\(KOWW', ll)]))
      value_exp <- save_val(as.numeric(gsub('.* = \\s(.*)',
                                            '\\1', ll[grepl('^Log Kow \\(Exper.', ll)])))
      unit_exp <- NA
      source_exp <- save_val(gsub('^.*\\:\\s(.*)','\\1',
                                  ll[which(grepl('^Log Kow \\(Exper.', ll)) + 1]))

      # Boiling Point
      prop <- c(prop, 'Boiling Point')
      value_pred <- c(value_pred,
                      save_val(as.numeric(gsub('.*:\\s+([-+]?[0-9]*\\.?[0-9]+[eE]?[-+]?[0-9]*).*',
                                      '\\1',
                                      ll[grepl('^Boiling Pt \\(deg C', ll)]))))
      unit_pred <- c(unit_pred, 'deg C')
      source_pred <- c(source_pred,
                       save_val(gsub('^.*\\((.*)\\)\\:$',
                            '\\1',
                            ll[grepl('^Boiling Pt, ', ll)])))

      value_exp <- c(value_exp, save_val(as.numeric(gsub('.*:\\s+([-+]?[0-9]*\\.?[0-9]+[eE]?[-+]?[0-9]*).*',
                                                             '\\1',
                                                             ll[grepl('^BP  \\(exp database', ll)]))))
      unit_exp <- c(unit_exp, 'deg C')
      source_exp <- c(source_exp, NA)

      # Melting Point
      prop <- c(prop, 'Melting Point')
      value_pred <- c(value_pred,
                      save_val(as.numeric(gsub('.*:\\s+([-+]?[0-9]*\\.?[0-9]+[eE]?[-+]?[0-9]*).*',
                                      '\\1',
                                      ll[grepl('^Melting Pt \\(deg C', ll)]))))
      unit_pred <- c(unit_pred, 'deg C')
      source_pred <- c(source_pred,
                       save_val(gsub('^.*\\((.*)\\)\\:$',
                            '\\1',
                            ll[grepl('^Boiling Pt, ', ll)])))
      value_exp <- c(value_exp, save_val(as.numeric(gsub('.*:\\s+([-+]?[0-9]*\\.?[0-9]+).*',
                                                         '\\1',
                                                         ll[grepl('^MP  \\(exp database', ll)]))))
      unit_exp <- c(unit_exp, 'deg C')
      source_exp <- c(source_exp, NA)
      # epi_mp_exp <- as.numeric(gsub('.*:\\s+([-+]?[0-9]*\\.?[0-9]+).*','\\1', ll[grepl('^MP\\s+\\(exp', ll)]))
      # epi_bp_exp <- as.numeric(gsub('.*:\\s+([-+]?[0-9]*\\.?[0-9]+).*','\\1', ll[grepl('^BP\\s+\\(exp', ll)]))

      # Water Solubility from KOW
      prop <- c(prop, 'Water Solubility from KOW')
      value_pred <- c(value_pred, save_val(as.numeric(gsub('.*:\\s+([-+]?[0-9]*\\.?[0-9]+[eE]?[-+]?[0-9]*).*',
                                                  '\\1',
                                                  ll[grepl('^Water Solubility at 25 deg C', ll)]))))
      unit_pred <- c(unit_pred, 'mg/L (25 deg C)')
      source_pred <- c(source_pred, save_val(gsub('^.*\\((.*)\\)\\:$','\\1',
                                         ll[grepl('^Water Solubility Estimate from Log Kow', ll)])))
      value_exp <- c(value_exp,
                     save_val(as.numeric(gsub('.*=\\s+([-+]?[0-9]*\\.?[0-9]+[eE]?[-+]?[0-9]*).*',
                                              '\\1',
                                              ll[grepl('^Water Sol \\(Exper. database match', ll)]))))
      unit_exp <- c(unit_exp,
                    save_val(gsub('.*=\\s+([-+]?[0-9]*\\.?[0-9]+[eE]?[-+]?[0-9]*)(.*)',
                                  '\\2',
                                  ll[grepl('^Water Sol \\(Exper. database match', ll)])))
      source_exp <- c(source_exp,
                      save_val(gsub('^.*\\:\\s(.*)',
                                    '\\1',
                                    ll[which(grepl('^Water Sol \\(Exper. database match', ll)) + 1])))

      # Water Solubility from Fragments
      prop <- c(prop, 'Water Solubility from Fragments')
      value_pred <- c(value_pred, save_val(as.numeric(gsub('.*=\\s+([-+]?[0-9]*\\.?[0-9]+[eE]?[-+]?[0-9]*).*',
                                                  '\\1',
                                                  ll[grepl('^Wat Sol \\(v1.01', ll)]))))
      unit_pred <- c(unit_pred, 'mg/L')
      source_pred <- c(source_pred, NA)
      value_exp <- c(value_exp,  NA)
      unit_exp <- c(unit_exp, NA)
      source_exp <- c(source_exp, NA)

      # Log Octanol-Air Partition Coefficient (25 deg C)
      prop <- c(prop, 'Log Octanol-Air Partition Coefficient (25 deg C)')

      value_pred_new <- save_val(as.numeric(gsub('.*:\\s(.*)', '\\1', ll[grepl('^Log Koa \\(KOAWIN', ll)])))
      value_pred <- c(value_pred, ifelse(length(value_pred_new)==0,NA,value_pred_new))

      unit_pred <- c(unit_pred, NA)
      source_pred <- c(source_pred, save_val(gsub('^.*\\[(.*)\\]\\:$',
                                         '\\1',
                                         ll[grepl('^Log Octanol-Air Partition Coefficient', ll)])))
      value_exp <- c(value_exp,
                     save_val(suppressWarnings(as.numeric(gsub('^.*\\:(.*)',
                                              '\\1',
                                              ll[grepl('^Log Koa \\(experimental database\\).*', ll)])))))
      unit_exp <- c(unit_exp, NA)
      source_exp <- c(source_exp, NA)

      # Log Soil Adsorption Coefficient
      prop <- c(prop, 'Log Soil Adsorption Coefficient')
      value_pred <- c(value_pred,
                      save_val(as.numeric(gsub('.*:\\s+([-+]?[0-9]*\\.?[0-9]+).*',
                                      '\\1',
                                      ll[grepl('^Log Koc:', ll)]))))
      unit_pred <- c(unit_pred, NA)
      source_pred <- c(source_pred, save_val(gsub('^.*\\((.*)\\)\\:$',
                                         '\\1',
                                         ll[grepl('^Soil Adsorption Coefficient', ll)])))
      value_exp <- c(value_exp, NA)
      unit_exp <- c(unit_exp, NA)
      source_exp <- c(source_exp, NA)

      epi <- data.frame(prop, value_pred, unit_pred, source_pred,
                        value_exp, unit_exp, source_exp, stringsAsFactors = FALSE)
    }

    out <- list(acd = acd,
                epi = epi,
                source_url = qurl)
    return(out)
  }

  out <- lapply(csid, foo, verbose = verbose)
  out <- setNames(out, csid)
  return(out)
}
