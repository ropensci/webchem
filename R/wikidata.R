#' Get Wikidata Item ID
#'
#' Search www.wikidata.org for wikidata item identifiers.  Note that this search
#' is currently not limited to chemical substances, so be sure to check your
#' results.
#'
#' @param query character; The searchterm
#' @param language character; the language to search in
#' @param match character; How should multiple hits be handeled? 'all' returns
#'   all matched IDs, 'first' only the first match, 'best' the best matching (by
#'   name) ID, 'ask' is a interactive mode and the user is asked for input, na'
#'   returns NA if multiple hits are found.
#' @param verbose logical; print message during processing to console?
#'
#' @return if match = 'all' a list with ids, otherwise a dataframe with 4 columns:
#' id, matched text, string distance to match and the queried string
#'
#' @note Only matches in labels are returned.
#'
#' @import jsonlite httr
#' @importFrom stats rgamma
#' @importFrom utils URLencode URLdecode
#' @importFrom purrr map_df
#' @export
#' @examples
#' \dontrun{
#' get_wdid('Triclosan', language = 'de')
#' get_wdid('DDT')
#' get_wdid('DDT', match = 'all')
#'
#' # multiple inputs
#' comps <- c('Triclosan', 'Glyphosate')
#' get_wdid(comps)
#' }
get_wdid <-
  function(query,
           match = c('best', 'first', 'all', 'ask', 'na'),
           verbose = TRUE,
           language = 'en') {

    if (!ping_service("wd")) stop(webchem_message("service_down"))

  # language <-  'en'
  # query <- 'Triclosan'

  match <- match.arg(match)
  foo <- function(query, language, match, verbose){
    if (is.na(query)){
      if (verbose) webchem_message("na")
      id <- NA
      matched_sub <- NA
    } else {
      query1 <- URLencode(query, reserved = TRUE)
      limit <- 50
      qurl <-
        paste0("wikidata.org/w/api.php?action=wbsearchentities&format=json&type=item")
      qurl <- paste0(qurl, "&language=", language, "&limit=", limit, "&search=", query1)
      if (verbose) webchem_message("query", query, appendLF = FALSE)
      Sys.sleep(0.3)
      res <- try(httr::RETRY("GET",
                             qurl,
                             httr::user_agent(webchem_url()),
                             terminate_on = 404,
                             quiet = TRUE), silent = TRUE)
      if (inherits(res, "try-error")) {
        if (verbose) webchem_message("service_down")
        return(tibble::tibble(query = query, match = NA, wdid = NA))
      }
      if (verbose) message(httr::message_for_status(res))
      if (res$status_code == 200) {
        cont <- jsonlite::fromJSON(httr::content(res,
                                                 type = "text",
                                                 encoding = "utf-8"))
        search <- cont$search
        if (length(search) == 0) {
          if (verbose) webchem_message("not_found")
          id <- NA
          matched_sub <- NA
        } else {
          # use only matches on label
          search <- search[search$match$type %in% c('label', 'alias'), ]
          # # check matches
          search <- search[tolower(iconv(search$match$text,
                                         "latin1",
                                         "ASCII",
                                         sub = "")) == tolower(query), ]
          id <-
            matcher(
              search$id,
              query = query,
              result = search$label,
              match = match,
              # from = from,
              verbose = verbose
            )
          matched_sub <- names(id)
        }
      }
      else {
        id <- NA
        matched_sub <- NA
      }
    }
    out <- tibble(query = query, match = matched_sub, wdid = id)
    return(out)
  }
  out <-
    purrr::map_df(query,
           ~ foo(
             query = .x,
             match = match,
             language = language,
             verbose = verbose
           ))
  return(out)
}

#! Use SPARQL to search of chemical compounds (P31)?! For a finer / better search?



#' Retrieve Indentifiers from Wikidata
#'
#' @import jsonlite
#' @import httr
#' @importFrom stats rgamma
#'
#' @param id character; identifier, as returned by \code{\link{get_wdid}}
#' @param verbose logical; print message during processing to console?
#'
#' @return A data.frame of identifiers. Currently these are 'smiles', 'cas', 'cid', 'einecs', 'csid', 'inchi', 'inchikey',
#' 'drugbank', 'zvg', 'chebi', 'chembl', 'unii' and source_url.
#'
#' @note Only matches in labels are returned. If more than one unique hit is found,
#' only the first is returned.
#'
#' @seealso \code{\link{get_wdid}}
#'
#' @references Willighagen, E., 2015. Getting CAS registry numbers out of WikiData. The Winnower.
#' \url{http://dx.doi.org/10.15200/winn.142867.72538}
#'
#' Mitraka, Elvira, Andra Waagmeester, Sebastian Burgstaller-Muehlbacher, et al. 2015
#' Wikidata: A Platform for Data Integration and Dissemination for the Life Sciences and beyond. bioRxiv: 031971.
#'
#' @export
#' @examples
#' \dontrun{
#'  id <- c("Q408646", "Q18216")
#'  wd_ident(id)
#' }
wd_ident <- function(id, verbose = TRUE){

  if (!ping_service("wd")) stop(webchem_message("service_down"))

  # id <- c( "Q163648", "Q18216")
  # id <- 'Q408646'
  foo <- function(id, verbose){
    empty <- as.list(rep(NA, 13))
    names(empty) <- c("smiles", "cas", "cid", "einecs", "csid", "inchi",
                      "inchikey", "drugbank", "zvg", "chebi", "chembl", "unii",
                      "source_url")
    if (is.na(id)) {
      if (verbose) webchem_message("na")
      return(empty)
    }
    baseurl <- 'https://query.wikidata.org/sparql?format=json&query='
    props <- c('P233', 'P231', 'P662', 'P232', 'P661', 'P234', 'P235', 'P715', 'P679',
               'P683', 'P592', 'P652')
    names <- c('smiles', 'cas', 'cid', 'einecs', 'csid', 'inchi', 'inchikey',
               'drugbank', 'zvg', 'chebi', 'chembl', 'unii')

    sparql_head <- paste('PREFIX wd: <http://www.wikidata.org/entity/>',
      'PREFIX wdt: <http://www.wikidata.org/prop/direct/>',
      'SELECT * WHERE {')
    sparql_body <- paste(paste0('OPTIONAL{wd:', id, ' wdt:', props, ' ?', names, ' .}'),
          collapse = ' ')
    sparql <- paste(sparql_head, sparql_body, '}')
    qurl <- paste0(baseurl, sparql)
    qurl <- URLencode(qurl)
    Sys.sleep( rgamma(1, shape = 15, scale = 1/10))
    if (verbose) webchem_message("query", id, appendLF = FALSE)
    res <- try(httr::RETRY("GET",
                           qurl,
                           httr::user_agent(webchem_url()),
                           terminate_on = 404,
                           quiet = TRUE), silent = TRUE)
    if (inherits(res, "try-error")) {
      if (verbose) webchem_message("service_down")
      return(empty)
    }
    if (verbose) message(httr::message_for_status(res))
    if (res$status_code == 200) {
      tmp <- fromJSON(content(res, as = "text"))

      vars_out <- tmp$head$vars
      out <- tmp$results$bindings

      if (length(out) == 0) {
        if (verbose) webchem_message("not_found")
        out <- as.list(rep(NA, 13))
        names(out) <- c(vars_out, 'source_url')
        return(out)
      }

      if (nrow(out) > 1) {
        message("More then one unique entry found! Returning first.")
        out <- out[1, ]
      }

      out <- lapply(out, '[[', 'value')

      # check for missing entries and add to out-list
      miss <- names[!names %in% names(out)]
      for (i in miss) {
        out[[i]] <- NA
      }
      out <- out[names]
      out[['source_url']] <- paste0('https://www.wikidata.org/wiki/', id)
      out <- unlist(out)
      return(out)
    }
    else {
      return(empty)
    }
  }
  # ugly fixing to return data.frame
  out <- data.frame(t(sapply(id, foo,verbose = verbose)), stringsAsFactors = FALSE, row.names = seq_along(id))
  out[['query']] <- id
  # even more ugly...
  out <- data.frame(t(apply(out, 1, unlist)), stringsAsFactors = FALSE)
  class(out) <- c('wd_ident', 'data.frame')
  return(out)
}
