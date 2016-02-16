#' Get Wikidata Item ID
#'
#' @import jsonlite httr
#' @importFrom stats rgamma
#'
#' @param query character; The searchterm
#' @param language character; the language to search in
#' @param match character; How should multiple hits be handeled? 'all' returns all matched IDs,
#' 'first' only the first match, 'best' the best matching (by name) ID, 'ask' is a interactive mode and the user is asked for input,
#' 'na' returns NA if multiple hits are found.
#' @param verbose logical; print message during processing to console?
#'
#' @return if match = 'all' a list with ids, otherwise a dataframe with 4 columns:
#' id, matched text, string distance to match and the queried string
#'
#' @note Only matches in labels are returned.
#'
#' @author Eduard Szoecs, \email{eduardszoecs@@gmail.com}
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
get_wdid <- function(query, language = 'en', match = c('best', 'first', 'all', 'ask', 'na'),
                     verbose = TRUE){
  # language <-  'en'
  # query <- 'Triclosan'
  match <- match.arg(match)
  foo <- function(query, language, match, verbose){
    limit <-  50
    qurl <- paste0("wikidata.org/w/api.php?action=wbsearchentities&format=json&type=item")
    qurl <- paste0(qurl, "&language=", language, "&limit=", limit, "&search=", query)
    if (verbose)
      message('Querying ', qurl)
    Sys.sleep(0.3)
    cont <- fromJSON(content(GET(qurl, user_agent('webchem (https://github.com/ropensci/webchem)')), 'text'))
    search <- cont$search
    if (length(search) == 0) {
      if (verbose)
        message('Substance not found! Returing NA. \n')
      id <- NA
      attr(id, "matched") <- NA
      attr(id, "distance") <- NA
      return(id)
    }
    # use only matches on label
    search <- search[search$match$type %in% c('label', 'alias'), ]
    # # check matches
    search <- search[tolower(search$match$text) == tolower(query), ]

    if (nrow(search) > 1) {
      if (verbose)
        message("More then one Link found. \n")
      if (match == 'na') {
        if (verbose)
          message("Returning NA. \n")
        id <- NA
        matched_sub <- NA
        d <- NA
      }
      if (match == 'all') {
        if (verbose)
          message("Returning all matches. \n")
        id <- search$id
        matched_sub <- search$label
        d <- 'all'
      }
      if (match == 'first') {
        if (verbose)
          message("Returning first match. \n")
        id <- search$id[1]
        matched_sub <- search$label[1]
        d <- 'first'
      }
      if (match == 'best') {
        if (verbose)
          message("Returning best match. \n")
        dd <- adist(query, search$label) / nchar(search$label)
        id <- search$id[which.min(dd)]
        d <- round(dd[which.min(dd)], 2)
        matched_sub <- search$label[which.min(dd)]
      }
      if (match == 'ask') {
        tochoose <- data.frame(match = search$label, url = search$url, description = search$description)
        print(tochoose)
        message("\nEnter rownumber of compounds (other inputs will return 'NA'):\n") # prompt
        take <- as.numeric(scan(n = 1, quiet = TRUE))
        if (length(take) == 0) {
          id <- NA
          matched_sub <- NA
          d <- NA
        }
        if (take %in% seq_len(nrow(tochoose))) {
          id <- search$id[take]
          matched_sub <- search$label[take]
          d <- 'interactive'
        }
      }
    } else {
      id <- search$id
      d <- 0
      matched_sub <- search$label
    }
    names(id) <- NULL
    attr(id, "matched") <- matched_sub
    attr(id, "distance") <- d
    return(id)
  }
  out <- lapply(query, foo, language = language, match = match, verbose = verbose)
  if (match != 'all') {
    out <- data.frame(t(sapply(out, function(y) {
      c(y, attr(y, 'matched'), attr(y, 'distance'))
    })), stringsAsFactors = FALSE)
    names(out) <- c('id', 'match', 'distance')
    out[['query']] <- query
  }
  return(out)
}

#! Use SPARQL to search of chemical compounds (P31)?! For a finer / better search?



#' Retrieve Indentifiers from wikidata
#'
#' @import jsonlite
#' @importFrom stats rgamma
#'
#' @param id character; identifier, as returned by \code{\link{get_wdid}}
#' @param verbose logical; print message during processing to console?
#'
#' @return A data.frame of identifiers. Currently these are 'smiles', 'cas', 'cid', 'einecs', 'csid', 'inchi', 'inchikey',
#' 'drugbank', 'zvg', 'chebi', 'chembl', 'unii' and source_url.
#'
#' @note Only matches in labels are returned.
#'
#' @seealso \code{\link{get_wdid}}
#'
#' @references Willighagen, E., 2015. Getting CAS registry numbers out of WikiData. The Winnower.
#' \url{http://dx.doi.org/10.15200/winn.142867.72538}
#'
#' Mitraka, Elvira, Andra Waagmeester, Sebastian Burgstaller-Muehlbacher, et al. 2015
#' Wikidata: A Platform for Data Integration and Dissemination for the Life Sciences and beyond. bioRxiv: 031971.
#'
#' @author Eduard Szoecs, \email{eduardszoecs@@gmail.com}
#' @export
#' @examples
#' \dontrun{
#'  id <- c("Q408646", "Q18216")
#'  wd_ident(id)
#' }
wd_ident <- function(id, verbose = TRUE){
  # id <- c( "Q163648", "Q18216")
  # id <- 'Q408646'
  foo <- function(id, verbose){
    if (is.na(id)) {
      if (verbose)
        message('NA as input! Returing NA. \n')
      out <- as.list(rep(NA, 13))
      names(out) <- c("smiles", "cas", "cid", "einecs", "csid", "inchi", "inchikey",
                      "drugbank", "zvg", "chebi", "chembl", "unii", "source_url")
      return(out)
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
    if (verbose)
      message('Querying ', qurl)
    tmp <- fromJSON(qurl)

    vars_out <- tmp$head$vars
    out <- tmp$results$bindings

    if (length(out) == 0) {
      if (verbose)
        message('Not found! Returing NA. \n')
      out <- as.list(rep(NA, 13))
      names(out) <- c(vars_out, 'source_url')
      return(out)
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
  # ugly fixing to return data.frame
  out <- data.frame(t(sapply(id, foo,verbose = verbose)), stringsAsFactors = FALSE, row.names = seq_along(id))
  out[['query']] <- id
  # even more ugly...
  out <- data.frame(t(apply(out, 1, unlist)), stringsAsFactors = FALSE)
  class(out) <- c('wd_ident', 'data.frame')
  return(out)
}

