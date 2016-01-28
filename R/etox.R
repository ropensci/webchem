#' Get ETOX ID
#'
#' Query ETOX: Information System Ecotoxicology and Environmental Quality Targets
#' \url{https://webetox.uba.de/webETOX/index.do} for their substance ID
#'
#' @import xml2 httr
#' @importFrom stats rgamma
#'
#' @param query character; The searchterm
#' @param mult character; How should multiple hits be handeled? 'all' returns all matched IDs,
#' 'first' only the first match, 'best' the best matching (by name) ID, 'ask' is a interactive mode and the user is asked for input,
#' 'na' returns NA if multiple hits are found.
#'
#' @param verbose logical; print message during processing to console?
#'
#' @return A character vector with the substance ID and additional attributes \code{matched}  (the matched
#' substance name) and \code{d} (either the string distance to the match, or the type of match).
#'
#' @note Before using this function, please read the disclaimer \url{https://webetox.uba.de/webETOX/disclaimer.do}.
#'
#' @seealso \code{\link{get_etoxid}} to retrieve ETOX IDs, \code{\link{etox_basic}} for basic information,
#' \code{\link{etox_targets}} for quality targets and \code{\link{etox_tests}} for test results.
#'
#' @author Eduard Szoecs, \email{eduardszoecs@@gmail.com}
#' @export
#' @examples
#' \dontrun{
#' # might fail if API is not available
#' get_etoxid('Triclosan')
#' # multiple inpus
#' comps <- c('Triclosan', 'Glyphosate')
#' sapply(comps, get_etoxid)
#' }
get_etoxid <- function(query, mult = c('all', 'first', 'best', 'ask', 'na'), verbose = TRUE) {
  if (length(query) > 1) {
    stop('Cannot handle multiple input strings.')
  }
  clean_char <- function(x){
    # rm \n \t
    x <- gsub('\n | \t', '', x)
    # rm leading / trailling whitespace
    x <- gsub("^\\s+|\\s+$", "", x)
    # replace multiple spaces by one,
    # http://stackoverflow.com/questions/25707647/merge-multiple-spaces-to-single-space-remove-trailing-leading-spaces
    x <- gsub("(?<=[\\s])\\s*|^\\s+$", "", x, perl = TRUE)
    return(x)
  }

  mult <- match.arg(mult)

  # query <- 'Triclosan'
  # query <- 'Thiamethoxam'
  if (verbose)
    message('Searching ', query)
  baseurl <- 'https://webetox.uba.de/webETOX/public/search/stoff.do'

  Sys.sleep( rgamma(1, shape = 15, scale = 1/10))
  h <- POST(url = baseurl, body = list('stoffname.selection[0].name' = query,
                                  event = 'Search'))
  # get substances and links
  tt <- read_html(h)
  subs <- clean_char(xml_text(xml_find_all(tt, "//*/table[@class = 'listForm resultList']//a")))
  if (length(subs) == 0) {
    if (verbose)
      message('Substance not found! Returing NA. \n')
    return(NA)
  }
  type <- clean_char(xml_text(xml_find_all(tt, "//*/table[@class = 'listForm resultList']/tr/td[2]")))
  links <- xml_attr(xml_find_all(tt, "//*/table[@class = 'listForm resultList']//a"), 'href')

  if (!'ETOX_NAME' %in% type) {
    warning('No ETOX_NAME found. Return match only for synonyms')
  }

  # match query with substance, get link
  ulinks <- unique(links)
  ename <- subs[type == 'ETOX_NAME']

  # multiple hits
  if (length(ulinks) > 1) {
    if (verbose)
      message("More then one Link found. \n")
    if (mult == 'na') {
      if (verbose)
        message("Returning NA. \n")
      id <- NA
      matched_sub <- NA
      d <- NA
    }
    if (mult == 'all') {
      if (verbose)
        message("Returning all matches. \n")
      id <- gsub('^.*\\?id=(.*)', '\\1', ulinks)
      matched_sub <- ename[sapply(id, function(x) grep(x, ename)[1])]
      d <- 'all'
    }
    if (mult == 'first') {
      if (verbose)
        message("Returning first match. \n")
      id <- gsub('^.*\\?id=(.*)', '\\1', ulinks[1])
      matched_sub <- ename[grep(id[1], ename)[1]]
      d <- 'first'
    }
    if (mult == 'best') {
      if (verbose)
        message("Returning best match. \n")
      dd <- adist(query, subs) / nchar(subs)
      id <- gsub('^.*\\?id=(.*)', '\\1', links[which.min(dd)])
      d <- dd[which.min(dd)]
      matched_sub <- subs[which.min(dd)]
    }
    if (mult == 'ask') {
      tochoose <- data.frame(match = subs, match_type = type)
      print(tochoose)
      message("\nEnter rownumber of compounds (other inputs will return 'NA'):\n") # prompt
      take <- as.numeric(scan(n = 1, quiet = TRUE))
      if (length(take) == 0) {
        id <- NA
        matched_sub <- NA
        d <- NA
      }
      if (take %in% seq_len(nrow(tochoose))) {
        id <- gsub('^.*\\?id=(.*)', '\\1', links[take])
        matched_sub <- subs[take]
        d <- 'interactive'
      }
    }
  } else {
    id <- gsub('^.*\\?id=(.*)', '\\1', unique(links))
    d <- 'direct match'
    matched_sub <- subs[1]
  }

  # return object
  names(id) <- NULL
  attr(id, "matched") <- matched_sub
  attr(id, "distance") <- d
  return(id)
}



#' Get basic information from a ETOX ID
#'
#' Query ETOX: Information System Ecotoxicology and Environmental Quality Targets
#' \url{https://webetox.uba.de/webETOX/index.do} for basic information
#'
#' @import xml2
#' @importFrom rvest html_table
#' @importFrom stats rgamma
#'
#' @param id character; ETOX ID
#' @param verbose logical; print message during processing to console?
#'
#' @return a list wit four entries: cas (the CAS numbers), ec (the EC number),
#' gsbl (the gsbl number), a data.frame synonys with synonyms and the source url.
#'
#' @note Before using this function, please read the disclaimer
#' \url{https://webetox.uba.de/webETOX/disclaimer.do}.
#'
#' @seealso \code{\link{get_etoxid}} to retrieve ETOX IDs, \code{\link{etox_basic}} for basic information,
#' \code{\link{etox_targets}} for quality targets and \code{\link{etox_tests}} for test results
#'
#' @author Eduard Szoecs, \email{eduardszoecs@@gmail.com}
#' @export
#' @examples
#' \dontrun{
#' id <- get_etoxid('Triclosan', mult = 'best')
#' etox_basic(id)
#'
#' # Retrieve CAS for multiple inputs
#' ids <- c("20179", "9051")
#' sapply(ids, function(y) etox_basic(y)$cas)
#' }
etox_basic <- function(id, verbose = TRUE){
  if (length(id) > 1) {
    stop('Cannot handle multiple input strings.')
  }
  if (is.na(id)) {
    message('ID is NA! Returning NA.\n')
    return(NA)
  }
  # id <- '20179'
  baseurl <- 'https://webetox.uba.de/webETOX/public/basics/stoff.do?language=en&id='
  qurl <- paste0(baseurl, id)
  if (verbose)
    message('Querying ', qurl)
  Sys.sleep( rgamma(1, shape = 15, scale = 1/10))
  tt <- try(read_html(qurl), silent = TRUE)
  if (inherits(tt, 'try-error')) {
    message('ID not found! Returning NA.\n')
    return(NA)
  }
  tabs <- html_table(tt, fill = TRUE)
  binf <- tabs[[length(tabs)]]
  cas <- binf[, 1][binf[, 2] == 'CAS']
  ec <- binf[, 1][grepl('EINEC', binf[, 2])]
  gsbl <- binf[, 1][binf[, 2] == 'GSBL']

  syns <- tabs[[2]][c(1, 3, 4)]
  colnames(syns) <- syns[1, ]
  syns <- syns[-1, ]
  syns <- syns[syns[ , 2] == 'SYNONYM' & !is.na(syns[ , 2]), ]
  syns <- syns[ , -2]
  names(syns) <- c('name', 'language')

  out <- list(cas = cas, ec = ec, gsbl = gsbl, synonyms = syns,
              source_url = qurl)
  return(out)
}


#' Get Quality Targets from a ETOX ID
#'
#' Query ETOX: Information System Ecotoxicology and Environmental Quality Targets
#' \url{https://webetox.uba.de/webETOX/index.do} for quality targets
#'
#' @import xml2 RCurl
#' @importFrom utils read.table
#' @importFrom stats rgamma
#'
#' @param id character; ETOX ID
#' @param verbose logical; print message during processing to console?
#'
#' @return A list of two: \code{res} a data.frame with quality targets from the ETOX database, and source_url.
#'
#' @note Before using this function, please read the disclaimer
#' \url{https://webetox.uba.de/webETOX/disclaimer.do}.
#' @seealso \code{\link{get_etoxid}} to retrieve ETOX IDs, \code{\link{etox_basic}} for basic information,
#' \code{\link{etox_targets}} for quality targets and \code{\link{etox_tests}} for test results
#'
#' @author Eduard Szoecs, \email{eduardszoecs@@gmail.com}
#' @export
#' @examples
#' \dontrun{
#' id <- get_etoxid('Triclosan', mult = 'best')
#' out <- etox_targets(id)
#' out[ , c('Substance', 'CAS_NO', 'Country_or_Region', 'Designation',
#' 'Value_Target_LR', 'Unit')]
#' # Retrieve MAC-EQA for Germany for multiple inputs
#' ids <- c("20179", "9051")
#' sapply(ids, function(y) {
#'   res <- etox_targets(y)
#'   if (length(res) == 1) {
#'     out <- res
#'   } else {
#'     out <- res[res$Country_or_Region == 'DEU' & res$Designation == 'MAC-EQS', c('Value_Target_LR')]
#'   }
#'   return(out)
#' }
#' )
#' }
etox_targets <- function(id, verbose = TRUE){
  if (length(id) > 1) {
    stop('Cannot handle multiple input strings.')
  }
  if (is.na(id)) {
    message('ID is NA! Returning NA.\n')
    return(NA)
  }
  # id <- '20179'
  # id <- '9051'
  baseurl <- 'https://webetox.uba.de/webETOX/public/basics/stoff.do?language=en&id='
  qurl <- paste0(baseurl, id)
  if (verbose)
    message('Querying ', qurl)
  Sys.sleep( rgamma(1, shape = 15, scale = 1/10))
  tt <- try(read_html(qurl), silent = TRUE)
  if (inherits(tt, 'try-error')) {
    message('ID not found! Returning NA.\n')
    return(NA)
  }
  link2 <- xml_attrs(xml_find_all(tt, "//a[contains(.,'Quali') and contains(@href,'stoff')]"), 'href')
  id2 <- gsub('.*=(\\d+)', '\\1', link2)

  tt2 <-  read_html(paste0('https://webetox.uba.de', link2, '&language=en'))
  mssg <- xml_text(xml_find_all(tt2, "//div[contains(@class, 'messages')]/ul/li/span[contains(@class, 'message')]"))
  if (length(mssg) > 0) {
    if (grepl('no result', mssg)) {
      message('No targets found found! Returning NA.\n')
      return(NA)
    } else {
      message('Problem found! Message: \n ', mssg)
    }
  }

  csvlink <- xml_attr(xml_find_all(tt2, "//a[contains(.,'Csv')]"), 'href')
  res <- read.table(paste0('https://webetox.uba.de', csvlink),
                    header = TRUE, sep = ',', dec = ',', fileEncoding = 'latin1',
                    stringsAsFactors = FALSE)
  res$Value_Target_LR <- as.numeric(res$Value_Target_LR)
  source_url <- paste0('https://webetox.uba.de', link2, '&language=en')
  source_url <- gsub('^(.*ziel\\.do)(.*)(\\?stoff=.*)$', '\\1\\3', source_url)
  out <- list(res = res, source_url = source_url)

  return(out)
}


#' Get Tests from a ETOX ID
#'
#' Query ETOX: Information System Ecotoxicology and Environmental Quality Targets
#' \url{https://webetox.uba.de/webETOX/index.do} for tests
#'
#' @import xml2 RCurl
#' @importFrom utils read.table
#' @importFrom stats rgamma
#'
#' @param id character; ETOX ID
#' @param verbose logical; print message during processing to console?
#'
#' @return A list of two: A data.frame with test results from the ETOX database and the source_url.
#' @note Before using this function, please read the disclaimer
#' \url{https://webetox.uba.de/webETOX/disclaimer.do}.
#'
#' @seealso \code{\link{get_etoxid}} to retrieve ETOX IDs, \code{\link{etox_basic}} for basic information,
#' \code{\link{etox_targets}} for quality targets and \code{\link{etox_tests}} for test results
#'
#' @author Eduard Szoecs, \email{eduardszoecs@@gmail.com}
#' @export
#' @examples
#' \dontrun{
#' id <- get_etoxid('Triclosan', mult = 'best')
#' out <- etox_tests(id)
#' out[ , c('Organism', 'Effect', 'Duration', 'Time_Unit',
#' 'Endpoint', 'Value', 'Unit')]
#' }
etox_tests <- function(id, verbose = TRUE){
  if (length(id) > 1) {
    stop('Cannot handle multiple input strings.')
  }
  if (is.na(id)) {
    message('ID is NA! Returning NA.\n')
    return(NA)
  }
  # id <- '20179'
  baseurl <- 'https://webetox.uba.de/webETOX/public/basics/stoff.do?id='
  qurl <- paste0(baseurl, id)
  if (verbose)
    message('Querying ', qurl)
  Sys.sleep( rgamma(1, shape = 15, scale = 1/10))
  tt <- try(read_html(qurl), silent = TRUE)
  if (inherits(tt, 'try-error')) {
    message('ID not found! Returning NA.\n')
    return(NA)
  }
  link2 <- xml_attrs(xml_find_all(tt, "//a[contains(.,'Tests') and contains(@href,'stoff')]"), 'href')
  id2 <- gsub('.*=(\\d+)', '\\1', link2)

  tt2 <-  read_html(paste0('https://webetox.uba.de', link2, '&language=en'))
  csvlink <- xml_attr(xml_find_all(tt2, "//a[contains(.,'Csv')]"), 'href')

  # csvlink <- gsub('^(.*\\.do).*$', '\\1', csvlink)
  res <- read.table(paste0('https://webetox.uba.de', csvlink),
                    header = TRUE, sep = ',', dec = ',', fileEncoding = 'latin1',
                    stringsAsFactors = FALSE)
  res$Value <- as.numeric(res$Value)

  source_url <- paste0('https://webetox.uba.de', link2, '&language=en')
  source_url <- gsub('^(.*test\\.do)(.*)(\\?stoff=.*)$', '\\1\\3', source_url)
  out <- list(res = res, source_url = source_url)

  return(out)
}