#' Get ETOX ID
#'
#' Query ETOX: Information System Ecotoxicology and Environmental Quality Targets
#' \url{http://webetox.uba.de/webETOX/index.do} for their substance ID
#'
#' @import XML RCurl
#'
#' @param  query character; The searchterm
#' @param verbose logical; print message during processing to console?
#'
#' @return A character vector with the substance ID and additional attributes \code{matched}  (the matched
#' substance name) and \code{distance} (the normalized string distance of the query to the match).
#'
#' @note If more than one reference is found only the first hit is taken.
#' Before using this function, please read the disclaimer \url{http://webetox.uba.de/webETOX/disclaimer.do}.
#'
#' @seealso \code{\link{get_etoxid}} to retrieve ETOX IDs, \code{\link{etox_basic}} for basic information,
#' \code{\link{etox_targets}} for quality targets and \code{\link{etox_tests}} for test results.
#'
#' @author Eduard Szoecs, \email{eduardszoecs@@gmail.com}
#' @export
#' @examples
#' \dontrun{
#' get_etoxid('Triclosan')
#' # multiple inpus
#' comps <- c('Triclosan', 'Glyphosate')
#' sapply(comps, get_etoxid)
#' }
get_etoxid <- function(query, verbose = TRUE){
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

  # query <- 'Triclosan'
  if (verbose)
    message('Searching ', query)
  baseurl <- 'http://webetox.uba.de/webETOX/public/search/stoff.do'

  Sys.sleep(0.1)
  out <- postForm(baseurl,
                  .params = list('stoffname.selection[0].name' = query,
                                 event = 'Search'))

  # get substances and links
  tt <- htmlParse(out)
  subs <- clean_char(xpathSApply(tt,"//*/table[@class = 'listForm resultList']//a",
                                 xmlValue))
  if (length(subs) == 0) {
    if (verbose)
      message('Substance not found! Returing NA. \n')
    return(NA)
  }
  type <- clean_char(xpathSApply(tt,"//*/table[@class = 'listForm resultList']/tr/td[2]",
                                 xmlValue))
  links <- xpathSApply(tt, "//*/table[@class = 'listForm resultList']//a//@href")

  # match query with substance, get link
  if (length(unique(links)) > 1) {
    if (verbose)
      message("More then one Link found. Returning best match. \n")
    dd <- adist(query, subs[type == 'ETOX_NAME']) / nchar(subs[type == 'ETOX_NAME'])
    takelink <- links[type == 'ETOX_NAME'][which.min(dd)]
    d <- dd[which.min(dd)]
    matched_sub <- subs[type == 'ETOX_NAME'][which.min(dd)]
  } else {
    takelink <- unique(links)
    d <- 0
    matched_sub <- subs[type == 'ETOX_NAME'][1]
  }
  id <- gsub('^.*\\?id=(.*)', '\\1', takelink)
  names(id) <- NULL
  attr(id, "matched") <- matched_sub
  attr(id, "distance") <- d
  return(id)
}

#' Get basic information from a ETOX ID
#'
#' Query ETOX: Information System Ecotoxicology and Environmental Quality Targets
#' \url{http://webetox.uba.de/webETOX/index.do} for basic information
#'
#' @import XML RCurl
#'
#' @param id character; ETOX ID
#' @param verbose logical; print message during processing to console?
#'
#' @return a list wit four entries: cas (the CAS numbers), ec (the EC number),
#' gsbl (the gsbl number) and a data.frame synonys with synonyms.
#'
#' @note Before using this function, please read the disclaimer
#' \url{http://webetox.uba.de/webETOX/disclaimer.do}.
#'
#' @seealso \code{\link{get_etoxid}} to retrieve ETOX IDs, \code{\link{etox_basic}} for basic information,
#' \code{\link{etox_targets}} for quality targets and \code{\link{etox_tests}} for test results
#'
#' @author Eduard Szoecs, \email{eduardszoecs@@gmail.com}
#' @export
#' @examples
#' \dontrun{
#' id <- get_etoxid('Triclosan')
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
  # id <- '20179'
  baseurl <- 'http://webetox.uba.de/webETOX/public/basics/stoff.do?id='
  qurl <- paste0(baseurl, id)
  httpheader = c("Accept-Language" = "en-US,en;q=0.5")
  if (verbose)
    message('Querying ', qurl)

  Sys.sleep(0.1)
  tt <- htmlParse(getURL(qurl, httpheader = httpheader))

  if (any(grepl('Problem', xpathSApply(tt, '//h3', xmlValue)))) {
    message('ID not found! Returning NA.\n')
    return(NA)
  }

  tabs <- readHTMLTable(tt, header = FALSE, stringsAsFactors = FALSE)

  binf <- tabs[[3]]
  cas <- binf[, 1][binf[, 2] == 'CAS']
  ec <- binf[, 1][grepl('EINEC', binf[, 2])]
  gsbl <- binf[, 1][binf[, 2] == 'GSBL']

  syns <- tabs[[2]][c(1, 3, 4)]
  colnames(syns) <- syns[1, ]
  syns <- syns[-1, ]
  syns <- syns[syns[ , 2] == 'SYNONYM' & !is.na(syns[ , 2]), ]
  syns <- syns[ , -2]
  names(syns) <- c('name', 'language')

  out <- list(cas = cas, ec = ec, gsbl = gsbl, synonyms = syns)
  return(out)
}


#' Get Quality Targets from a ETOX ID
#'
#' Query ETOX: Information System Ecotoxicology and Environmental Quality Targets
#' \url{http://webetox.uba.de/webETOX/index.do} for quality targets
#'
#' @import XML RCurl
#' @importFrom utils read.table
#'
#' @param id character; ETOX ID
#' @param verbose logical; print message during processing to console?
#'
#' @return A data.frame with quality targets from the ETOX database.
#'
#' @note Before using this function, please read the disclaimer
#' \url{http://webetox.uba.de/webETOX/disclaimer.do}.
#' @seealso \code{\link{get_etoxid}} to retrieve ETOX IDs, \code{\link{etox_basic}} for basic information,
#' \code{\link{etox_targets}} for quality targets and \code{\link{etox_tests}} for test results
#'
#' @author Eduard Szoecs, \email{eduardszoecs@@gmail.com}
#' @export
#' @examples
#' \dontrun{
#' id <- get_etoxid('Triclosan')
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
  # id <- '20179'
  # id <- '9051
  baseurl <- 'http://webetox.uba.de/webETOX/public/basics/stoff.do?id='
  qurl <- paste0(baseurl, id)
  httpheader = c("Accept-Language" = "en-US,en;q=0.5")
  if (verbose)
    message('Querying ', qurl)

  # get second id
  Sys.sleep(0.1)
  tt <- htmlParse(getURL(qurl, httpheader = httpheader))
  if (any(grepl('Problem', xpathSApply(tt, '//h3', xmlValue)))) {
    message('ID not found! Returning NA.\n')
    return(NA)
  }
  link2 <- xpathSApply(tt, "//a[contains(.,'Quali')]/@href[contains(.,'stoff')]")
  id2 <- gsub('.*=(\\d+)', '\\1', link2)

  tt2 <-  htmlParse(getURL(paste0('http://webetox.uba.de', link2),
                           httpheader = httpheader))
  mssg <- xpathSApply(tt2, "//div[contains(@class, 'messages')]/ul/li/span[contains(@class, 'message')]", xmlValue)
  if (length(mssg) > 0) {
    if (grepl('no result', mssg)) {
      message('No targets found found! Returning NA.\n')
      return(NA)
    } else {
      message('Problem found! Message: \n ', mssg)
    }
  }


  csvlink <- xpathSApply(tt2, "//a[contains(.,'Csv')]/@href")
  cont <- getURL(paste0('http://webetox.uba.de', csvlink),
                 httpheader = httpheader)
  out <- read.table(text = cont, header = TRUE, sep = ',', dec = ',',
                    stringsAsFactors = FALSE)
  out$Value_Target_LR <- as.numeric(out$Value_Target_LR)
  return(out)
}

#' Get Tests from a ETOX ID
#'
#' Query ETOX: Information System Ecotoxicology and Environmental Quality Targets
#' \url{http://webetox.uba.de/webETOX/index.do} for tests
#'
#' @import XML RCurl
#' @importFrom utils read.table
#'
#' @param id character; ETOX ID
#' @param verbose logical; print message during processing to console?
#'
#' @return A data.frame with test results from the ETOX database.
#' @note Before using this function, please read the disclaimer
#' \url{http://webetox.uba.de/webETOX/disclaimer.do}.
#'
#' @seealso \code{\link{get_etoxid}} to retrieve ETOX IDs, \code{\link{etox_basic}} for basic information,
#' \code{\link{etox_targets}} for quality targets and \code{\link{etox_tests}} for test results
#'
#' @author Eduard Szoecs, \email{eduardszoecs@@gmail.com}
#' @export
#' @examples
#' \dontrun{
#' id <- get_etoxid('Triclosan')
#' out <- etox_tests(id)
#' out[ , c('Organism', 'Effect', 'Duration', 'Time_Unit',
#' 'Endpoint', 'Value', 'Unit')]
#' }
etox_tests <- function(id, verbose = TRUE){
  if (length(id) > 1) {
    stop('Cannot handle multiple input strings.')
  }
  # id <- '20179'
  baseurl <- 'http://webetox.uba.de/webETOX/public/basics/stoff.do?id='
  qurl <- paste0(baseurl, id)
  httpheader = c("Accept-Language" = "en-US,en;q=0.5")
  if (verbose)
    message('Querying ', qurl)

  # get second id
  Sys.sleep(0.1)
  tt <- htmlParse(getURL(qurl, httpheader = httpheader))
  if (any(grepl('Problem', xpathSApply(tt, '//h3', xmlValue)))) {
    message('ID not found! Returning NA.\n')
    return(NA)
  }
  link2 <- xpathSApply(tt, "//a[contains(.,'Test')]/@href[contains(.,'stoff')]")
  id2 <- gsub('.*=(\\d+)', '\\1', link2)

  tt2 <-  htmlParse(getURL(paste0('http://webetox.uba.de', link2),
                           httpheader = httpheader))
  csvlink <- xpathSApply(tt2, "//a[contains(.,'Csv')]/@href")
  cont <- getURL(paste0('http://webetox.uba.de', csvlink),
                 httpheader = httpheader)
  out <- read.table(text = cont, header = TRUE, sep = ',', dec = ',',
                    stringsAsFactors = FALSE)
  out$Value <- as.numeric(out$Value)
  return(out)
}