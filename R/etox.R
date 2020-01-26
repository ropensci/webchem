#' Get ETOX ID
#'
#' Query ETOX: Information System Ecotoxicology and Environmental Quality
#' Targets \url{https://webetox.uba.de/webETOX/index.do} for their substance ID
#'
#' @import xml2 httr
#' @importFrom stats rgamma
#' @importFrom dplyr bind_rows
#' @param query character; The searchterm
#' @param match character; How should multiple hits be handeled? "all" returns
#' all matched IDs, "first" only the first match, "best" the best matching (by
#' name) ID, "ask" is a interactive mode and the user is asked for input, "na"
#' returns NA if multiple hits are found.
#' @param verbose logical; print message during processing to console?
#' @return a dataframe with 4 columns: etoxID, matched substance, string
#' distance to match and the queried string
#' @note Before using this function, please read the disclaimer
#' \url{https://webetox.uba.de/webETOX/disclaimer.do}.
#' @seealso \code{\link{etox_basic}} for basic information,
#' \code{\link{etox_targets}} for quality targets and
#' \code{\link{etox_tests}} for test results.
#' @author Eduard Szoecs, \email{eduardszoecs@@gmail.com}
#' @author Tamas Stirling, \email{stirling.tamas@@gmail.com}
#' @export
#' @examples
#' \dontrun{
#' # might fail if API is not available
#' get_etoxid("Triclosan")
#' # multiple inputs
#' comps <- c("Triclosan", "Glyphosate")
#' get_etoxid(comps)
#' get_etoxid(comps, match = "all")
#' }
get_etoxid <- function(query,
                       match = c("best", "all", "first", "ask", "na"),
                       verbose = TRUE) {
  clean_char <- function(x) {
    # rm \n \t
    x <- gsub("\n | \t", "", x)
    # rm leading / trailling whitespace
    x <- gsub("^\\s+|\\s+$", "", x)
    # replace multiple spaces by one,
    x <- gsub("(?<=[\\s])\\s*|^\\s+$", "", x, perl = TRUE)
    return(x)
  }
  match <- match.arg(match)
  foo <- function(query, match, verbose) {
    if (verbose)
      message("Searching ", query)
    baseurl <- "https://webetox.uba.de/webETOX/public/search/stoff.do"
    Sys.sleep(rgamma(1, shape = 15, scale = 1/10))
    h <- POST(url = baseurl, body = list("stoffname.selection[0].name" = query,
                                    event = "Search"))
    # get substances and links
    tt <- read_html(h)
    subs <- clean_char(xml_text(xml_find_all(
      tt, "//*/table[@class = 'listForm resultList']//a")))[-1]
    if (length(subs) == 0) {
      if (verbose)
        message("Substance not found! Returning NA. \n")
      id <- NA
      matched_sub <- NA
      d <- NA
    }
    if (length(subs) > 0) {
      # types can be ETOX_NAME, SYNONYM
      # the function currently uses all types, there is no filtering
      type <- clean_char(xml_text(xml_find_all(
        tt, "//*/table[@class = 'listForm resultList']/tr/td[2]")))
      if (!'ETOX_NAME' %in% type) {
        warning('No ETOX_NAME found. Returning SYNONYM.')
      }
      links <- xml_attr(xml_find_all(
        tt, "//*/table[@class = 'listForm resultList']//a"), "href")[-1]
      }
      # multiple hits
      if (length(subs) == 1) {
        id <- gsub("^.*\\?id=(.*)", "\\1", links)
        d <- ifelse(match == "best", 0, as.character(0))
        matched_sub <- subs[1]
      }
      if (length(subs) > 1){
        if (verbose)
          message("More then one Link found. \n")
        if (match == "na") {
          if (verbose)
            message("Returning NA. \n")
          id <- NA
          matched_sub <- NA
          d <- "na"
        }
        if (match == "all") {
          if (verbose)
            message("Returning all matches. \n")
          id <- gsub("^.*\\?id=(.*)", "\\1", links)
          matched_sub <- subs[sapply(id, function(x) grep(x, subs)[1])]
          d <- "all"
        }
        if (match == "first") {
          if (verbose)
            message("Returning first match. \n")
          id <- gsub("^.*\\?id=(.*)", "\\1", links[1])
          matched_sub <- subs[grep(id[1], subs)[1]]
          d <- "first"
        }
        if (match == "best") {
          if (verbose)
            message("Returning best match. \n")
          msubs <- gsub(" \\(.*\\)", "", subs)
          dd <- adist(query, msubs) / nchar(msubs)
          id <- gsub("^.*\\?id=(.*)", "\\1", links[which.min(dd)])
          d <- round(dd[which.min(dd)], 2)
          matched_sub <- subs[which.min(dd)]
        }
        if (match == "ask") {
          matched_sub <- chooser(subs, "all")
          id <- gsub("^.*\\?id=(.*)", "\\1", links[which(subs == matched_sub)])
          d <- "interactive"
        }
      }
    # return object
    hit <- data.frame(
      "etoxid" = id,
      "match" = matched_sub,
      "distance" = d,
      "query" = query,
      stringsAsFactors = FALSE
    )
    return(hit)
  }
  out <- lapply(query, foo, match = match, verbose = verbose)
  out <- dplyr::bind_rows(out)
  return(out)
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
#' @return a list with lists of four entries: cas (the CAS numbers), ec (the EC number),
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
#' id <- get_etoxid('Triclosan', match = 'best')
#' etox_basic(id$etoxid)
#'
#' # Retrieve CAS for multiple inputs
#' ids <- c("20179", "9051")
#' out <- etox_basic(ids)
#' out
#'
#' # extract ec numbers
#' sapply(out, function(y) y$ec)
#' }
etox_basic <- function(id, verbose = TRUE) {
  if (!mode(id) %in% c("numeric","character")) {
    stop("id must be a vector!")
  }
  # id <- c("20179", "9051")
  foo <- function(id, verbose){
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
  out <- lapply(id, foo,verbose = verbose)
  out <- setNames(out, id)
  class(out) <- c('etox_basic','list')
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
#' @return A list of lists of two: \code{res} a data.frame with quality targets from the ETOX database, and source_url.
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
#' id <- get_etoxid('Triclosan', match = 'best')
#' out <- etox_targets(id)
#' out[ , c('Substance', 'CAS_NO', 'Country_or_Region', 'Designation',
#' 'Value_Target_LR', 'Unit')]
#' etox_targets( c("20179", "9051"))
#'
#' }
etox_targets <- function(id, verbose = TRUE) {
  if (!mode(id) %in% c("numeric","character")) {
    stop("id must be a vector!")
  }
  foo <- function(id, verbose){
    if (is.na(id)) {
      message('ID is NA! Returning NA.\n')
      return(NA)
    }
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
    mssg <- xml_text(xml_find_all(tt2, "//div[contains(@class, 'messages')]/ul/li/span[contains(@class, 'message')]"), trim = TRUE)
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
  out <- lapply(id, foo,verbose = verbose)
  out <- setNames(out, id)
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
#' @return A list of lists of two: A data.frame with test results from the ETOX database and the source_url.
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
#' id <- get_etoxid('Triclosan', match = 'best')
#' out <- etox_tests(id)
#' out[ , c('Organism', 'Effect', 'Duration', 'Time_Unit',
#' 'Endpoint', 'Value', 'Unit')]
#' etox_tests( c("20179", "9051"))
#' }
etox_tests <- function(id, verbose = TRUE) {
  if (!mode(id) %in% c("numeric","character")) {
    stop("id must be a vector!")
  }
  foo <- function(id, verbose){
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
    mssg <- xml_text(xml_find_all(tt2, "//div[contains(@class, 'messages')]/ul/li/span[contains(@class, 'message')]"), trim = TRUE)
    if (length(mssg) > 0) {
      if (grepl('no result', mssg)) {
        message('No targets found found! Returning NA.\n')
        return(NA)
      } else {
        message('Problem found! Message: \n ', mssg)
      }
    }
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
  out <- lapply(id, foo,verbose = verbose)
  out <- setNames(out, id)
  return(out)
}