#' Get ETOX ID
#'
#' Query ETOX: Information System Ecotoxicology and Environmental Quality
#' Targets \url{https://webetox.uba.de/webETOX/index.do} for their substance ID
#'
#' @import xml2 httr
#' @importFrom dplyr bind_rows
#' @importFrom tibble tibble
#' @param query character; The searchterm
#' @param from character; Type of input, can be one of "name" (chemical name),
#' "cas" (CAS Number), "ec" (European Community number for regulatory purposes),
#' "gsbl" (Identifier used by \url{https://www.chemikalieninfo.de/}) and "rtecs"
#' (Identifier used by the Registry of Toxic Effects of Chemical Substances
#' database).
#' @param match character; How should multiple hits be handeled? "all" returns
#' all matched IDs, "first" only the first match, "best" the best matching (by
#' name) ID, "ask" is a interactive mode and the user is asked for input, "na"
#' returns \code{NA} if multiple hits are found.
#' @param verbose logical; print message during processing to console?
#' @return a tibble with 3 columns: the query, the match, and the etoxID
#' @note Before using this function, please read the disclaimer
#' \url{https://webetox.uba.de/webETOX/disclaimer.do}.
#' @seealso \code{\link{etox_basic}} for basic information,
#' \code{\link{etox_targets}} for quality targets and
#' \code{\link{etox_tests}} for test results.
#' @references Eduard Szöcs, Tamás Stirling, Eric R. Scott, Andreas Scharmüller,
#' Ralf B. Schäfer (2020). webchem: An R Package to Retrieve Chemical
#' Information from the Web. Journal of Statistical Software, 93(13).
#' \doi{10.18637/jss.v093.i13}.
#' @export
#' @examples
#' \dontrun{
#' # might fail if API is not available
#' get_etoxid("Triclosan")
#' # multiple inputs
#' comps <- c("Triclosan", "Glyphosate")
#' get_etoxid(comps)
#' get_etoxid(comps, match = "all")
#' get_etoxid("34123-59-6", from = "cas") # Isoproturon
#' get_etoxid("133483", from = "gsbl") # 3-Butin-1-ol
#' get_etoxid("203-157-5", from = "ec") # Paracetamol
#' }
get_etoxid <- function(query,
                       from = c("name", "cas", "ec", "gsbl", "rtecs"),
                       match = c("all", "best", "first", "ask", "na"),
                       verbose = getOption("verbose")) {

  if (!ping_service("etox")) stop(webchem_message("service_down"))

  clean_char <- function(x) {
    # rm \n \t
    x <- gsub("\n | \t", "", x)
    # rm leading / trailling whitespace
    x <- gsub("^\\s+|\\s+$", "", x)
    # replace multiple spaces by one,
    x <- gsub("(?<=[\\s])\\s*|^\\s+$", "", x, perl = TRUE)
    return(x)
  }
  # checks
  from <- match.arg(from)
  match <- match.arg(match)
  if (from != "name" & match == "best") {
    warning("match = 'best' only makes sense when querying chemical names. ")
  }
  foo <- function(query, from, match, verbose) {
    if (is.na(query)) {
      if (verbose) webchem_message("na")
      return(tibble("query" = query, "match" = NA, "etoxid" = NA))
    }
    if(verbose) webchem_message("query", query, appendLF = FALSE)
    baseurl <- "https://webetox.uba.de/webETOX/public/search/stoff.do"
    if (from == 'name') {
      body <- list("stoffname.selection[0].name" = query,
                   "stoffname.selection[0].type" = "",
                   event = "Search")
    } else {
      from_look <- c(cas = 69,
                     ec = 70,
                     rtecs = 72,
                     gsbl = 73)
      type <- from_look[ names(from_look) == from ]
      body <- list('stoffnummer.selection[0].name' = query,
                   'stoffnummer.selection[0].type' = type,
                   event = "Search")
    }
    webchem_sleep(type = 'scrape')
    h <- try(httr::RETRY("POST",
                         url = baseurl,
                         httr::user_agent(webchem_url()),
                         handle = handle(''),
                         body = body,
                         terminate_on = 404,
                         quiet = TRUE), silent = TRUE)
    if (inherits(h, "try-error")) {
      if (verbose) webchem_message("service_down")
      return(tibble::tibble("query" = query, "match" = NA, "etoxid" = NA))
    }
    if (verbose) message(httr::message_for_status(h))
    if (h$status_code == 200) {
      tt <- read_html(h)
      subs <- clean_char(xml_text(xml_find_all(
        tt, "//*/table[@class = 'listForm resultList']//a")))[-1]
      if (length(subs) == 0) {
        if (verbose) webchem_message("not_found")
        hit <- tibble("query" = query,
                      "match" = NA,
                      "etoxid" = NA)
        return(hit)
      } else {

        links <- xml_attr(xml_find_all(
          tt, "//*/table[@class = 'listForm resultList']//a"), "href")[-1]

        subs_names <- gsub(" \\(.*\\)", "", subs)
        id <- gsub("^.*\\?id=(.*)", "\\1", links)

        out <- matcher(id, query = query, result = subs_names, from = from, match = match)

        hit <- tibble("query" = query,
                      "match" = names(out),
                      "etoxid" = out)
        return(hit)
      }
    }
    else {
      return(tibble::tibble(query = NA, match = NA, etoxid = NA))
    }
  }
  out <- lapply(query, foo, from = from, match = match, verbose = verbose)
  out <- dplyr::bind_rows(out)
  return(out)
}

#' Get basic information from a ETOX ID
#'
#' Query ETOX: Information System Ecotoxicology and Environmental Quality
#' Targets \url{https://webetox.uba.de/webETOX/index.do} for basic information
#'
#' @import xml2
#' @importFrom rvest html_table
#' @param id character; ETOX ID
#' @param verbose logical; print message during processing to console?
#'
#' @return a list with lists of four entries: cas (the CAS numbers), ec (the EC
#'   number), gsbl (the gsbl number), a data.frame synonys with synonyms and the
#'   source url.
#'
#' @note Before using this function, please read the disclaimer
#'   \url{https://webetox.uba.de/webETOX/disclaimer.do}.
#'
#' @seealso \code{\link{get_etoxid}} to retrieve ETOX IDs,
#'   \code{\link{etox_basic}} for basic information, \code{\link{etox_targets}}
#'   for quality targets and \code{\link{etox_tests}} for test results
#' @references Eduard Szöcs, Tamás Stirling, Eric R. Scott, Andreas Scharmüller,
#' Ralf B. Schäfer (2020). webchem: An R Package to Retrieve Chemical
#' Information from the Web. Journal of Statistical Software, 93(13).
#' \doi{10.18637/jss.v093.i13}.
#' @export
#' @examples
#' \dontrun{
#' id <- get_etoxid('Triclosan', match = 'best')
#' etox_basic(id$etoxid)
#'
#' # Retrieve data for multiple inputs
#' ids <- c("20179", "9051")
#' out <- etox_basic(ids)
#' out
#'
#' # extract cas numbers
#' sapply(out, function(y) y$cas)
#' }
etox_basic <- function(id, verbose = getOption("verbose")) {

  if (!ping_service("etox")) stop(webchem_message("service_down"))

  foo <- function(id, verbose) {
    if (is.na(id)) {
      if (verbose) webchem_message("na")
      return(NA)
    }
    baseurl <- 'https://webetox.uba.de/webETOX/public/basics/stoff.do?language=en&id='
    qurl <- paste0(baseurl, id)
    if(verbose) webchem_message("query", id, appendLF = FALSE)
    webchem_sleep(type = 'scrape')
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
    if (res$status_code == 200) {
      tt <- try(read_html(res), silent = TRUE)
      if (inherits(tt, 'try-error')) {
        if (verbose) webchem_message("not_found")
        return(NA)
      }
      tabs <- try(suppressWarnings(html_table(tt, fill = TRUE)), silent = TRUE)
      if (inherits(tabs, 'try-error')) {
        webchem_message("not_found")
        return(NA)
      }
      binf <- tabs[[length(tabs)]]
      cas <- binf[, 1][binf[, 2] == 'CAS']
      ec <- binf[, 1][grepl('^EC$|EINEC', binf[, 2])]
      gsbl <- binf[, 1][binf[, 2] == 'GSBL']

      syns <- tabs[[2]][c(1, 3, 4)]
      colnames(syns) <- as.character(syns[1, ])
      syns <- syns[-1, ]
      syns <- syns[syns[[2]] == 'SYNONYM' & !is.na(syns[[2]]), ] #syns[[2]] or syns$ETOX_NAME?
      syns <- syns[ , -2]
      names(syns) <- c('name', 'language')

      out <- list(cas = cas, ec = ec, gsbl = gsbl, synonyms = syns,
                  source_url = qurl)
      return(out)

      # CODE FOR A POSSIBLE FUTURE RELEASE
      # binf <- tabs[[length(tabs)]]
      # cas <- binf[, 1][binf[, 2] == 'CAS']
      # ec <- binf[, 1][grepl('^EC$|EINEC', binf[, 2])]
      # gsbl <- binf[, 1][binf[, 2] == 'GSBL']
      #
      # syns <- tabs[[2]][c(1, 3, 4)]
      # names(syns) <- tolower(gsub('\\s+', '_', names(syns)))
      # group <- tolower(syns[ syns$substance_name_typ == 'GROUP_USE' &
      #                          syns$language == 'English', ]$notation)
      # syn <- syns[ syns$substance_name_typ == 'SYNONYM', ]
      # syn <- syn[ ,-2]
      # names(syn) <- c('name', 'language')
      # # return list of data.frames
      # l <- list(cas = cas,
      #           ec = ec,
      #           gsbl = gsbl,
      #           source_url = qurl)
      # data <- as.data.frame(t(do.call(rbind, l)),
      #                       stringsAsFactors = FALSE)
      # chem_group <- as.data.frame(t(group), stringsAsFactors = FALSE)
      # names(chem_group) <- chem_group[1, ]
      # chem_group[1, ] <- TRUE
      # out <- list(data = data,
      #             chemical_group = chem_group,
      #             synonyms = syn)
      ### END
    }
    else {
      return(NA)
    }
    }
  out <- lapply(id, foo, verbose = verbose)
  names(out) <- id
  class(out) <- c('etox_basic', 'list')
  return(out)
}


#' Get Quality Targets from a ETOX ID
#'
#' Query ETOX: Information System Ecotoxicology and Environmental Quality
#' Targets \url{https://webetox.uba.de/webETOX/index.do} for quality targets
#'
#' @import xml2
#' @importFrom utils read.table
#' @param id character; ETOX ID
#' @param verbose logical; print message during processing to console?
#'
#' @return A list of lists of two: \code{res} a data.frame with quality targets
#'   from the ETOX database, and source_url.
#'
#' @note Before using this function, please read the disclaimer
#'   \url{https://webetox.uba.de/webETOX/disclaimer.do}.
#' @seealso \code{\link{get_etoxid}} to retrieve ETOX IDs,
#'   \code{\link{etox_basic}} for basic information, \code{\link{etox_targets}}
#'   for quality targets and \code{\link{etox_tests}} for test results
#' @references Eduard Szöcs, Tamás Stirling, Eric R. Scott, Andreas Scharmüller,
#' Ralf B. Schäfer (2020). webchem: An R Package to Retrieve Chemical
#' Information from the Web. Journal of Statistical Software, 93(13).
#' \doi{10.18637/jss.v093.i13}.
#' @export
#' @examples
#' \dontrun{
#' id <- get_etoxid('Triclosan', match = 'best')
#' out <- etox_targets(id$etoxid)
#' out[ , c('Substance', 'CAS_NO', 'Country_or_Region', 'Designation',
#' 'Value_Target_LR', 'Unit')]
#' etox_targets( c("20179", "9051"))
#'
#' }
etox_targets <- function(id, verbose = getOption("verbose")) {

  if (!ping_service("etox")) stop(webchem_message("service_down"))

  foo <- function(id, verbose) {
    if (is.na(id)) {
      if (verbose) webchem_message("na")
      return(NA)
    }
    baseurl <- 'https://webetox.uba.de/webETOX/public/basics/stoff.do?language=en&id='
    qurl <- paste0(baseurl, id)
    if(verbose) webchem_message("query", id, appendLF = FALSE)
    webchem_sleep(type = 'scrape')
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
    if(res$status_code == 200){
      tt <- try(read_html(res), silent = TRUE)
      if (inherits(tt, 'try-error')) {
        if (verbose) webchem_message("not_found")
        return(NA)
      }
      link2 <-
        xml_attrs(xml_find_all(tt, "//a[contains(.,'Quali') and contains(@href,'stoff')]"),
                  'href')
      id2 <- gsub('.*=(\\d+)', '\\1', link2)

      tt2 <-  read_html(paste0('https://webetox.uba.de', link2, '&language=en'))
      mssg <-
        xml_text(
          xml_find_all(
            tt2,
            "//div[contains(@class, 'messages')]/ul/li/span[contains(@class, 'message')]"
          ),
          trim = TRUE
        )
      if (length(mssg) > 0) {
        if (grepl('no result', mssg)) {
          if (verbose) webchem_message("not_found")
          return(NA)
        } else {
          if (verbose) message(paste0(" Problem found. Message: ", mssg))
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
    else {
      return(NA)
    }
  }
  out <- lapply(id, foo, verbose = verbose)
  names(out) <- id
  return(out)
}


#' Get Tests from a ETOX ID
#'
#' Query ETOX: Information System Ecotoxicology and Environmental Quality Targets
#' \url{https://webetox.uba.de/webETOX/index.do} for tests
#'
#' @import xml2
#' @importFrom utils read.table
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
#' @export
#' @examples
#' \dontrun{
#' id <- get_etoxid('Triclosan', match = 'best')
#' out <- etox_tests(id$etoxid)
#' out[ , c('Organism', 'Effect', 'Duration', 'Time_Unit',
#' 'Endpoint', 'Value', 'Unit')]
#' etox_tests( c("20179", "9051"))
#' }
etox_tests <- function(id, verbose = getOption("verbose")) {

  if (!ping_service("etox")) stop(webchem_message("service_down"))

  foo <- function(id, verbose){
    if (is.na(id)) {
      if (verbose) webchem_message("na")
      return(NA)
    }
    baseurl <- 'https://webetox.uba.de/webETOX/public/basics/stoff.do?id='
    qurl <- paste0(baseurl, id)
    if(verbose) webchem_message("query", id, appendLF = FALSE)
    webchem_sleep(type = 'scrape')
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
    if (res$status_code == 200) {
      tt <- try(read_html(res), silent = TRUE)
      if (inherits(tt, 'try-error')) {
        if (verbose) webchem_message("not_found")
        return(NA)
      }
      link2 <- xml_attrs(xml_find_all(tt, "//a[contains(.,'Tests') and contains(@href,'stoff')]"), 'href')
      id2 <- gsub('.*=(\\d+)', '\\1', link2)
      tt2 <-  read_html(paste0('https://webetox.uba.de', link2, '&language=en'))
      mssg <- xml_text(xml_find_all(tt2, "//div[contains(@class, 'messages')]/ul/li/span[contains(@class, 'message')]"), trim = TRUE)
      if (length(mssg) > 0) {
        if (grepl('no result', mssg)) {
          if (verbose) message(" No targets found. Returning NA.")
          return(NA)
        } else {
          if (verbose) message(paste0(" Problem found. Message: ", mssg),
                               appendLF = FALSE)
        }
      }
      csvlink <- xml_attr(xml_find_all(tt2, "//a[contains(.,'Csv')]"), 'href')

      # csvlink <- gsub('^(.*\\.do).*$', '\\1', csvlink)
      res2 <- read.table(paste0('https://webetox.uba.de', csvlink),
                         header = TRUE, sep = ',', dec = ',',
                         fileEncoding = 'latin1',
                         stringsAsFactors = FALSE)
      res2$Value <- as.numeric(res2$Value)

      source_url <- paste0('https://webetox.uba.de', link2, '&language=en')
      source_url <- gsub('^(.*test\\.do)(.*)(\\?stoff=.*)$', '\\1\\3', source_url)
      out <- list(res = res2, source_url = source_url)
      return(out)
    }
    else {
      return(NA)
    }
  }
  out <- lapply(id, foo, verbose = verbose)
  names(out) <- id
  return(out)
}
