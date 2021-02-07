#' Query http://www.alanwood.net/pesticides/
#'
#' Query Alan Woods Compendium of Pesticide Common Names
#' \url{http://www.alanwood.net/pesticides/}
#' @import xml2
#'
#' @param  query character; search string
#' @param from character; type of input ('cas' or 'name')
#' @param verbose logical; print message during processing to console?
#' @param ... additional arguments to internal utility functions
#' @param type deprecated
#' @return A list of eight entries: common-name, status, preferred IUPAC Name,
#' IUPAC Name, cas, formula, activity, subactivity, inchikey, inchi and source
#' url.
#' @note for from = 'cas' only the first matched link is returned.
#' Please respect Copyright, Terms and Conditions
#' \url{http://www.alanwood.net/pesticides/legal.html}!
#' @references Eduard Szöcs, Tamás Stirling, Eric R. Scott, Andreas Scharmüller,
#' Ralf B. Schäfer (2020). webchem: An R Package to Retrieve Chemical
#' Information from the Web. Journal of Statistical Software, 93(13).
#' \doi{10.18637/jss.v093.i13}.
#' @export
#' @examples
#' \dontrun{
#' aw_query('Fluazinam', from = 'name')
#' out <- aw_query(c('Fluazinam', 'Diclofop'), from = 'name')
#' out
#' # extract subactivity from object
#' sapply(out, function(y) y$subactivity[1])
#'
#' # use CAS-numbers
#' aw_query("79622-59-6", from = 'cas')
#' }
aw_query <- function(query, from = c("name", "cas"), verbose = TRUE,
                     type, ...) {

  if (!ping_service("aw")) stop(webchem_message("service_down"))

  if (!missing(type)) {
    message('"type" is deprecated. Please use "from" instead. ')
    from <- type
  }

  if ("commonname" %in% from) {
    message('To search by compound name use "name" instead of "commonname"')
    from <- "name"
  }
  from <- match.arg(from)
  aw_idx <- build_aw_idx(verbose, ...)

  foo <- function(query, from, verbose) {
    if (is.na(query)) {
      if (verbose) webchem_message("na")
      return(NA)
    }
    if (verbose) webchem_message("query", query, appendLF = FALSE)
  # search links in indexes
    if (from == "name") {
      links <- aw_idx$links[aw_idx$source == "cn"]
      names <- aw_idx$linknames[aw_idx$source == "cn"]
      cname <-  query
    }

    if (from == "cas") {
      names <- aw_idx$names[aw_idx$source == "rn"]
      # select only first link
      links <- aw_idx$links[aw_idx$source == "rn"]
      linknames <- aw_idx$linknames[aw_idx$source == "rn"]
      cname <-  linknames[tolower(names) == tolower(query)]
    }

    takelink <- links[tolower(names) == tolower(query)]
    if (length(takelink) == 0) {
      if (verbose) message("Not found.")
      return(NA)
    }
    if (length(takelink) > 1) {
      takelink <- unique(takelink)
      if (length(takelink) > 1) {
        message("More then one link found. Returning first.")
        takelink <- takelink[1]
      }
    }

    qurl <- paste0("http://www.alanwood.net/pesticides/", takelink)
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
    if (res$status_code == 200){
      ttt <- read_html(res)
      status <- xml_text(
        xml_find_all(ttt, "//tr/th[@id='r1']/following-sibling::td"))
      pref_iupac_name <- xml_text(
        xml_find_all(ttt, "//tr/th[@id='r2']/following-sibling::td"))
      iupac_name <- xml_text(
        xml_find_all(ttt, "//tr/th[@id='r3']/following-sibling::td"))
      cas <- xml_text(
        xml_find_all(ttt, "//tr/th[@id='r5']/following-sibling::td"))
      formula <- xml_text(
        xml_find_all(ttt, "//tr/th[@id='r6']/following-sibling::td"))
      activity <- xml_text(
        xml_find_all(ttt, "//tr/th[@id='r7']/following-sibling::td"))
      subactivity <- trimws(
        strsplit(gsub("^.*\\((.*)\\)", "\\1", activity), ";")[[1]])
      activity <- gsub("^(.*) \\(.*\\)", "\\1", activity)
      inchikey_r <- xml_text(
        xml_find_all(ttt, "//tr/th[@id='r11']/following-sibling::td"))
      if (length(inchikey_r) == 0) {
        inchikey <- NA
      } else {
        if (grepl("isomer", inchikey_r)) {
          inchikey <- c(
            s_isomer = gsub(
              ".*\\(S\\)-isomer:(.*)(minor component.*)", "\\1", inchikey_r),
            r_isomer = gsub(".*\\(R\\)-isomer:(.*)", "\\1", inchikey_r))
        }
        if (grepl("identifier", inchikey_r)) {
          inchikey <- c(gsub("(.*)identifier.*", "\\1", inchikey_r),
                        gsub(".*identifier.*:(.*)", "\\1", inchikey_r))
          names(inchikey) <- c("inchikey",
                               gsub(".*(identifier.*:).*", "\\1", inchikey_r)
          )
        }
        if (!grepl("isomer", inchikey_r) & !grepl("identifier", inchikey_r))
          inchikey <- inchikey_r
      }

      inchi <- xml_text(
        xml_find_all(ttt, "//tr/th[@id='r12']/following-sibling::td"))
      if (length(inchi) == 0) {
        inchi <- NA
      } else {
        if (grepl("isomer", inchi)) {
          inchi <- c(s_isomer = gsub(".*\\(S\\)-isomer:(.*)(minor component.*)",
                                     "\\1", inchi),
                     r_isomer = gsub(".*\\(R\\)-isomer:(.*)", "\\1", inchi))
        }
      }
      # add source url
      source_url <- paste0("http://www.alanwood.net/pesticides/", takelink)
      out <- list(cname = cname, status = status,
                  pref_iupac_name = pref_iupac_name, iupac_name = iupac_name,
                  cas = cas, formula = formula, activity = activity,
                  subactivity = subactivity, inchikey = inchikey, inchi = inchi,
                  source_url = source_url)
      return(out)
    }
    else {
      return(NA)
    }
  }
  out <- lapply(query, function(x) foo(x, from = from, verbose = verbose))
  names(out) <- query
  class(out) <- c("aw_query", "list")
  return(out)
}

#' Function to build index
#'
#' This function builds an index of Alan Woods Compendium of Pesticides
#' \url{http://www.alanwood.net/pesticides/} and saves it to
#' \code{\link{tempdir}}.
#' @import xml2
#'
#' @param verbose logical; print message during processing to console?
#' @param force_build logical; force building a new index?
#' @return a data.frame
#' @seealso \code{\link{aw_query}}, \code{\link{tempdir}}
#' @source \url{http://www.alanwood.net/pesticides/}
#' @noRd
build_aw_idx <- function(verbose = TRUE, force_build = FALSE) {
  if (!ping_service("aw")) stop(webchem_message("service_down"))
  suppressWarnings(try(load(paste0(tempdir(), "/data/aw_idx.rda")),
                       silent = TRUE))
  if (!file.exists(paste0(tempdir(), "/data/aw_idx.rda")) |
      force_build |
      try(Sys.Date() - attr(aw_idx, "date"), silent = TRUE) > 30) {
    if (!dir.exists(paste0(tempdir(), "/data"))) {
      dir.create(paste0(tempdir(), "/data"))
    }
    if (verbose) message("Building index. ", appendLF = FALSE)
    res <- try(httr::RETRY("GET",
                           "http://www.alanwood.net/pesticides/index_rn.html",
                           httr::user_agent(webchem_url()),
                           terminate_on = 404,
                           quiet = TRUE), silent= TRUE)
    if (inherits(res, "try-error")) {
      if (verbose) webchem_message("service_down")
      return(NA)
    }
    if (verbose) message(httr::message_for_status(res))
    if (res$status_code == 200){
      idx1 <- read_html("http://www.alanwood.net/pesticides/index_rn.html")
      prep_idx <- function(y) {
        names <- xml_text(xml_find_all(y, "//dl/dt"))
        links <- xml_attr(
          xml_find_all(y, "//dt/following-sibling::dd[1]/a[1]"), "href")
        linknames <- xml_text(
          xml_find_all(y, "//dt/following-sibling::dd[1]/a[1]"))
        return(data.frame(names, links, linknames, stringsAsFactors = FALSE))
      }
      aw_idx <- rbind(prep_idx(idx1))
      aw_idx[["source"]] <- "rn"
      idx4 <- read_html("http://www.alanwood.net/pesticides/index_cn.html")
      n <- xml_find_all(idx4, "//a")
      names <- xml_text(n)
      rm <- names == ""
      names <- names[!rm]
      links <- xml_attr(n, "href")
      links <- links[!rm]
      idx4 <- data.frame(names = NA, links = links, linknames = names,
                         source = "cn", stringsAsFactors = FALSE)
      aw_idx <- rbind(aw_idx, idx4)

      # fix encoding
      ln <- aw_idx$linknames
      Encoding(ln) <- "latin1"
      ln <- iconv(ln, from = "latin1", to = "ASCII", sub = "")
      aw_idx$linknames <- ln
      attr(aw_idx, "date") <- Sys.Date()
      save(aw_idx, file = paste0(tempdir(), "/data/aw_idx.rda"))
    }
  }
  return(aw_idx)
}
