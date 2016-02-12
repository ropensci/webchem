#' Query http://www.alanwood.net/pesticides
#'
#' Query Alan Woods Compendium of Pesticide Common Names http://www.alanwood.net/pesticides
#' @import xml2
#' @importFrom stats rgamma
#'
#' @param  x character; search string
#' @param type character; type of input ('cas' or 'commonname')
#' @param verbose logical; print message during processing to console?
#' @return A list of eight entries: common-name, status, preferredd IUPAC Name,
#'          IUPAC Name, cas, formula, activity, subactivity, inchikey, inchi and source url.
#'
#' @note for type = 'cas' only the first matched link is returned.
#' Please respect Copyright, Terms and Conditions \url{http://www.alanwood.net/pesticides/legal.html}!
#' @author Eduard Szoecs, \email{eduardszoecs@@gmail.com}
#' @export
#' @examples
#' \dontrun{
#' aw_query('Fluazinam', type = 'commonname')
#' sapply(c('Fluazinam', 'Diclofop'), alanwood, type = 'com')
#' aw_query("79622-59-6", type = 'cas')
#' }
aw_query <- function(x, type = c("commonname", "cas"), verbose = TRUE){
  # x <- 'Fluazinam'
  # x <- "79622-59-6"
  # x <- '12071-83-9'
  # x <- '91465-08-6'
  # x <- "S-Metolachlor"
  if (length(x) > 1) {
    stop('Cannot handle multiple input strings.')
  }
  type <- match.arg(type)
  # search links in indexes
  if (type == 'commonname') {
    baseurl <- 'http://www.alanwood.net/pesticides/index_cn.html'
    ttt <- read_html(baseurl)
    n <- xml_find_all(ttt, '//a')
    names <- xml_text(n)
    rm <- names == ''
    names <- names[!rm]
    links <- xml_attr(n, 'href')
    links <- links[!rm]
    cname <-  x
  }

  if (type == 'cas') {
    if (!is.cas(x)){
      message('Input is not a CAS number! Returning NA.\n')
      return(NA)
    }
    f <- as.numeric(gsub('^(\\d*)-\\d*-\\d*', '\\1', x))
    if (f < 10000) {
      baseurl <- 'http://www.alanwood.net/pesticides/index_rn.html'
    } else if (f > 10000 & f < 60000) {
      baseurl <- 'http://www.alanwood.net/pesticides/index_rn1.html'
    } else {
      baseurl <- 'http://www.alanwood.net/pesticides/index_rn2.html'
    }

    Sys.sleep( rgamma(1, shape = 15, scale = 1/10))
    ttt <- read_html(baseurl)

    names <- xml_text(xml_find_all(ttt, "//dl/dt"))
    # select only first link
    links <- xml_attr(xml_find_all(ttt, '//dt/following-sibling::dd[1]/a[1]'), 'href')
    linknames <- xml_text(xml_find_all(ttt, '//dt/following-sibling::dd[1]/a[1]'))

    cname <-  linknames[tolower(names) == tolower(x)]
  }

  takelink <- links[tolower(names) == tolower(x)]
  if (length(takelink) == 0) {
    message('Not found! Returning NA.\n')
    return(NA)
  }
  if (length(takelink) > 1) {
    message('More then one link found! Returning first.\n')
    takelink <- takelink[1]
  }
  if (verbose)
    message('Querying ', takelink)

  Sys.sleep( rgamma(1, shape = 15, scale = 1/10))
  ttt <- read_html(paste0('http://www.alanwood.net/pesticides/', takelink))

  status <- xml_text(xml_find_all(ttt, "//tr/th[@id='r1']/following-sibling::td"))
  pref_iupac_name <- xml_text(xml_find_all(ttt, "//tr/th[@id='r2']/following-sibling::td"))
  iupac_name <- xml_text(xml_find_all(ttt, "//tr/th[@id='r3']/following-sibling::td"))
  cas <- xml_text(xml_find_all(ttt, "//tr/th[@id='r5']/following-sibling::td"))
  formula <- xml_text(xml_find_all(ttt, "//tr/th[@id='r6']/following-sibling::td"))
  activity <- xml_text(xml_find_all(ttt, "//tr/th[@id='r7']/following-sibling::td"))
  subactivity <- trimws(strsplit(gsub('^.*\\((.*)\\)', '\\1', activity), ';')[[1]])
  activity <- gsub('^(.*) \\(.*\\)', '\\1', activity)
  inchikey_r <- xml_text(xml_find_all(ttt, "//tr/th[@id='r11']/following-sibling::td"))
  if (length(inchikey_r) == 0) {
    inchikey <- NA
  } else {
    if (grepl('isomer', inchikey_r)) {
      inchikey <- c(s_isomer = gsub('.*\\(S\\)-isomer:(.*)(minor component.*)', '\\1', inchikey_r),
        r_isomer = gsub('.*\\(R\\)-isomer:(.*)', '\\1', inchikey_r))
    }
    if (grepl('identifier', inchikey_r)) {
      inchikey <- c(gsub('(.*)identifier.*', '\\1', inchikey_r), gsub('.*identifier.*:(.*)', '\\1', inchikey_r))
      names(inchikey) <- c('inchikey',
                           gsub('.*(identifier.*:).*', '\\1', inchikey_r)
                           )
    }
    if (!grepl('isomer', inchikey_r) & !grepl('identifier', inchikey_r))
      inchikey <- inchikey_r
  }

  inchi <- xml_text(xml_find_all(ttt, "//tr/th[@id='r12']/following-sibling::td"))
  if (length(inchi) == 0) {
    inchi <- NA
  } else {
    if (grepl('isomer', inchi)) {
      inchi <- c(s_isomer = gsub('.*\\(S\\)-isomer:(.*)(minor component.*)', '\\1', inchi),
                 r_isomer = gsub('.*\\(R\\)-isomer:(.*)', '\\1', inchi))
    }
  }

  # add source url
  source_url <- paste0('http://www.alanwood.net/pesticides/', takelink)
  out <- list(cname = cname, status = status, pref_iupac_name = pref_iupac_name,
              iupac_name = iupac_name, cas = cas, formula = formula,
              activity = activity, subactivity = subactivity,
              inchikey = inchikey, inch = inchi, source_url = source_url)
  return(out)
}
