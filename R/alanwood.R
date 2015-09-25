#' Query http://www.alanwood.net/pesticides
#'
#' Query Alan Woods Compendium of Pesticide Common Names http://www.alanwood.net/pesticides
#' @import XML RCurl
#'
#' @param  x character; search string
#' @param type character; type of input
#' @param verbose logical; print message during processing to console?
#' @return A list of eight entries: common-name, status, preferredd IUPAC Name,
#'          IUPAC Name, cas, formula, activity, inchikey, inchi
#'
#' @note for type = 'cas' only the first link is returned
#' @author Eduard Szoecs, \email{eduardszoecs@@gmail.com}
#' @export
#' @examples
#' \dontrun{
#' alanwood('Fluazinam', type = 'commonname')
#' sapply(c('Fluazinam', 'Diclofop', 'xxxxx'), alanwood, type = 'com')
#' alanwood("79622-59-6", type = 'cas')
#' }
alanwood <- function(x, type = c("commonname", "cas"), verbose = TRUE){
  if (length(x) > 1) {
    stop('Cannot handle multiple input strings.')
  }
  type <- match.arg(type)
  if (type == 'commonname') {
    baseurl <- 'http://www.alanwood.net/pesticides/index_cn.html'
    ttt <- htmlParse(getURL(baseurl))
    names <- xpathSApply(ttt, "//a" , xmlValue)
    names <- names[!names == '']
    links <- xpathSApply(ttt,"//a//@href")
    cname <-  x
  }
  if (type == 'cas') {
    baseurl0 <- 'http://www.alanwood.net/pesticides/index_rn.html'
    ttt0 <- htmlParse(getURL(baseurl0))
    names0 <- xpathSApply(ttt0, "//dl/dt" , xmlValue)
    # select only first link
    links0 <- xpathSApply(ttt0, '//dt/following-sibling::dd[1]/a[1]/@href')
    linkn0 <- xpathSApply(ttt0, '//dt/following-sibling::dd[1]/a[1]', xmlValue)

    baseurl1 <- 'http://www.alanwood.net/pesticides/index_rn1.html'
    ttt1 <- htmlParse(getURL(baseurl1))
    names1 <- xpathSApply(ttt1, "//dt" , xmlValue)
    links1 <- xpathSApply(ttt1, '//dt/following-sibling::dd[1]/a[1]/@href')
    linkn1 <- xpathSApply(ttt1, '//dt/following-sibling::dd[1]/a[1]', xmlValue)

    baseurl2 <- 'http://www.alanwood.net/pesticides/index_rn2.html'
    ttt2 <- htmlParse(getURL(baseurl2))
    names2 <- xpathSApply(ttt2, "//dt" , xmlValue)
    links2 <- xpathSApply(ttt2, '//dt/following-sibling::dd[1]/a[1]/@href')
    linkn2 <- xpathSApply(ttt2, '//dt/following-sibling::dd[1]/a[1]', xmlValue)

    names <- c(names0, names1, names2)
    links <- c(links0, links1, links2)
    linknames <- c(linkn0, linkn1, linkn2)
    cname <-  linknames[tolower(names) == tolower(x)]
  }
  takelink <- links[tolower(names) == tolower(x)]
  if (length(takelink) == 0) {
    message('Not found! Returning NA.\n')
    return(NA)
  }
  if (verbose)
    message('Querying ', takelink)

  Sys.sleep(0.3)
  ttt <- htmlParse(getURL(paste0('http://www.alanwood.net/pesticides/', takelink)))
  status <- xpathSApply(ttt, "//tr/th[@id='r1']/following-sibling::td", xmlValue)
  pref_iupac_name <- xpathSApply(ttt, "//tr/th[@id='r2']/following-sibling::td", xmlValue)
  iupac_name <- xpathSApply(ttt, "//tr/th[@id='r3']/following-sibling::td", xmlValue)
  cas <- xpathSApply(ttt, "//tr/th[@id='r5']/following-sibling::td", xmlValue)
  formula <- xpathSApply(ttt, "//tr/th[@id='r6']/following-sibling::td", xmlValue)
  activity <- xpathSApply(ttt, "//tr/th[@id='r7']/following-sibling::td", xmlValue)
  inchikey <- xpathSApply(ttt, "//tr/th[@id='r11']/following-sibling::td", xmlValue)
  inchi <- xpathSApply(ttt, "//tr/th[@id='r12']/following-sibling::td", xmlValue)
  out <- list(cname = cname, status = status, pref_iupac_name = pref_iupac_name,
              iupac_name = iupac_name, cas = cas, formula = formula,
              activity = activity, inchikey = inchikey, inch = inchi)
  return(out)
}
