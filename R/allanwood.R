#' Query http://www.alanwood.net/pesticides
#'
#' Query http://www.alanwood.net/pesticides
#' @import XML RCurl
#'
#' @param  x character; search string
#' @param type character; type of input
#' @param verbose logical; print message during processing to console?
#' @return A list of eight entries: status, preferredd IUPAC Name, IUPAC Name,
#'          cas, formula, activity, inchikey, inchi
#'
#' @author Eduard Szoecs, \email{eduardszoecs@@gmail.com}
#' @export
#' @examples
#' \dontrun{
#' allanwood('Fluazinam', type = 'commonname')
#' sapply(c('Fluazinam', 'Diclofop', 'xxxxx'), allanwood)
#' }
allanwood <- function(x, type = c("commonname", "cas"), verbose = TRUE){
  type <- match.arg(type)
  if (type == 'commonname') {
    baseurl <- 'http://www.alanwood.net/pesticides/index_cn.html'
    ttt <- htmlParse(getURL(baseurl))
    names <- xpathSApply(ttt, "//a" , xmlValue)
    names <- names[!names == '']
    links <- xpathSApply(ttt,"//a//@href")
    takelink <- links[tolower(names) == tolower(x)]
  } else {
    stop('currently not implemented!')
  }
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
  out <- list(status = status, pref_iupac_name = pref_iupac_name,
              iupac_name = iupac_name, cas = cas, formula = formula,
              activity = activity, inchikey = inchikey, inch = inchi)
  return(out)
}