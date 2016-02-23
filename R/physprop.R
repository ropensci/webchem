#' Query SRC PHYSPROP Database
#'
#' Query SRCs PHYSPROP Database. The PHYSPROP database contains chemical structures,
#' names and physical properties for over 41,000 chemicals.
#' Physical properties collected from a wide variety of sources include experimental,
#' extrapolated and estimated values. For more information
#' see \url{http://www.srcinc.com/what-we-do/environmental/scientific-databases.html#physprop}.
#'
#' @import xml2 httr
#' @importFrom stats rgamma
#' @importFrom stats setNames
#'
#' @param cas character; A CAS number to query.
#' @param verbose logical; print message during processing to console?
#'
#' @return A list of lists with 5 entries: cas (CAS-Number), cname (Chemical Name),
#' mw (Molecular weigth), prop (Properties) and source url.
#' prop is a data.frame, with variables, value, unit, temp, type (see note) and ref (see note).
#'
#' @note Abbreviations in the 'Type' field: EXP = Experimental Data,
#' EST = Estimated Data, EXT = Extrapolated Data.
#' Please respect the terms of use: \url{http://www.srcinc.com/terms-of-use.html}.
#'
#' @author Eduard Szoecs, \email{eduardszoecs@@gmail.com}
#' @export
#' @examples
#' \dontrun{
#' pp_query('50-00-0')
#' out <- pp_query(c('50-00-0', '79622-59-6', 'xxxxx'))
#' out
#'
#' # extract lop-P
#'sapply(out, function(y){
#'  if (length(y) == 1 && is.na(y))
#'    return(NA)
#'  y$prop$value[y$prop$variable == 'Log P (octanol-water)']
#'  })
#' }

pp_query <- function(cas, verbose = TRUE){
  # cas <- c('50-00-0', '79622-59-6', 'xxxxx')
  foo <- function(cas, verbose) {
    query <- gsub('-', '', cas)
    baseurl <- 'http://esc.syrres.com/fatepointer/webprop.asp?CAS='
    qurl <- paste0(baseurl, query)
    if (verbose)
      message('Querying ', qurl)
    Sys.sleep( rgamma(1, shape = 5, scale = 1/10))
    ttt <- try(
      read_html(
        qurl,    # for http
        # content(
        #   GET(qurl, config = config( ssl_verifypeer = 0L, ssl_verifyhost = 0L)),
        #   as = 'text'
        #   ),
        encoding = "UTF-8"), silent = TRUE)
    if (inherits(ttt, 'try-error')) {
      warning('Cannot retrive data from server. \n Returning NA.')
      return(NA)
    }


    if (grepl('No records', xml_text(xml_find_all(ttt, '//p'))[3])) {
      message('Not found! Returning NA.\n')
      return(NA)
    }

    variables <- xml_text(xml_find_all(ttt, '//ul/following-sibling::text()[1]'))
    variables <- gsub(':', '', variables)

    nd <- xml_find_all(ttt, '//ul[@class!="ph"]')
    prop <- data.frame(t(sapply(nd, function(y) {
      value_var <- xml_text(xml_find_all(y, './li[starts-with(text(),"Value")]'))
      value_var <- gsub('Value.:.(.*)', '\\1', value_var)
      value <- gsub('^(\\d*\\.?\\d*).*', '\\1', value_var)
      unit <- gsub('^\\d*\\.?\\d*.(.*)', '\\1', value_var)
      temp <- xml_text(xml_find_all(y, './li[starts-with(text(),"Temp")]'))
      temp <- gsub('Temp.*:.(.*)', '\\1', temp)
      if (length(temp) == 0) {
        temp <- NA
      }
      type <- xml_text(xml_find_all(y,  './li[starts-with(text(),"Type")]'))
      type <- gsub('Type.*:.(.*)', '\\1', type)
      if (length(type) == 0) {
        type <- NA
      }
      ref <- xml_text(xml_find_all(y, './li[starts-with(text(),"Ref")]'))
      ref <- gsub('Ref.*:.(.*)', '\\1', ref)
      if (length(ref) == 0) {
        ref <- NA
      }
      c(value, unit, temp, type, ref)
    })), stringsAsFactors = FALSE)
    names(prop) <- c("value", "unit", "temp", "type", "ref")
    prop$variable <- variables
    prop <- prop[, c("variable", "value", "unit", "temp", "type", "ref")]
    prop[ , 'value'] <-  as.numeric(prop[ , 'value'])

    cas <- xml_text(xml_find_all(ttt, '//ul[@class="ph"]/li[starts-with(text(),"CAS")]'))
    cas <- sub(".*:.", "", cas)
    cas <- sub("^[0]+", "", cas)

    cname <- xml_text(xml_find_all(ttt, '//ul[@class="ph"]/li[starts-with(text(),"Chem")]'))
    cname <- sub(".*:.", "", cname)

    mw <- xml_text(xml_find_all(ttt, "//ul[@class='ph']/li[4]"))
    mw <- as.numeric(sub(".*:.", "", mw))

    mp <- xml_text(xml_find_all(ttt, "//ul[@class='ph']/li[5]"))
    prop <- rbind(prop, data.frame(variable = 'Melting Point',
                                   value = extr_num(mp),
                                   unit = 'deg C',
                                   temp = NA,
                                   type = NA,
                                   ref = NA))
    bp <- xml_text(xml_find_all(ttt, "//ul[@class='ph']/li[6]"))
    prop <- rbind(prop, data.frame(variable = 'Boiling Point',
                                   value = extr_num(bp),
                                   unit = 'deg C',
                                   temp = NA,
                                   type = NA,
                                   ref = NA))

    out <- list(cas = cas, cname = cname, mw = mw, prop = prop, source_url = qurl)
    return(out)
  }
  out <- lapply(cas, foo,verbose = verbose)
  out <- setNames(out, cas)
  return(out)
}


