#' Query SRC PHYSPROP Database
#'
#' Query SRCs PHYSPROP Database. The PHYSPROP database contains chemical structures,
#' names and physical properties for over 41,000 chemicals.
#' Physical properties collected from a wide variety of sources include experimental,
#' extrapolated and estimated values. For more information
#' see \url{http://www.srcinc.com/what-we-do/environmental/scientific-databases.html#physprop}.
#'
#' @import XML

query <- '50-00-0'
query <- gsub('-', '', query)
baseurl <- 'http://esc.syrres.com/fatepointer/webprop.asp?CAS='

ttt <- htmlParse(getURL(paste0(baseurl, query)))

xpathSApply(ttt, '//ul[@class!="ph"]' , xmlValue)
