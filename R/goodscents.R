#' Scrape \url{thegoodsscentscompany.com} for aromas of compounds
#'
#' @import xml2
#' @import rvest
#' 
#' @param CAS character; CAS number to search by. See \code{\link{is.cas}} for
#' correct formatting.
#' @param flavor boolean; FALSE by default, if TRUE, returns flavor instead of
#' odor
#' 
#' @return Returns a character value of the found aromas or an NA if not found.
#' 
#' @author Josh Morimoto, \email{joshua.morimoto@@tufts.edu}
#' 
#' @examples
#' \donttest{
#' tgsc_percept('66-25-1')
#' }
#' @export


tgsc_percept = function(CAS, flavor = FALSE) {
  # Base url for goodscents
  search.url = 'http://www.thegoodscentscompany.com/search3.php?qName='
  if (!is.na(CAS)) {
    # Paste CAS into url and read that page
    stink.url = read_html(paste0(search.url, CAS)) #%>%
    # Find the relevant data block in the page
    stink.block = html_nodes(stink.url, '.lstw11') #%>%
    # Read text
    stink = html_text(stink.block)
    if (flavor == TRUE) {
    return(stink[2])
    }
    else {
      return(stink[1])
    }
  }
  else {
    return(NA)
  }
}
