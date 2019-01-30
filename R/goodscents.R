#' Scrape \url{thegoodsscentscompany.com} for aromas of compounds
#'
#' @import xml2
#' @import rvest
#'
#' @param CAS character; CAS number to search by. See \code{\link{is.cas}} for
#' correct formatting.
#' @param odor logical; TRUE by default, if TRUE, returns odor data from web
#' @param flavor logical; FALSE by default, if TRUE, returns flavor data from web
#' @param ... Currently not used
#'
#' @return Returns a list of character vectors the found odors and/or flavors
#' or an NA if not found.
#'
#' @author Josh Morimoto, \email{joshua.morimoto@@tufts.edu}
#'
#' @examples
#' \donttest{
#' tgsc_percept('66-25-1')
#' }
#' @export



tgsc_percept = function(CAS,
                        odor = TRUE,
                        flavor = FALSE,
                        ...) {
  foo = function(CAS, odor, flavor) {
    # Base url for goodscents
    search.url = 'http://www.thegoodscentscompany.com/search3.php?qName='
    if (!is.na(CAS)) {
      # Paste CAS into url and read that page
      stink.url = read_html(paste0(search.url, CAS)) #%>%
      # Find the relevant data block in the page
      stink.block = html_nodes(stink.url, '.lstw11') #%>%
      # Read text
      stink = html_text(stink.block)
      out = c(NULL, NULL)
      # If odor checked, return first block
      if (odor) {
        out[1] = stink[1]
      }
      # If flavor checked, return second block
      if (flavor) {
        out[2] = stink[2]
      }
      # If only flavor, just return flavor element
      if (!odor & flavor) {
        out = out[2]
      }
      return(out)
    }
    else {
      return(NA)
    }
  }
  out = lapply(CAS, foo, odor = odor, flavor = flavor)
  return(out)
}
