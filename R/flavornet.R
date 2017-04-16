#' Retrieve flavor percepts from www.flavornet.org
#'
#' Retreive flavor percepts from \url{http://www.flavornet.org}.  Flavornet is a database of 738 compounds with odors
#' perceptible to humans detected using gas chromatography ofactometry (GCO).
#'
#' @import xml2
#' @importFrom stats rgamma
#'
#' @param CAS character; CAS number to search by. See \code{\link{is.cas}} for correct formatting
#' @param verbose logical; should a verbose output be printed on the console?
#' @param ... not currently used
#'
#' @return A named character vector containing flavor percepts or NA's in the case of CAS numbers that are not found
#'
#' @author Eric Scott, \email{eric.scott@@tufts.edu}
#'
#' @examples
#' \donttest{
#' # might fail if website is not available
#' fn_percept("123-32-0")
#'
#' CASs <- c("75-07-0",  "64-17-5",  "109-66-0", "78-94-4",  "78-93-3")
#' fn_percept(CASs)
#' }
#' @export

fn_percept <- function(CAS, verbose = TRUE, ...)
{
  foo <- function (CAS, verbose){
    qurl = paste0("http://www.flavornet.org/info/",CAS,".html")
    if (verbose)
      message(qurl)
    Sys.sleep(rgamma(1, shape = 10, scale = 1/10))
    h <- try(read_html(qurl), silent = TRUE)
    if (inherits(h, "try-error")) {
      warning("CAS not found... Returning NA.")
      return(NA)
    }
    doc.text = xml_text(xml_find_all(h, "/html/body/p[3]"))
    pattern = "Percepts: "
    percept <- gsub(pattern, "", doc.text)
    return(percept)
  }
  percepts <- sapply(CAS, foo, verbose = verbose)
  percepts <- setNames(percepts, CAS)
  return(percepts)
}