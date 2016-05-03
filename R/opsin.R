#' OPSIN web interface
#'
#' Query the OPSIN  (Open Parser for Systematic IUPAC nomenclature) web service
#' \url{http://opsin.ch.cam.ac.uk/instructions.html}.
#'
#' @import jsonlite httr xml2
#' @param query character;  chemical name that should be queryed.
#' @param verbose logical; should a verbose output be printed on the console?
#' @param ... currently not used.
#' @return a data.frame with five columnns: "inchi", "stdinchi", "stdinchikey", "smiles", "message"
#'
#' @references Lowe, D. M., Corbett, P. T., Murray-Rust, P., & Glen, R. C. (2011).
#' Chemical Name to Structure: OPSIN, an Open Source Solution. Journal of Chemical Information and Modeling,
#' 51(3), 739–753. http://doi.org/10.1021/ci100384d
#' @examples
#' \donttest{
#' opsin_query('Cyclopropane')
#' opsin_query(c('Cyclopropane', 'Octane'))
#' opsin_query(c('Cyclopropane', 'Octane', 'xxxxx'))
#'}
#' @export

opsin_query <- function(query, verbose = TRUE, ...){
  # query <- 'cyclopropane'
  foo <- function(query, verbose){
    baseurl <- "http://opsin.ch.cam.ac.uk/opsin/"
    out <- 'json'
    qurl <- paste0(baseurl, query, '.', out)
    if (verbose)
      message('Querying ', query)
    Sys.sleep( rgamma(1, shape = 5, scale = 1/10))
    h <- try(GET(qurl), silent = TRUE)
    if (inherits(h, "try-error")) {
      warning('Problem with web service encountered... Returning NA.')
      return(rep(NA, 5))
    }
    cont <- content(h, as = 'text')
    if (substr(cont, 1, 14) == '<!DOCTYPE html') {
      cont <- read_html(cont)
      warning(xml_text(xml_find_all(cont, '//h3')), "\nReturning NA.")
      return(rep(NA, 5))
    }
    cont <- fromJSON(cont)
    cont[['cml']] <- NULL
    cont <- unlist(cont)
    return(cont)
  }
  out <- sapply(query, foo, verbose = verbose)
  out <- data.frame(t(out), stringsAsFactors = FALSE)
  out[['query']] <- rownames(out)
  class(out) <- c('data.frame', 'opsin_query')
  return(out)
}