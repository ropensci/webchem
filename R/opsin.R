#' OPSIN web interface
#'
#' Query the OPSIN  (Open Parser for Systematic IUPAC nomenclature) web service
#' \url{http://opsin.ch.cam.ac.uk/instructions.html}.
#'
#' @import jsonlite httr xml2
#' @import tibble
#' @importFrom dplyr select everything
#' @importFrom purrr map_dfr
#' @importFrom utils URLencode URLdecode
#' @importFrom httr RETRY user_agent message_for_status content
#' @param query character;  chemical name that should be queryed.
#' @param verbose logical; should a verbose output be printed on the console?
#' @param ... currently not used.
#' @return a tibble with six columnns: "query", inchi", "stdinchi", "stdinchikey", "smiles", "message", and "status"
#'
#' @references Lowe, D. M., Corbett, P. T., Murray-Rust, P., & Glen, R. C. (2011).
#' Chemical Name to Structure: OPSIN, an Open Source Solution. Journal of Chemical Information and Modeling,
#' 51(3), 739–753. \url{http://doi.org/10.1021/ci100384d}
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
    empty <- c(query, rep(NA, 6))
    names(empty) <- c("query", "inchi", "stdinchi", "stdinchikey", "smiles", "message", "status")
    empty <- as_tibble(t(empty))
    if (is.na(query)) {
      if (verbose) message(standard_string("na"))
      return(empty)
    }
    query_u <- URLencode(query, reserved = TRUE)
    baseurl <- "http://opsin.ch.cam.ac.uk/opsin/"
    out <- 'json'
    qurl <- paste0(baseurl, query_u, '.', out)
    if (verbose) message(standard_string("query", query), appendLF = FALSE)
    Sys.sleep( rgamma(1, shape = 5, scale = 1/10))
    res <- httr::RETRY("GET",
                       qurl,
                       user_agent(standard_string("webchem")),
                       terminate_on = 404,
                       quiet = TRUE)
    if (res$status_code == 200) {
      if (verbose) message(httr::message_for_status(res), appendLF = FALSE)
      cont <- content(res, as = 'text')
      if (substr(cont, 1, 14) == '<!DOCTYPE html') {
        cont <- read_html(cont)
        if (verbose) message(xml_text(xml_find_all(cont, '//h3')),
                             " Returning NA.")
        return(empty)
      }
      cont <- fromJSON(cont)
      cont[['cml']] <- NULL
      cont <- c(query = query, unlist(cont))
      cont <- tibble::as_tibble(t(cont))
      if (verbose) message("")
      return(cont)
    }
    else {
      if (verbose) message(httr::message_for_status(res))
      return(empty)
    }
  }
  out <- purrr::map_dfr(query, ~foo(.x, verbose = verbose))
  out <- dplyr::select(out, query, everything())
  class(out) <- c("opsin_query", class(out))
  return(out)
}
