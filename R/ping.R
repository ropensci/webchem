#' Ping an API used in webchem to see if it's working.
#'
#' @name ping
#' @param ... Curl options passed on to \code{\link[httr]{GET}} or \code{\link[httr]{POST}}
#' @return A logical, TRUE or FALSE
#'
#'

# pubchem -----------------------------------------------------------------
#' @import httr
#' @rdname ping
#' @return TRUE if pubchem is reachable
#' @export
#' @examples
#' \dontrun{
#'  # might fail if API is not available
#'  ping_pubchem()
#'  }
ping_pubchem <- function(...) {
  query = 'Aspirin'
  from = 'name'
  prolog <- 'https://pubchem.ncbi.nlm.nih.gov/rest/pug'
  input <- paste0('/compound/', from)
  output <- '/synonyms/JSON'
  qurl <- paste0(prolog, input, output)

  res <- POST(qurl, body = paste0(from, '=', query), ...)
  stopifnot(is(res, "response"))
  res$status_code == 200
}


# Chemspider webpage -----------------------------------------------------------
#' @import httr
#' @rdname ping
#' @return TRUE if chemspider is reachable
#' @export
#' @examples
#' \dontrun{
#'  # might fail if API is not available
#'  ping_cs()
#'  }
ping_cs <- function(...) {
  res <- GET('https://www.chemspider.com/Chemical-Structure.5363.html', ...)
  stopifnot(is(res, "response"))
  res$status_code == 200
}




# PAN ---------------------------------------------------------------------
#' @import httr
#' @rdname ping
#' @return TRUE if PAN is reachable
#' @export
#' @examples
#' \dontrun{
#'  # might fail if API is not available
#'  ping_pan()
#'  }
ping_pan <- function(...) {
  res <- try(GET('http://www.pesticideinfo.org/List_Chemicals.jsp?', timeout(1)))
  if (inherits(res, 'try-error'))
    return(FALSE)
  res$status_code == 200
}
