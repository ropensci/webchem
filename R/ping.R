#' Ping an API used in webchem to see if it's working.
#'
#' @name ping
#' @param what (character) One of 'status' (default), 'content', or an HTTP status
#' code. If status, we just check that the HTTP status code is 200, or similar
#' signifying the service is up. If content, we do a simple, quick check to
#' determine if returned content matches what's expected. If an HTTP status
#' code, it must match an appropriate code. See \code{\link{status_codes}}.
#' @param ... Curl options passed on to \code{\link[httr]{GET}} or \code{\link[httr]{POST}}
#' @return A logical, TRUE or FALSE
#'
#'

# pubchem -----------------------------------------------------------------
#' @import httr
#' @rdname ping
#' @export
pubchem_ping <- function(what = "status", ...) {
  query = 'Aspirin'
  from = 'name'
  prolog <- 'https://pubchem.ncbi.nlm.nih.gov/rest/pug'
  input <- paste0('/compound/', from)
  output <- '/synonyms/JSON'
  qurl <- paste0(prolog, input, output)

  res <- POST(qurl, body = paste0(from, '=', query), ...)
  switch(matchwhat(what),
         status = match_status(res),
         code = match_code(res, what),
         content = stop("what = 'content' currently not supported")
  )
}




# Utility functions -------------------------------------------------------


matchwhat <- function(x){
  x <- as.character(x)
  if (x %in% c("status", "content"))
    x
  else
    "code"
}

match_status <- function(x){
  stopifnot(is(x, "response"))
  x$status_code == 200
}

match_code <- function(x, y){
  stopifnot(is(x, "response"))
  x$status_code == y
}