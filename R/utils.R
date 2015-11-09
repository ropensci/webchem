#' Check if input is a valid inchikey
#'
#' @param x character; input string
#' @return a logical
#'
#' @export
#' @examples
#' is.inchikey('BQJCRHHNABKAKU-KBQPJGBKSA-N')
#' is.inchikey('BQJCRHHNABKAKU-KBQPJGBKS')
is.inchikey = function(x) {
  (x == toupper(x)) &
    (nchar(x) == 27) &
    (substr(x, 15, 15) == "-") &
    (substr(x, 26, 26) == "-")
}


#' Extract number from string
#' @param x character; input string
#' @return a numeric vector
#' @export
#' @examples
#' extr_num('aaaa -95')
extr_num <- function(x) {
  as.numeric(gsub("[^0-9\\-]+", "", x))
}



#' Check if input is a valid CAS
#'
#' @import stringr
#' @param x character; input strin
#' @param verbose logical; print message during processing to console?g
#' @return a logical
#'
#' @export
#' @examples
#' is.cas('64-17-5')
is.cas = function(x, verbose = TRUE) {
  x <- '64-17-5'

  # cas must have two hyphens
  nsep <- str_count(x, '-')
  if (nsep != 2) {
    if (verbose)
      message('Less than 2 hyphens in string.')
    return(FALSE)
  }

  # first part up to 7 digits
  fi <- gsub('^(.*)-(.*)-(.*)$', '\\1', x)
  if (nchar(fi) < 7) {
    if (verbose)
      message('First part with more than 7 digits!')
    return(FALSE)
  }

  # second part must be two digits
  se <- gsub('^(.*)-(.*)-(.*)$', '\\2', x)
  if (nchar(se) != 2) {
    if (verbose)
      message('Second part has not two digits!')
    return(FALSE)
  }

  # third part (checksum) must be 1 digit
  th <- gsub('^(.*)-(.*)-(.*)$', '\\3', x)
  if (nchar(th) != 1) {
    if (verbose)
      message('Third part has not 1 digit!')
    return(FALSE)
  }

  # check checksum
  di <-  as.numeric(strsplit(gsub('^(.*)-(.*)-(.*)$', '\\1\\2', x), split = '')[[1]])
  checksum <- sum(rev(seq_along(di)) * di)
  if (checksum %% 10 != as.numeric(th)) {
    if (verbose)
      message('Checksum is not correct! ', checksum %% 10, ' vs. ', th)
    return(FALSE)
  }
  return(TRUE)
}
