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


#' Extract a number from a string
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
#' @description This function checks if a string is a valid CAS registry number.
#' A valid CAS is 1) separated by two hyphes into three parts; 2) the first part
#' consists from two up to seven digits; 3) the second of two digits; 4) the third
#' of one digit (check digit); 5) the check digits corresponds the checksum.
#' The checksum is found by taking the last digit (excluding the check digit) multiplyingit with 1,
#' the second last multiplied with 2, the third-last multiplied with 3 etc.
#' The modulo 10 of the sum of these is the checksum.
#'
#' @import stringr
#' @param x character; input strin
#' @param verbose logical; print message during processing to console?g
#' @return a logical
#'
#' @export
#' @examples
#' is.cas('64-17-5')
#' is.cas('64175')
#' is.cas('4-17-5')
#' is.cas('64-177-6')
#' is.cas('64-17-55')
#' is.cas('64-17-6')
is.cas = function(x, verbose = TRUE) {
  # x <- '64-17-5'

  # cas must have two hyphens
  nsep <- str_count(x, '-')
  if (nsep != 2) {
    if (verbose)
      message('Less than 2 hyphens in string.')
    return(FALSE)
  }

  # first part 2 to 7 digits
  fi <- gsub('^(.*)-(.*)-(.*)$', '\\1', x)
  if (nchar(fi) > 7 | nchar(fi) < 2) {
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
