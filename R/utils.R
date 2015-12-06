#' Check if input is a valid inchikey
#'
#' @description This function checks if a string is a valid inchikey.
#' Inchikey must fulfill the following criteria:
#' 1) consist of 27 characters;
#' 2) be all uppercase, all letters (no numbers);
#' 3) contain two hyphens at positions 15 and 26;
#' 4) 24th character (flag character) be 'S' (Standard InChI) or 'N' (non-standard)
#' 5) 25th character (version character) must be 'A' (currently).
#'
#' @param x character; input string
#' @param verbose logical; print messages during processing to console?
#' @return a logical
#'
#' @references Heller, Stephen R., et al. "InChI, the IUPAC International Chemical Identifier." Journal of Cheminformatics 7.1 (2015): 23.
#'
#' @author Eduard Szoecs, \email{eduardszoecs@@gmail.com}
#' @export
#' @examples
#' is.inchikey('BQJCRHHNABKAKU-KBQPJGBKSA-N')
#' is.inchikey('BQJCRHHNABKAKU-KBQPJGBKSA')
#' is.inchikey('BQJCRHHNABKAKU-KBQPJGBKSA-5')
#' is.inchikey('BQJCRHHNABKAKU-KBQPJGBKSA-n')
#' is.inchikey('BQJCRHHNABKAKU/KBQPJGBKSA/N')
#' is.inchikey('BQJCRHHNABKAKU-KBQPJGBKXA-N')
#' is.inchikey('BQJCRHHNABKAKU-KBQPJGBKSB-N')
is.inchikey = function(x, verbose = TRUE) {
  # x <- 'BQJCRHHNABKAKU-KBQPJGBKSA-N'
  nch <- nchar(x)
  if (nch != 27) {
    if (verbose)
      message('Not 27 characters long.')
    return(FALSE)
  }

  let <- strsplit(x, split = '')[[1]]
  if (any(grepl("[[:digit:]]", let))) {
    if (verbose)
      message('strings contains numbers.')
    return(FALSE)
  }

  if (x != toupper(x)) {
    if (verbose)
      message('Not all character uppercase.')
    return(FALSE)
  }

  if (substr(x, 15, 15) != "-" | substr(x, 26, 26) != "-") {
    if (verbose)
      message('Hyphens not at position 15 and 26.')
    return(FALSE)
  }

  f <- substr(x, 24, 24)
  if (f != 'S' & f != 'N') {
    if (verbose)
      message("Flag character not 'S' or 'N'.")
    return(FALSE)
  }

  f <- substr(x, 25, 25)
  if (f != 'A') {
    if (verbose)
      message("Version character not 'A'.")
    return(FALSE)
  }

  return(TRUE)
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
#' @param verbose logical; print messages during processing to console?
#' @return a logical
#'
#' @author Eduard Szoecs, \email{eduardszoecs@@gmail.com}
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



#' Extract a number from a string
#' @param x character; input string
#' @return a numeric vector
#' @export
#' @examples
#' extr_num('aaaa -95')
extr_num <- function(x) {
  as.numeric(gsub("[^0-9\\-]+", "", x))
}


#' Parse Molfile (as returned by chemspider) into a R-object.
#'
#' @param string molfile as one string
#' @return A list with of four entries: header (h), counts line (cl), atom block (ab) and bond block (bb).
#' header: a = number of atoms, b = number of bonds, l = number of atom lists, f = obsolete,
#' c = chiral flag (0=not chiral, 1 = chiral), s = number of stext entries, x, r, p, i = obsolete,
#' m = 999, v0 version
#'
#' atom block: x, y, z = atom coordinates, a = mass difference, c= charge, s= stereo parity,
#' h = hydrogen count 1, b = stereo care box, v = valence, h = h0 designator, r, i = not used,
#' m = atom-atom mapping number, n = inversion/retention flag, e = exact change flag
#'
#' bond block:
#' 1 = first atom, 2 = second atom, t = bond type, s = stereo type, x = not used, r = bond typology,
#' c = reacting center status.
#'
#' For more information see  \url{infochim.u-strasbg.fr/recherche/Download/Fragmentor/MDL_SDF.pdf}.
#'
#' @author Eduard Szoecs, \email{eduardszoecs@@gmail.com}
#' @references Grabner, M., Varmuza, K., & Dehmer, M. (2012). RMol:
#' a toolset for transforming SD/Molfile structure information into R objects.
#' Source Code for Biology and Medicine, 7, 12. http://doi.org/10.1186/1751-0473-7-12
#' @export

parse_mol <- function(string) {
  if (length(string) > 1)
    stop('string must be of length 1')
  m <- readLines(textConnection(string))
  # header
  h <- trimws(m[1:3])
  # counts line
  cl <- m[4]
  nchar(cl)
  splits <- c(seq(1, 33, by = 3), 34)
  cl <- trimws(substring(cl, splits, c(splits[-1] - 1, nchar(cl))))
  names(cl) <- c('a', 'b', 'l', 'f', 'c', 's', 'x', 'r', 'p', 'i', 'm', 'p')
  # atom block
  na <- as.numeric(cl[1])
  ab <- m[5:(4 + na)]
  ab <- read.table(text = ab)
  names(ab) <- c('x', 'y', 'z', 'a', 'd', 'c', 's', 'h', 'b', 'v', 'H', 'm', 'n', 'e')
  # bound block
  nb <- as.numeric(cl[2])
  bb <- m[(5 + na):(4 + na + nb)]
  bb <- read.table(text = bb)
  names(bb) <- c('1', '2', 't', 's', 'x', 'r', 'c')
  return(list(h = h, cl = cl, ab = ab, bb = bb))
}
