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
#' @param x character; input InChIKey
#' @param type character; How should be checked? Either, by format (see above)
#' ('format') or by ChemSpider ('chemspider').
#' @param verbose logical; print messages during processing to console?
#' @return a logical
#'
#' @note This function can handle only one inchikey string.
#'
#' @references Heller, Stephen R., et al. "InChI, the IUPAC International
#' Chemical Identifier." Journal of Cheminformatics 7.1 (2015): 23.
#' @references Eduard Szöcs, Tamás Stirling, Eric R. Scott, Andreas Scharmüller,
#' Ralf B. Schäfer (2020). webchem: An R Package to Retrieve Chemical
#' Information from the Web. Journal of Statistical Software, 93(13).
#' \doi{10.18637/jss.v093.i13}.
#' @export
#' @examples
#' is.inchikey('BQJCRHHNABKAKU-KBQPJGBKSA-N')
#' is.inchikey('BQJCRHHNABKAKU-KBQPJGBKSA')
#' is.inchikey('BQJCRHHNABKAKU-KBQPJGBKSA-5')
#' is.inchikey('BQJCRHHNABKAKU-KBQPJGBKSA-n')
#' is.inchikey('BQJCRHHNABKAKU/KBQPJGBKSA/N')
#' is.inchikey('BQJCRHHNABKAKU-KBQPJGBKXA-N')
#' is.inchikey('BQJCRHHNABKAKU-KBQPJGBKSB-N')
is.inchikey = function(x, type = c('format', 'chemspider'),
                       verbose = getOption("verbose")) {
  # x <- 'BQJCRHHNABKAKU-KBQPJGBKSA-N'
  if (length(x) > 1) {
    stop('Cannot handle multiple input strings.')
  }

  type <- match.arg(type)
  out <- switch(type,
                format = is.inchikey_format(x, verbose = verbose),
                chemspider = is.inchikey_cs(x, verbose = verbose))
  return(out)
}


#' Check if input is a valid inchikey using ChemSpider API
#'
#' @param x character; input string
#' @param verbose logical; print messages during processing to console?
#' @return a logical
#'
#' @seealso \code{\link{is.inchikey}} for a pure-R implementation.
#' @export
#' @examples
#' \dontrun{
#' # might fail if API is not available
#' is.inchikey_cs('BQJCRHHNABKAKU-KBQPJGBKSA-N')
#' is.inchikey_cs('BQJCRHHNABKAKU-KBQPJGBKSA')
#' is.inchikey_cs('BQJCRHHNABKAKU-KBQPJGBKSA-5')
#' is.inchikey_cs('BQJCRHHNABKAKU-KBQPJGBKSA-n')
#' is.inchikey_cs('BQJCRHHNABKAKU/KBQPJGBKSA/N')
#' is.inchikey_cs('BQJCRHHNABKAKU-KBQPJGBKXA-N')
#' is.inchikey_cs('BQJCRHHNABKAKU-KBQPJGBKSB-N')
#' }
is.inchikey_cs <- function(x, verbose = getOption("verbose")){

  if (!ping_service("cs_web")) stop(webchem_message("service_down"))

  if (length(x) > 1) {
    stop('Cannot handle multiple input strings.')
  }
  if (is.na(x)) {
    if (verbose) webchem_message("na")
    return(NA)
  }
  baseurl <- 'http://www.chemspider.com/InChI.asmx/IsValidInChIKey?'
  qurl <- paste0(baseurl, 'inchi_key=', x)
  webchem_sleep(type = 'scrape')
  if (verbose) webchem_message("query", x, appendLF = FALSE)
  res <- try(httr::RETRY("GET",
                         qurl,
                         httr::user_agent(webchem_url()),
                         terminate_on = 404,
                         quiet = TRUE), silent = TRUE)
  if (inherits(res, "try-error")) {
    if (verbose) webchem_message("service_down")
    return(NA)
  }
  if (verbose) message(httr::message_for_status(res))
  if (res$status_code == 200){
    h <- xml2::read_xml(res)
    out <- as.logical(xml_text(h))
    return(out)
    }
  else {
    return(NA)
  }
}



#' Check if input is a valid inchikey using format
#'
#' @description  Inchikey must fulfill the following criteria:
#' 1) consist of 27 characters;
#' 2) be all uppercase, all letters (no numbers);
#' 3) contain two hyphens at positions 15 and 26;
#' 4) 24th character (flag character) be 'S' (Standard InChI) or 'N'
#' (non-standard)
#' 5) 25th character (version character) must be 'A' (currently).
#'
#' @param x character; input string
#' @param verbose logical; print messages during processing to console?
#' @return a logical
#'
#' @seealso \code{\link{is.inchikey}} for a pure-R implementation.
#' @export
#' @examples
#' \dontrun{
#' # might fail if API is not available
#' is.inchikey_format('BQJCRHHNABKAKU-KBQPJGBKSA-N')
#' is.inchikey_format('BQJCRHHNABKAKU-KBQPJGBKSA')
#' is.inchikey_format('BQJCRHHNABKAKU-KBQPJGBKSA-5')
#' is.inchikey_format('BQJCRHHNABKAKU-KBQPJGBKSA-n')
#' is.inchikey_format('BQJCRHHNABKAKU/KBQPJGBKSA/N')
#' is.inchikey_format('BQJCRHHNABKAKU-KBQPJGBKXA-N')
#' is.inchikey_format('BQJCRHHNABKAKU-KBQPJGBKSB-N')
#' }
is.inchikey_format = function(x, verbose = getOption("verbose")) {
  # x <- 'BQJCRHHNABKAKU-KBQPJGBKSA-N'
  if (length(x) > 1) {
    stop('Cannot handle multiple input strings.')
  }
  nch <- nchar(x)
  if (nch != 27) {
    if (verbose)
      message('Not 27 characters long.')
    return(FALSE)
  }
  let <- strsplit(x, split = '')[[1]]
  if (any(grepl("[[:digit:]]", let))) {
    if (verbose)
      message('string contains numbers.')
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
#' consists from two up to seven digits; 3) the second of two digits; 4) the
#' third of one digit (check digit); 5) the check digits corresponds the
#' checksum. The checksum is found by taking the last digit (excluding the check
#' digit) multiplyingit with 1, the second last multiplied with 2, the
#' third-last multiplied with 3 etc. The modulo 10 of the sum of these is the
#' checksum.
#'
#' @import stringr
#' @param x character; input CAS
#' @param verbose logical; print messages during processing to console?
#' @return a logical
#' @note This function can only handle one CAS string
#' @references Eduard Szöcs, Tamás Stirling, Eric R. Scott, Andreas Scharmüller,
#' Ralf B. Schäfer (2020). webchem: An R Package to Retrieve Chemical
#' Information from the Web. Journal of Statistical Software, 93(13).
#' \doi{10.18637/jss.v093.i13}.
#'
#' @export
#' @examples
#' is.cas('64-17-5')
#' is.cas('64175')
#' is.cas('4-17-5')
#' is.cas('64-177-6')
#' is.cas('64-17-55')
#' is.cas('64-17-6')
is.cas <-  function(x, verbose = getOption("verbose")) {

  foo <- function(x, verbose) {
    # pass NA's through
    if (is.na(x)) return(NA)

    # cas must not have any alpha characters
    if (grepl(pattern = "[[:alpha:]]", x = x)) {
      if (isTRUE(verbose)) {
        message(x,": String contains alpha characters")
        }
      return(FALSE)
    }

    # cas must not have any white space
    if (grepl(pattern = "\\s+", x = x)) {
      if(isTRUE(verbose)) {
        message(x, ": String contains whitespace")
      }
      return(FALSE)
    }
    # cas must have two hyphens
    nsep <- str_count(x, '-')
    if (nsep != 2) {
      if (isTRUE(verbose))
        message(x, ': Less than 2 hyphens in string.')
      return(FALSE)
    }

    # first part 2 to 7 digits
    fi <- gsub('^(.*)-(.*)-(.*)$', '\\1', x)
    if (nchar(fi) > 7 | nchar(fi) < 2) {
      if (isTRUE(verbose))
        message(x, ': First part has more than 7 digits!')
      return(FALSE)
    }

    # second part must be two digits
    se <- gsub('^(.*)-(.*)-(.*)$', '\\2', x)
    if (nchar(se) != 2) {
      if (isTRUE(verbose))
        message(x, ': Second part should have two digits!')
      return(FALSE)
    }

    # third part (checksum) must be 1 digit
    th <- gsub('^(.*)-(.*)-(.*)$', '\\3', x)
    if (nchar(th) != 1) {
      if (isTRUE(verbose))
        message(x, ': Third part should have 1 digit!')
      return(FALSE)
    }

    # check checksum
    di <-  as.numeric(strsplit(gsub('^(.*)-(.*)-(.*)$', '\\1\\2', x),
                               split = '')[[1]])
    checksum <- sum(rev(seq_along(di)) * di)
    if (checksum %% 10 != as.numeric(th)) {
      if (isTRUE(verbose))
        message(x, ': Checksum is not correct! ', checksum %% 10, ' vs. ', th)
      return(FALSE)
    }
    return(TRUE)
  }
  return(sapply(x, foo, verbose))
}


#' Check if input is a SMILES string
#'
#' @description This function checks if a string is a valid SMILES by checking
#' if (R)CDK can parse it. If it cannot be parsed by rcdk FALSE is returned,
#' else TRUE.
#' @param x character; input SMILES.
#' @param verbose logical; print messages during processing to console?
#' @return a logical
#'
#' @note This function can handle only one SMILES string.
#'
#' @references Egon Willighagen (2015). How to test SMILES strings in
#' Supplementary Information.
#' \url{https://chem-bla-ics.blogspot.nl/2015/10/how-to-test-smiles-strings-in.html}
#'
#' @export
#' @examples
#' \dontrun{
#' # might fail if rcdk is not working properly
#' is.smiles('Clc(c(Cl)c(Cl)c1C(=O)O)c(Cl)c1Cl')
#' is.smiles('Clc(c(Cl)c(Cl)c1C(=O)O)c(Cl)c1ClJ')
#' }
is.smiles <- function(x, verbose = getOption("verbose")) {
  if (!requireNamespace("rcdk", quietly = TRUE)) {
    stop("rcdk needed for this function to work. Please install it.",
         call. = FALSE)
  }
  # x <- 'Clc(c(Cl)c(Cl)c1C(=O)O)c(Cl)c1Cl'
  if (length(x) > 1) {
    stop('Cannot handle multiple input strings.')
  }
  out <- try(rcdk::parse.smiles(x), silent = TRUE)
  if (inherits(out[[1]], 'try-error') | is.null(out[[1]])) {
    return(FALSE)
  } else {
    return(TRUE)
  }
}


#' Extract a number from a string
#' @param x character; input string
#' @return a numeric vector
#' @noRd
#' @examples
#' extr_num('aaaa -95')
#' extr_num("Melting Pt : -44.6 deg C")
extr_num <- function(x) {
  if (length(x) == 0)
    return(NA)
  as.numeric(gsub("[^0-9\\.\\-]+", "", x))
}


#' Parse Molfile (as returned by ChemSpider) into a R-object.
#'
#' @param string molfile as one string
#' @return A list with of four entries: header (eh), counts line (cl), atom
#' block (ab) and bond block (bb).
#'
#' header: a = number of atoms, b = number of bonds, l = number of atom lists,
#' f = obsolete, c = chiral flag (0=not chiral, 1 = chiral), s = number of stext
#' entries, x, r, p, i = obsolete, m = 999, v0 version
#'
#' atom block: x, y, z = atom coordinates, a = mass difference, c= charge,
#' s= stereo parity, h = hydrogen count 1, b = stereo care box, v = valence,
#' h = h0 designator, r, i = not used, m = atom-atom mapping number,
#' n = inversion/retention flag, e = exact change flag
#'
#' bond block:
#' 1 = first atom, 2 = second atom, t = bond type, s = stereo type, x = not
#' used, r = bond typology, c = reacting center status.
#'
#' @references Grabner, M., Varmuza, K., & Dehmer, M. (2012). RMol:
#' a toolset for transforming SD/Molfile structure information into R objects.
#' Source Code for Biology and Medicine, 7, 12.
#' \doi{10.1186/1751-0473-7-12}
#' @export

parse_mol <- function(string) {
  if (!is.character(string)) stop("string is not a character string")
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
  # atom block
  na <- as.numeric(cl[1])
  ab <- m[5:(4 + na)]
  ab <- read.table(text = ab)
  # bound block
  nb <- as.numeric(cl[2])
  bb <- m[(5 + na):(4 + na + nb)]
  bb <- read.table(text = bb)
  return(list(eh = h, cl = cl, ab = ab, bb = bb))
}

#' Export a Chemical Structure in .mol Format.
#'
#' Some webchem functions return character strings that contain a chemical
#' structure in Mol format. This function exports a character string as a .mol
#' file so it can be imported with other chemistry software.
#' @param x a character string of a chemical structure in mol format.
#' @param file a character vector of file names
#' @export
#' @examples
#' \dontrun{
#' # export Mol file
#' csid <- get_csid("bergapten")
#' mol3d <- cs_compinfo(csid$csid, field = "Mol3D")
#' write_mol(mol3d$mol3D, file = mol3d$id)
#'
#' # export multiple Mol files
#' csids <- get_csid(c("bergapten", "xanthotoxin"))
#' mol3ds <- cs_compinfo(csids$csid, field = "Mol3D")
#' mapply(function(x, y) write_mol(x, y), x = mol3ds$mol3D, y = mol3ds$id)
#' }
write_mol <- function(x, file = "") {
  if (!is.character(x)) stop("x is not a character string")
  mol <- try(parse_mol(x), silent = TRUE)
  if (inherits(mol, "try-error")) {
    stop ("x is not a Mol string")
  }
  utils::write.table(x,
                     file = file,
                     row.names = FALSE,
                     col.names= FALSE,
                     quote = FALSE)
}

#' Format numbers as CAS numbers
#' @description This function attempts to format numeric (or character) vectors
#' as character vectors of CAS numbers.  If they cannot be converted to CAS
#' format or don't pass \code{\link{is.cas}}, \code{NA} is returned
#' @param x numeric vector, or character vector of CAS numbers missing the
#' hyphens
#'
#' @return character vector of valid CAS numbers
#' @seealso \code{\link{is.cas}}
#' @export
#' @examples
#' x = c(58082, 123456, "hexenol")
#' as.cas(x)
#'
as.cas <- function(x){
  format.cas <- function(x){
    if(is.na(x)) {
      return(NA)
    } else if (suppressMessages(is.cas(x))) {
      return(x)
    } else {
      parsed <- gsub("([0-9]+)([0-9]{2})([0-9]{1})", '\\1-\\2-\\3', x)
      pass <- is.cas(parsed)
      out <- ifelse(pass, parsed, NA)
      return(out)
    }
  }

  sapply(x, format.cas, USE.NAMES = FALSE)
}



#' Used internally to handle the `match` argument in most functions.
#'
#' @param x a vector of hits returned from a query
#' @param query what the query was, only used if match = "best"
#' @param result vector of results of the same type as `query` and same length
#'   as `x`, only used if match = "best
#' @param match character; How should multiple hits be handled? "all" returns
#'   all matched IDs, "first" only the first match, "best" the best matching (by
#'   name) ID, "ask" is a interactive mode and the user is asked for input, "na"
#' @param from character; used only to check that match = "best" is used sensibly.
#' @param verbose print messages?
#' @noRd
#'
#' @examples
#' testids <- c("123", "456", "789")
#' results <- c("apple", "banana", "orange")
#' matcher(testids, query = "bananananan", result = results, match = "best")
matcher <-
  function(x,
           query = NULL,
           result = NULL,
           match = c("all", "best", "first", "ask", "na"),
           from = NULL,
           verbose = getOption("verbose")) {

    match <- match.arg(match)
    names(x) <- result

    if (length(x) == 1) {
      return(x)
    } else {
      if (verbose) message(" Multiple found. ", appendLF = FALSE)

      if (!is.null(from)) {
        if (!str_detect(tolower(from), "name") & match == "best") {
          warning("match = 'best' only makes sense for chemical name queries.\n
                  Setting match = 'first'.")
          match <- "first"
        }
      }

      if (match == "all") {
        if (verbose) message("Returning all.")
        return(x)
      }

      else if (match == "best") {
        #check that x and result are same length
        if (length(x) != length(result))
          stop("Can't use match = 'best' without query matches for each output")
        if (verbose) message("Returning best.")
        dd <- adist(query, result) / nchar(result)
        return(x[which.min(dd)])
      } else if (match == "first") {
        if (verbose) message("Returning first.")
        return(x[1])

      } else if (match == "ask" & interactive()) {
        if (!is.null(result)) {
          choices <- paste0(result, ": ", x)
        } else {
          choices <- x
        }
        pick <- menu(choices, graphics = FALSE, paste0("Select result for '", query, "':"))
        return(x[pick])

      } else if (match == "na") {
        if (verbose) message("Returning NA.")
        x <- NA
        names(x) <- NA
        return(x)
      }
    }
  }

#' Webchem messages
#'
#' Webchem spacific messages to be used in verbose messages.
#' @noRd
webchem_message <- function(action = c("na",
                                       "query",
                                       "query_all",
                                       "not_found",
                                       "not_available",
                                       "service_down"),
                            appendLF = TRUE,
                            ...) {
  action <- match.arg(action)
  string <- switch(
    action,
    na = "Query is NA. Returning NA.",
    query = paste0("Querying ", ..., ". "),
    query_all = "Querying. ",
    not_found = "Not found. Returning NA.",
    not_available = "Not available. Returning NA.",
    service_down = "Service not available. Returning NA."
    )
  message(string, appendLF = FALSE)
  if (appendLF) message("")
}

#' Webchem URL
#'
#' URL of the webchem package to be used in httr::user_agent()
#' @noRd
webchem_url <- function() {
  url <- "https://cran.r-project.org/web/packages/webchem/index.html"
  return(url)
}

#' Function to wait between every web-service query
#'
#' @param time numeric; Wait time in seconds.
#' @param type character; Will be an API queried or a website scraped?
#' @noRd
#'
webchem_sleep <- function(time = NULL,
                          type = c('API', 'scrape')) {
  type <- match.arg(type)
  if (is.null(time)) {
    if (type == 'API') {
      time <- 0.2
    }
    if (type == 'scrape') {
      time <- 0.3
    }
  } else {
    if (!is.numeric(time)) {
      stop('Set time is not numeric.')
    }
  }

  Sys.sleep(time)
}
