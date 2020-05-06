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

# pubchem PUG-VIEW-----------------------------------------------------------------
#' @import httr
#' @rdname ping
#' @return TRUE if pubchem PUG-VIEW is reachable
#' @export
#' @examples
#' \dontrun{
#'  # might fail if API is not available
#'  ping_pubchem_pw()
#'  }
ping_pubchem_pw <- function(...) {
  qurl <- paste("https://pubchem.ncbi.nlm.nih.gov/rest/pug_view/data",
               "compound/176/JSON?heading=pka", sep = "/")
  res <- POST(qurl,
              user_agent("webchem (https://github.com/ropensci/webchem)"))
  res$status_code == 200
}



# ChemSpider webpage -----------------------------------------------------------
#' @import httr
#' @rdname ping
#' @return TRUE if ChemSpider is reachable
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

# Alan Woods Compendium ---------------------------------------------------------------------
#' @import httr
#' @rdname ping
#' @return TRUE if www.alanwood.net/pesticides is reachable
#' @export
#' @examples
#' \dontrun{
#'  # might fail if site is not available
#'  ping_aw()
#'  }
ping_aw <- function(...) {
  qurl <- 'http://www.alanwood.net/pesticides'
  res <- try(GET(qurl, timeout = 3))
  if (inherits(res, 'try-error'))
    return(FALSE)
  res$status_code == 200
}

# ChEBI ---------------------------------------------------------------------
#' @import httr
#' @rdname ping
#' @return TRUE if ChEBI is reachable
#' @export
#' @examples
#' \dontrun{
#'  # might fail if site is not available
#'  ping_chebi()
#'  }
ping_chebi <- function(...) {
  qurl <- 'https://www.ebi.ac.uk/chebi/webServices.do'
  res <- try(GET(qurl, timeout = 3))
  if (inherits(res, 'try-error'))
    return(FALSE)
  res$status_code == 200
}

# ChemIDPlus ---------------------------------------------------------------------
#' @import httr
#' @rdname ping
#' @return TRUE if ChemIDPlus is reachable
#' @export
#' @examples
#' \dontrun{
#'  # might fail if site is not available
#'  ping_ci()
#'  }
ping_ci <- function(...) {
  qurl <- 'http://chem.sis.nlm.nih.gov/chemidplus'
  res <- try(GET(qurl, timeout = 3))
  if (inherits(res, 'try-error'))
    return(FALSE)
  res$status_code == 200
}

# CIR ---------------------------------------------------------------------
#' @import httr
#' @rdname ping
#' @return TRUE if Chemical Identity Resolver is reachable
#' @export
#' @examples
#' \dontrun{
#'  # might fail if site is not available
#'  ping_cir()
#'  }
ping_cir <- function(...) {
  qurl <- 'http://cactus.nci.nih.gov/chemical/structure/Triclosan/cas/xml'
  res <- try(GET(qurl, timeout = 3))
  if (inherits(res, 'try-error'))
    return(FALSE)
  res$status_code == 200
}

# CTS ---------------------------------------------------------------------
#' @import httr
#' @rdname ping
#' @return TRUE if Chemical Translation Service is reachable
#' @export
#' @examples
#' \dontrun{
#'  # might fail if site is not available
#'  ping_cts()
#'  }
ping_cts <- function(...) {
  qurl <- 'http://cts.fiehnlab.ucdavis.edu/service/compound/XEFQLINVKFYRCS-UHFFFAOYSA-N'
  res <- try(GET(qurl, timeout = 3))
  if (inherits(res, 'try-error'))
    return(FALSE)
  res$status_code == 200
}

# ETOX ---------------------------------------------------------------------
#' @import httr
#' @rdname ping
#' @return TRUE if ETOX is reachable
#' @export
#' @examples
#' \dontrun{
#'  # might fail if site is not available
#'  ping_etox()
#'  }
ping_etox <- function(...) {
  qurl <- 'https://webetox.uba.de/webETOX/public/search/stoff.do'
  res <- try(GET(qurl, timeout = 3))
  if (inherits(res, 'try-error'))
    return(FALSE)
  res$status_code == 200
}

# Flavornet ---------------------------------------------------------------------
#' @import httr
#' @rdname ping
#' @return TRUE if flavornet.org is reachable
#' @export
#' @examples
#' \dontrun{
#'  # might fail if site is not available
#'  ping_fn()
#'  }
ping_fn <- function(...) {
  qurl <- 'http://www.flavornet.org'
  res <- try(GET(qurl, timeout = 3))
  if (inherits(res, 'try-error'))
    return(FALSE)
  res$status_code == 200
}

# NIST web book ---------------------------------------------------------------------
#' @import httr
#' @rdname ping
#' @return TRUE if NIST Web Book is reachable
#' @export
#' @examples
#' \dontrun{
#'  # might fail if site is not available
#'  ping_nist()
#'  }
ping_nist <- function(...) {
  qurl <- "https://webbook.nist.gov/chemistry/cas-ser/"
  res <- try(GET(qurl, timeout = 3))
  if (inherits(res, 'try-error'))
    return(FALSE)
  res$status_code == 200
}

# OPSIN ---------------------------------------------------------------------
#' @import httr
#' @rdname ping
#' @return TRUE if OPSIN is reachable
#' @export
#' @examples
#' \dontrun{
#'  # might fail if site is not available
#'  ping_opsin()
#'  }
ping_opsin <- function(...) {
  qurl <- "http://opsin.ch.cam.ac.uk/opsin/"
  res <- try(GET(qurl, timeout = 3))
  if (inherits(res, 'try-error'))
    return(FALSE)
  res$status_code == 200
}


# Wikidata ---------------------------------------------------------------------
#' @import httr
#' @rdname ping
#' @return TRUE if Wikidata is reachable
#' @export
#' @examples
#' \dontrun{
#'  # might fail if site is not available
#'  ping_wd()
#'  }
ping_wd <- function(...) {
  qurl <- 'https://www.wikidata.org/w/api.php'
  res <- try(GET(qurl, timeout = 3))
  if (inherits(res, 'try-error'))
    return(FALSE)
  res$status_code == 200
}