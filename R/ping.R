#' Ping an API used in webchem to see if it's working.
#'
#' @param service character; the same abbreviations used as prefixes in
#' \code{webchem} functions, with the exception of \code{"cs_web"}, which only
#' checks if the ChemSpider website is up, and thus doesn't require an API key.
#' @param apikey character; API key for services that require API keys
#' @import httr
#' @return A logical, TRUE if the service is available or FALSE if it isn't
#' @export
#' @examples
#' \dontrun{
#' ping_service("pan")
#' }
ping_service <-
  function(service = c(
    "bcpc",
    "chebi",
    "chembl",
    "ci",
    "cs",
    "cs_web",
    "cir",
    "cts",
    "etox",
    "fn",
    "nist",
    "opsin",
    "pan",
    "pc",
    "srs",
    "wd"
  ), apikey = NULL
  ) {
    service <- match.arg(service)

    #if pinging service requires POST request, write separate non-exported
    #function, and call here:
    if (service %in% c("pc", "chebi", "cs", "etox")) {
      out <-
        switch(service,
               "pc" = ping_pubchem() & ping_pubchem_pw(),
               "chebi" = ping_chebi(),
               "cs" = ping_cs(apikey = apikey),
               "etox" = ping_etox()
               )
    } else {
      #if service can be pinged with simple GET request, just add URL
      ping_url <-
        switch(service,
               "bcpc" = "https://pesticidecompendium.bcpc.org/introduction.html",
               "chembl" = "https://www.ebi.ac.uk/chembl/api/data/molecule/CHEMBL1082.json",
               "ci" = "https://chem.nlm.nih.gov/chemidplus/rn/50-00-0",
               "cir" = "http://cactus.nci.nih.gov/chemical/structure/Triclosan/cas/xml",
               "cts" = "http://cts.fiehnlab.ucdavis.edu/service/compound/XEFQLINVKFYRCS-UHFFFAOYSA-N",
               "cs_web" = "http://www.chemspider.com/Chemical-Structure.5363.html",
               "fn" = "http://www.flavornet.org/info/121-33-5.html",
               "nist" = "https://webbook.nist.gov/cgi/cbook.cgi?Name=2-hexene&Units=SI",
               "opsin" = "https://opsin.ch.cam.ac.uk/opsin/cyclopropane.json",
               "pan" = "http://www.pesticideinfo.org/List_Chemicals.jsp?",
               "srs" = "https://cdxnodengn.epa.gov/cdx-srs-rest/substance/name/triclosan",
               "wd" = "https://www.wikidata.org/w/api.php"
        )
      if (identical(service, "bcpc")) {
        # For the BCPC server we need to disable gzip encoding as it currently
        # (2021-11-18) results in
        # Error in curl_fetch_memory(https://...):
        # "Failed writing received data to disk/application"
        httr_config <- httr::config(accept_encoding = "identity")
      } else {
        httr_config <- httr::config()
      }
      res <- try(httr::RETRY("GET",
                             ping_url,
                             httr::user_agent(webchem_url()),
                             terminate_on = 404,
                             config = httr_config,
                             quiet = FALSE), silent = FALSE)
      if (inherits(res, "try-error")) {
        out <- FALSE
      }
      else {
        out <- res$status_code == 200
      }
    }
    return(out)
  }


# ETOX ---------------------------------------------------------------------
#' @import httr
#' @noRd
#' @return TRUE if ETOX is reachable
#' @examples
#' \dontrun{
#'  ping_etox()
#'  }
ping_etox <- function(...) {
  baseurl <- "https://webetox.uba.de/webETOX/public/search/stoff.do"

  body <- list("stoffname.selection[0].name" = "triclosan",
               "stoffname.selection[0].type" = "",
               event = "Search")
  res <- try(httr::RETRY("POST",
                         url = baseurl,
                         handle = handle(''),
                         body = body,
                         httr::user_agent(webchem_url()),
                         terminate_on = 404,
                         quiet = TRUE), silent = TRUE)
  if (inherits(res, "try-error")) {
    return(FALSE)
  }
  return(res$status_code == 200)
}

# ChemSpider -----------------------------------------------------------
#' @import httr
#' @import jsonlite
#' @noRd
#' @param apikey character; your API key. If NULL (default),
#'   \code{cs_check_key()} will look for it in .Renviron or .Rprofile.
#' @return TRUE if ChemSpider is reachable
#' @examples
#' \dontrun{
#'  ping_cs()
#'  }
ping_cs <- function(apikey = NULL) {
  if (is.null(apikey)) {
    apikey <- cs_check_key()
  }
  headers <- c("Content-Type" = "", "apikey" = apikey)
  body <- list("name" = "triclosan", "orderBy" = "recordId", "orderDirection" = "ascending")
  body <- jsonlite::toJSON(body, auto_unbox = TRUE)
  res <- try(httr::RETRY("POST",
                         "https://api.rsc.org/compounds/v1/filter/name",
                         add_headers(headers),
                         body = body,
                         httr::user_agent(webchem_url()),
                         terminate_on = 404,
                         quiet = TRUE), silent = TRUE)
  if (inherits(res, "try-error")) {
    return(FALSE)
  }
  return(res$status_code == 200)
}


# ChEBI ---------------------------------------------------------------------
#' @import httr
#' @noRd
#' @return TRUE if ChEBI is reachable
#' @examples
#' \dontrun{
#'  ping_chebi()
#'  }
ping_chebi <- function(...) {
  baseurl <- 'http://www.ebi.ac.uk:80/webservices/chebi/2.0/webservice'

  headers <- c(Accept = 'text/xml',
               Accept = 'multipart/*',
               `Content-Type` = 'text/xml; charset=utf-8',
               SOAPAction = '')
  body <-
    '<soapenv:Envelope
     xmlns:soapenv="http://schemas.xmlsoap.org/soap/envelope/"
     xmlns:chebi="https://www.ebi.ac.uk/webservices/chebi">
      <soapenv:Header/>
        <soapenv:Body>
          <chebi:getLiteEntity>
            <chebi:search>triclosan</chebi:search>
            <chebi:searchCategory>ALL</chebi:searchCategory>
            <chebi:maximumResults>200</chebi:maximumResults>
            <chebi:stars>ALL</chebi:stars>
          </chebi:getLiteEntity>
        </soapenv:Body>
     </soapenv:Envelope>'
  res <- try(httr::RETRY("POST",
                         baseurl,
                         add_headers(headers),
                         body = body,
                         httr::user_agent(webchem_url()),
                         terminate_on = 400,
                         quiet = TRUE), silent = TRUE)
  if (inherits(res, "try-error")) {
    return(FALSE)
  }
  return(res$status_code == 200)
}


# pubchem -----------------------------------------------------------------
#' @import httr
#' @noRd
#' @return TRUE if pubchem is reachable
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
  res <- try(httr::RETRY("POST",
                         qurl,
                         body = paste0(from, '=', query),
                         httr::user_agent(webchem_url()),
                         terminate_on = 404,
                         quiet = TRUE,
                         ...), silent = TRUE)
  if (inherits(res, "try-error")) {
    return(FALSE)
  }
  return(res$status_code == 200)
}

# pubchem PUG-VIEW-----------------------------------------------------------------
#' @import httr
#' @noRd
#' @return TRUE if pubchem PUG-VIEW is reachable
#' @examples
#' \dontrun{
#'  # might fail if API is not available
#'  ping_pubchem_pw()
#'  }
ping_pubchem_pw <- function(...) {
  qurl <- paste("https://pubchem.ncbi.nlm.nih.gov/rest/pug_view/data",
               "compound/176/JSON", sep = "/")
  res <- try(httr::RETRY("POST",
                         qurl,
                         httr::user_agent(webchem_url()),
                         terminate_on = 404,
                         quiet = TRUE), silent = TRUE)
  if (inherits(res, "try-error")) {
    return(FALSE)
  }
  return(res$status_code == 200)
}
