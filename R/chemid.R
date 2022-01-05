#' Retrieve information from ChemIDPlus
#'
#' Retrieve information from ChemIDPlus
#' \url{https://chem.nlm.nih.gov/chemidplus}
#'
#' @import xml2
#' @importFrom rvest html_table
#' @param query character; query string
#' @param from character; type of query string, can be one of \code{"rn"} for
#' CAS registry numbers or \code{"inchikey"}.
#' @param verbose logical; should a verbose output be printed on the console?
#' @return A list of 8 entries: name (vector), synonyms (vector), cas (vector),
#' inchi (vector), inchikey (vector), smiles(vector), toxicity (data.frame),
#' physprop (data.frame) and source_url.
#' @note Please respect the Terms and Conditions of the National Library of
#' Medicine, \url{https://www.nlm.nih.gov/databases/download.html}.
#' @export
#' @examples
#' \dontrun{
#' # might fail if API is not available
#' y1 <- ci_query('50-00-0', from = 'rn')
#' y1[['50-00-0']]$inchikey
#'
#' # query by inchikey
#' y2 <- ci_query('WSFSSNUMVMOOMR-UHFFFAOYSA-N', from = 'inchikey')
#' y2[[1]]$name
#'
#' # query multiple compounds
#' comps <- c("50-00-0", "64-17-5")
#' y3 <- ci_query(comps, from = "rn")
#'
#' # extract log-P
#' sapply(y3, function(y){
#'  if (length(y) == 1 && is.na(y))
#'    return(NA)
#'  y$physprop$Value[y$physprop$`Physical Property` == 'log P (octanol-water)']
#'  })
#' }
ci_query <- function(query, from = c("rn", "inchikey"),
                     verbose = getOption("verbose")){

  if (!ping_service("ci")) stop(webchem_message("service_down"))

  from <- match.arg(from)

  foo <- function(query, from, verbose){
    if (is.na(query)) {
      if (verbose) webchem_message("na")
      return(NA)
    }
    if (from == "rn" && !is.cas(query, verbose = verbose)) return(NA)
    if (verbose) webchem_message("query", query, appendLF = FALSE)
    qurl <- paste0("https://chem.nlm.nih.gov/chemidplus/",
                   switch(from, rn = "rn/", inchikey = "inchikey/"),
                   query)
    webchem_sleep(type = 'scrape')
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
    if (res$status_code != 200){
      return(NA)
    }
    else {
      ttt <- read_html(res)
      source_url <- qurl

      # Extract data:

      if (is.na(xml_find_first(ttt, "//h3[contains(., 'Name of Substance')]/following-sibling::div[1]//li"))) {
        name <- NA
      } else {
        name <- xml_text(xml_find_all(ttt, "//h3[contains(., 'Name of Substance')]/following-sibling::div[1]//li"))
      }

      if (is.na(xml_find_first(ttt, "//h3[contains(., 'Synonyms')]/following-sibling::div[1]//li"))) {
        synonyms <- NA
      } else {
        synonyms <- xml_text(xml_find_all(ttt, "//h3[contains(., 'Synonyms')]/following-sibling::div[1]//li"))
      }

      if (is.na(xml_find_first(ttt, "//h3[contains(., 'CAS Registry')]/following-sibling::ul[1]//li"))) {
        cas <- NA
      } else {
        cas <- xml_text(xml_find_all(ttt, "//h3[contains(., 'CAS Registry')]/following-sibling::ul[1]//li"))
      }

      if (is.na(xml_find_first(ttt, "//h3[contains(., 'InChI')]/following-sibling::text()[1]"))) {
        inchi <- NA
      } else {
        inchi <- gsub('\\n|\\t', '',
                      xml_text(xml_find_all(ttt, "//h3[contains(., 'InChI')]/following-sibling::text()[1]"))[1]
        )
      }

      if (is.na(xml_find_first(ttt, "//h3[contains(., 'InChIKey')]/following-sibling::text()[1]"))) {
        inchikey <- NA
      } else {
        inchikey <- gsub('\\n|\\t|\\r', '',
                         xml_text(xml_find_all(ttt, "//h3[contains(., 'InChIKey')]/following-sibling::text()[1]"))
        )
      }

      if (is.na(xml_find_first(ttt, "//h3[contains(., 'Smiles')]/following-sibling::text()[1]"))) {
        smiles <- NA
      } else {
        smiles <- gsub('\\n|\\t|\\r', '',
                       xml_text(xml_find_all(ttt, "//h3[contains(., 'Smiles')]/following-sibling::text()[1]"))
        )
      }

      if (is.na(xml_find_first(ttt, "//h2[contains(., 'Toxicity')]/following-sibling::div//table"))) {
        toxicity <- NA
      } else {
        toxicity <- html_table(xml_find_all(ttt, "//h2[contains(., 'Toxicity')]/following-sibling::div//table"))[[1]]
      }

      if (is.na(xml_find_first(ttt, "//h2[contains(., 'Physical Prop')]/following-sibling::div//table"))) {
        physprop <- NA
      } else {
        physprop <- html_table(xml_find_all(ttt, "//h2[contains(., 'Physical Prop')]/following-sibling::div//table"))[[1]]
        physprop[ , 'Value'] <- as.numeric(unlist(physprop[ , 'Value']))
        #= same as physprop
      }

      out <- list(name = name, synonyms = synonyms, cas = cas, inchi = inchi,
                  inchikey = inchikey, smiles = smiles, toxicity = toxicity,
                  physprop = physprop, source_url = source_url)
      return(out)
      }
  }

  out <- lapply(query, foo, from = from, verbose = verbose)
  names(out) <- query
  class(out) <- c('ci_query', 'list')
  return(out)
}
