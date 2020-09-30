#' Retrieve information from ChemIDPlus
#'
#' Retrieve information from ChemIDPlus
#' \url{https://chem.nlm.nih.gov/chemidplus}
#'
#' @import xml2
#' @importFrom rvest html_table
#' @importFrom stats rgamma
#' @importFrom utils URLencode URLdecode
#'
#' @param query character; query string
#' @param from character; type of query string. \code{"rn"} for registry number
#'   (see
#'   \href{https://chem.nlm.nih.gov/chemidplus/jsp/chemidheavy/help.jsp#LiteSearchDataFields}{documentation}
#'   for more details), \code{"name"} for common name, or \code{"inchikey"} for
#'   inchikey as input. \code{"cas"} is a synonym for \code{"rn"} and provided
#'   for consistency across functions.
#' @param match character; How should multiple hits be handled? \code{"first"}
#' returns only the first match, \code{"best"} the best matching (by name) ID,
#' \code{"ask"} enters an interactive mode and the user is asked for input,
#' \code{"na"} returns NA if multiple hits are found.
#' @param verbose logical; should a verbose output be printed on the console?
#' @param type deprecated
#' @return A list of 8 entries: name (vector), synonyms (vector), cas (vector),
#' inchi (vector), inchikey (vector), smiles(vector), toxicity (data.frame),
#' physprop (data.frame) and source_url.
#' @note Please respect the Terms and Conditions of the National Library of
#' Medicine, \url{https://www.nlm.nih.gov/databases/download.html}.
#'
#' @export
#' @examples
#' \dontrun{
#' # might fail if API is not available
#' # query common name
#' y1 <- ci_query(c('Formaldehyde', 'Triclosan'), from = 'name')
#' names(y1)
#' str(y1[['Triclosan']]) # lots of information inside
#' y1[['Triclosan']]$inchikey
#'
#' # Query by CAS
#' y2 <- ci_query('50-00-0', from = 'rn', match = 'first')
#' y2[['50-00-0']]$inchikey
#'
#' # query by inchikey
#' y3 <- ci_query('WSFSSNUMVMOOMR-UHFFFAOYSA-N', from = 'inchikey')
#' y3[[1]]$name
#'
#' # extract lop-P
#'sapply(y1, function(y){
#'  if (length(y) == 1 && is.na(y))
#'    return(NA)
#'  y$physprop$Value[y$physprop$`Physical Property` == 'log P (octanol-water)']
#'  })
#' }
ci_query <- function(query, from = c('name', 'rn', 'inchikey', 'cas'),
                     match = c('first', 'best', 'ask', 'na'),
                     verbose = TRUE, type){

  if (!ping_service("ci")) stop(webchem_message("service_down"))

  if (!missing(type)) {
    message('"type" is deprecated. Please use "from" instead. ')
    from <- type
  }
  from <- match.arg(from)
  match <- match.arg(match)

  foo <- function(query, from, match, verbose){
    if (is.na(query)) {
      if (verbose) webchem_message("na")
      return(NA)
    }
    if (verbose) webchem_message("query", query, appendLF = FALSE)
    query <- URLencode(query, reserved = TRUE)
    baseurl <- switch(
      from,
      rn = 'https://chem.nlm.nih.gov/chemidplus/rn/startswith/',
      name = "https://chem.nlm.nih.gov/chemidplus/name/startswith/",
      inchikey = "https://chem.nlm.nih.gov/chemidplus/inchikey/startswith/")
    # return max 50 hits
    qurl <- paste0(baseurl, query, '?DT_START_ROW=0&DT_ROWS_PER_PAGE=50')
    Sys.sleep( rgamma(1, shape = 15, scale = 1/10))
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
    if (res$status_code == 200) {
      ttt <- read_html(res)
      tit <- xml_text(xml_find_all(ttt, "//head/title"))
      no <- xml_text(xml_find_all(ttt, "//h3"))
      if (length(no) != 0 && 'The following query produced no records:' %in% no) {
        webchem_message("not_found")
        return(NA)
      }

      # handle multiple inputs
      if (grepl('^ChemIDplus Results - Chemical information', x = tit)) {
        if (verbose)
          message(" More then one Link found. ", appendLF = FALSE)
        hit_names <- xml_text(
          xml_find_all(ttt, "//a[@title='Open record details']"))
        hit_cas <- xml_text(
          xml_find_all(
            ttt,
            "//a[@title='Open record details']/following-sibling::text()[1]"))

        # exclude missing cas
        keep <- is.cas(hit_cas, verbose = FALSE)
        hit_cas <- hit_cas[keep]

        hit_names <- hit_names[keep]
        hit_names <- gsub(' \\[.*\\]', '', hit_names)

        hit_cas <-
          matcher(
            hit_cas,
            query = URLdecode(query),
            result = hit_names,
            match = match,
            from = from,
            verbose = verbose
          )
        matched_sub <- names(hit_cas)

        # check hit
        if (is.na(hit_cas)) {
          if (verbose)
            message('CAS not found! Returning NA.')
          return(NA)
        }

        # retry with CAS-API
        qurl <- paste0('https://chem.nlm.nih.gov/chemidplus/rn/', hit_cas)
        if (verbose) webchem_message("query", hit_cas, appendLF = FALSE)
        Sys.sleep( rgamma(1, shape = 15, scale = 1/10))
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
        if (res$status_code == 200) {
          ttt <- read_html(res)
          source_url <- qurl
        }

      } else {
        matched_sub <- xml_text(
          xml_find_all(
            ttt,
            "//h3[contains(., 'Name of Substance')]/following-sibling::div[1]//li"))[1]
        source_url <- gsub('^(.*)\\?.*', '\\1', qurl)
      }

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
        physprop[ , 'Value'] <- as.numeric(physprop[ , 'Value'])
        #= same as physprop
      }

      out <- list(name = name, synonyms = synonyms, cas = cas, inchi = inchi,
                  inchikey = inchikey, smiles = smiles, toxicity = toxicity,
                  physprop = physprop, source_url = source_url)
      attr(out, "matched") <- matched_sub
      class(out) <- 'chemid'
      return(out)


    } else {
      return(NA)
    }
  }
  out <- lapply(query, foo, from = from, match = match, verbose = verbose)
  out <- setNames(out, query)
  class(out) <- c('ci_query', 'list')
  return(out)
}
