#' Retrieve information from ChemIDPlus \url{http://chem.sis.nlm.nih.gov/chemidplus/name/triclosan}
#'
#' Retrieve information from ChemIDPlus \url{http://chem.sis.nlm.nih.gov/chemidplus/name/triclosan}
#'
#' @import RCurl
#'
#' @param query character; query string
#' @param type character; type of query string.
#'     'rn' for regeistry number or 'name' for common name or 'inchikey' for inchikey as input.
#' @param verbose logical; should a verbose output be printed on the console?
#' @return A list of 8 entries: name (vector), synonyms (vector), cas (vector),
#' inchi (vector), inchikey (vector), smiles(vector), toxicity (data.frame),
#' physprop (data.frame).
#'
#' @note The data of the entry \code{physprop} is identical to the result returned
#' by \code{\link{physprop}}.
#'
#' @export
#' @examples
#' \dontrun{
#' # might fail if API is not available
#' y1 <- chemid('Formaldehyde', type = 'name')
#' str(y1)
#' y1$name
#'
#' y2 <- chemid('50-00-0', type = 'rn')
#' str(y2)
#' y2$name
#'
#' y3 <- chemid('50-00-0', type = 'name')
#' y3
#' }
chemid <- function(query, type = c('rn', 'name', 'inchikey'), verbose = TRUE){
  # query <- '50-00-0'
  # query <- 'Triclosan'
  # query <- 'xxxx'
  type <- match.arg(type)
  if (type == 'rn')
    baseurl <- 'http://chem.sis.nlm.nih.gov/chemidplus/rn/'
  if (type == 'name')
    baseurl <- 'http://chem.sis.nlm.nih.gov/chemidplus/name/'
  if (type == 'inchikey')
    baseurl <- 'http://chem.sis.nlm.nih.gov/chemidplus/inchikey'
  qurl <- paste0(baseurl, query)
  if (verbose)
    message(qurl)
  Sys.sleep(0.3)
  tt <- getURL(qurl)
  ttt <- htmlParse(tt)
  #! maybe not the best test....
  if (length(xpathSApply(ttt, "//div[@id = 'resultsContent']")) > 0) {
    message('Not found! Returning NA.\n')
    return(NA)
  }
  name <- xpathSApply(ttt, "//h3[contains(., 'Name of Substance')]/following-sibling::div[1]//li", xmlValue)
  synonyms <- xpathSApply(ttt, "//h3[contains(., 'Synonyms')]/following-sibling::div[1]//li", xmlValue)
  cas <- xpathSApply(ttt, "//h3[contains(., 'CAS Registry')]/following-sibling::ul[1]//li", xmlValue)
  inchi <- gsub('\\n|\\t', '',
                xpathSApply(ttt, "//h3[contains(., 'InChI')]/following-sibling::text()[1]", xmlValue)[1]
                )
  inchikey <- gsub('\\n|\\t', '',
                   xpathSApply(ttt, "//h3[contains(., 'InChIKey')]/following-sibling::text()[1]", xmlValue)
  )
  smiles <- gsub('\\n|\\t', '',
                 xpathSApply(ttt, "//h3[contains(., 'Smiles')]/following-sibling::text()[1]", xmlValue)
  )
  toxicity <- readHTMLTable(xpathSApply(ttt, "//h2[contains(., 'Toxicity')]/following-sibling::div//table")[[1]],
                            stringsAsFactors = FALSE)
  physprop <- readHTMLTable(xpathSApply(ttt, "//h2[contains(., 'Physical Prop')]/following-sibling::div//table")[[1]],
                            stringsAsFactors = FALSE)
  physprop[ , 'Value'] <- as.numeric(physprop[ , 'Value'])
  #= same as physprop

  out <- list(name = name, synonyms = synonyms, cas = cas, inchi = inchi,
              inchikey = inchikey, smiles = smiles, toxicity = toxicity,
              physprop = physprop)
  return(out)
}