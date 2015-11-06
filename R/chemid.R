#' Retrieve information from ChemIDPlus \url{http://chem.sis.nlm.nih.gov/chemidplus/name/triclosan}
#'
#' Retrieve information from ChemIDPlus \url{http://chem.sis.nlm.nih.gov/chemidplus/name/triclosan}
#'
#' @import RCurl xml2
#'
#' @param query character; query string
#' @param type character; type of querystring.
#'     'rn' for regeistry number or 'name' for common name.
#'
#' @export
chemid <- function(query, type = c('rn', 'name'), verbose = TRUE){
  # query <- '50-00-0'
  # query <- 'Triclosan'
  # query <- 'xxxx'
  type <- match.arg(type)
  if (type == 'rn')
    baseurl <- 'http://chem.sis.nlm.nih.gov/chemidplus/rn/'
  if (type = 'name')
    baseurl <- 'http://chem.sis.nlm.nih.gov/chemidplus/name/'
  qurl <- paste0(baseurl, query)
  if (verbose)
    message(qurl)
  Sys.sleep(0.1)
  tt <- getURL(qurl)
  ttt <- htmlParse(tt)
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
  toxicity <- readHTMLTable(xpathSApply(ttt, "//h2[contains(., 'Toxicity')]/following-sibling::div//table")[[1]])
  physprop <- readHTMLTable(xpathSApply(ttt, "//h2[contains(., 'Physical Prop')]/following-sibling::div//table")[[1]])
  #= same as physprop

}