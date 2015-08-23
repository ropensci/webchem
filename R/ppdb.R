#' PPDB search index
#'
#' A dataset containing the matched strings and links of the PPDB  \url{http://sitem.herts.ac.uk/aeru/iupac/search.htm}.
#' This dataset has been created using code{\link{ppdb_buildidx}}
#'
#' @format A data frame with 53940 rows and 10 variables:
#' \describe{
#'   \item{cas}{cas}
#'   \item{link}{matched link}
#' }
#' @source  \url{http://sitem.herts.ac.uk/aeru/iupac/search.htm}
#' @seealso \code{\link{ppdb_buildidx}}
"ppdb_idx"

#' Query PPDB search index
#'
#' This function queries the PPDB search index \url{http://sitem.herts.ac.uk/aeru/iupac/search.htm} and
#' is used to get an updated index.
#' This is used to build the index shipped with the webchem package - code{\link{ppdb_idx}}.
#'
#' @import XML RCurl stringr
#'
#' @return A dataframe with 2 variables:
#' \describe{
#'   \item{cas}{string}
#'   \item{link}{matched link}
#'   ...
#' }
#'
#' @author Eduard Szoecs, \email{eduardszoecs@@gmail.com}
#' @seealso \code{\link{ppdb_idx}}
#' @note There should be no need to use this function.
#' Only use this to retrieve an updated index.
#' @export
#' @examples
#' \dontrun{
#' # code used the build the index shipped with etox
#' ppdb_idx <- ppdb_buildidx()
#' }
ppdb_buildidx <- function(){
  # query seach index
  url <- 'http://sitem.herts.ac.uk/aeru/iupac/search.htm'
  search <- getURL(url)
  tt <- htmlParse(search)
  cont <- xpathApply(tt, "//script[contains(.,'# of titles present in the database')]", xmlValue)
  cont <- strsplit(cont[[1]], '\\n')
  links <- cont[[1]][grepl('^links\\[', cont[[1]])]
  links <- str_match(links, '\\\"(.*)\\"')[ , 2]
  baseurl <- 'http://sitem.herts.ac.uk/aeru/iupac/'
  links <- paste0(baseurl, links)

  titles <- cont[[1]][grepl('^title\\[', cont[[1]])][-1]
  # rm start and end of titles
  titles <- str_match(titles, '\\\"(.*)\\"')[ , 2]
  # possible titles
  ptitles <- str_split(titles, ', ')
  cas <- sapply(ptitles, function(x) x[[3]])

  index <- data.frame(cas = cas, link = links)
  return(index)
}


#' Query the ppdb for information
#'
#' This function queries the PPDB \url{http://sitem.herts.ac.uk/aeru/iupac/search.htm} for information.
#'
#' @import XML RCurl
#'
#' @param character; CAS number to query.
#' @return A list of
#'
#' @author Eduard Szoecs, \email{eduardszoecs@@gmail.com}

#' @export
#' @examples
#' \dontrun{
#' ppdb_query('1071-83-6')
#' }
ppdb_query <- function(cas, verbose = TRUE){
  # cas <- '1071-83-6'
  qurl <- ppdb_idx[ppdb_idx$cas == cas, 'link']
  if (length(qurl) == 0) {
    message('CAS not found! Returning NA.\n')
    return(NA)
  }
  if (verbose)
    message('Querying ', qurl)

  tt <- getURL(qurl)
  tables <- readHTMLTable(tt, stringsAsFactors = FALSE, header = FALSE)
  ttt <- htmlParse(tt)

  # ec regulation
  ec_regulation <- tables[[5]]
  colnames(ec_regulation) <- ec_regulation[1, ]
  ec_regulation <- ec_regulation[-1, ]

  # approved in contries
  status <- unlist(xpathApply(ttt, "//*[contains(.,'Approved')]/following-sibling::table[1]/tr[2]/td/p/img/@src"))
  approved_id <- data.frame(t(tables[[6]]))
  approved_id$status <- ifelse(status == 'tick.jpg', TRUE, FALSE)
  approved_id[,'X2'] <- NULL
  names(approved_id)[1] <- 'country'

  # general status
  general <- tables[[7]]
  names(general) <- c('variable', 'value')

#   # formulations
#   formulation <- tables[[8]]
#   names(formulation) <- c('variable', 'value')

  # fate
  fate <- tables[[9]]

  out <- list(ec_regulation = ec_regulation,
       approved_in = approved_id,
       general = general
       # formulation = formulation
       )
  return(out)
}