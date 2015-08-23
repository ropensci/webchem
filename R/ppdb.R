#' Sappschot of the PPDB search index
#'
#' A dataset containing matched strings and links to the PPDB \url{http://sitem.herts.ac.uk/aeru/iupac/search.htm}
#' Build using \code{\link{ppdb_buildidx}} on 23th August 2015.
#'
#' @format A data frame with 27383 rows and 2 variables:
#' \describe{
#'   \item{match}{string}
#'   \item{link}{matched link}
#' }
#'
#' @seealso \code{\link{ppdb_buildidx}}
#' @source \url{http://sitem.herts.ac.uk/aeru/iupac/search.htm}
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
#'   \item{match}{string}
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
#' save(ppdb_idx, file = 'ppdb_idx.Rdata')
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
  # how often to replicate link
  nptitles <- sapply(ptitles, length)
  links_rep <- rep(links, times = nptitles)

  index <- data.frame(match = unlist(ptitles), link = links_rep)
  index <- index[!index$match == '', ]
  return(index)
}