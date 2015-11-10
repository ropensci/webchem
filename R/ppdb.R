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
#' @details Retrieved using \code{\link{ppdb_idx}} on 11th October 2015.
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
  # fix utf8
  cas <- gsub('\u0096', '-', cas)
  ppdb_idx <- data.frame(cas = iconv(cas, from = "UTF-8", to = "ASCII"),
                      link = iconv(links, from = "UTF-8", to = "ASCII"),
                      stringsAsFactors = FALSE)
  ppdb_idx <- ppdb_idx[!ppdb_idx$cas == '', ]
  # save(ppdb_idx, file = 'data/ppdb_idx.rda')
  return(ppdb_idx)
}


#' Query the ppdb for information
#'
#' This function queries the PPDB \url{http://sitem.herts.ac.uk/aeru/iupac/search.htm} for information.
#'
#' @import XML RCurl
#'
#' @param cas character; CAS number to query.
#' @param verbose logical; print message during processing to console?
#' @param index A index object, as created by \code{\link{ppdb_buildidx}}.
#' If NULL (default), the index shipped with webchem is used \code{\link{ppdb_idx}}.
#' @return A list of 10 data.frames : ec_regulation, approved_in, general, parents, fate,
#' deg, soil, metab, etox and names.
#'
#' See also \url{http://sitem.herts.ac.uk/aeru/iupac/docs/Background_and_Support.pdf} for more information on the data
#'
#' @author Eduard Szoecs, \email{eduardszoecs@@gmail.com}

#' @export
#' @examples
#' \dontrun{
#' # might fail if Server is not available
#' gly <- ppdb('1071-83-6')
#' gly$approved_in
#'
#' # handle multiple CAS
#'  cas <- c('1071-83-6', '50-00-0')
#' }
ppdb <- function(cas, verbose = TRUE, index = NULL){
  # cas <- '1071-83-6'
  # cas <- '50-00-0'
  # cas <- 'xxxxx'

  chk <- function(x){
    if (inherits(x, 'try-error'))
      return(NA)
    return(x)
  }

  if (is.null(index)) {
    # make dataset available
    ppdb_idx <- webchem::ppdb_idx
  } else {
    ppdb_idx <- index
  }


  qurl <- ppdb_idx[ppdb_idx$cas == cas, 'link']
  if (length(qurl) == 0) {
    message('CAS not found! Returning NA.\n')
    return(NA)
  }
  if (verbose)
    message('Querying ', qurl)

  Sys.sleep(0.3)
  tt <- getURL(qurl)
  ttt <- htmlParse(tt)

  # ec regulation
  ec_regulation <- readHTMLTable(getNodeSet(ttt, "//p[contains(.,'EC Reg')]/following-sibling::table[1]")[[1]],
                                 stringsAsFactors = FALSE)


  # approved in contries
  status <- unlist(xpathApply(ttt, "//*[contains(.,'Approved')]/following-sibling::table[1]/tr[2]/td/p/img/@src"))
  approved_id <- data.frame(t(readHTMLTable(getNodeSet(ttt, "//p[contains(.,'Approved for use')]/following-sibling::table[1]")[[1]],
                                            stringsAsFactors = FALSE)))
  approved_id$status <- ifelse(status == 'tick.jpg', TRUE, FALSE)
  approved_id[ , 1] <- NULL
  approved_id$county <- rownames(approved_id)

  # general status
  general <- readHTMLTable(getNodeSet(ttt, "//p[contains(.,'General status')]/following-sibling::table[1]")[[1]],
                           stringsAsFactors = FALSE)
  names(general) <- c('variable', 'value')

  # parents
  parents <- try(readHTMLTable(getNodeSet(ttt, "//p[contains(.,'Can be a metabolite of')]/following-sibling::table[1]")[[1]],
                               stringsAsFactors = FALSE),
                 silent = TRUE)
  parents <- chk(parents)


#   # formulations
#   formulation <- tables[[8]]
#   names(formulation) <- c('variable', 'value')

  # fate
  fate <- try(readHTMLTable(getNodeSet(ttt, "//p[contains(.,'ENVIRONMENTAL FATE')]/following-sibling::table[1]")[[1]],
                            stringsAsFactors = FALSE),
                 silent = TRUE)
  fate <- chk(fate)
  if (!length(fate) == 0) {
    take <- fate[!is.na(fate[ , 'V5']), ]
    take[ , 'V1'] <- paste(take[ , 'V1'], take[ , 'V2'])
    take[ , 'V2'] <- NULL
    fate <- fate[is.na(fate[ , 'V5']), 1:4]
    nam <- c('Property', 'Value', 'Source/Quality', 'Interpretation')
    colnames(fate) <- nam
    colnames(take) <- nam
    fate <- rbind(fate, take)
    fate <- fate[!grepl('Note', fate[ , 'Property']), ]
    if (any(is.na(fate[ , 'Interpretation']))) {
      take <- fate[is.na(fate[ , 'Interpretation']), ]
      corr <- which(is.na(fate[ , 'Interpretation']))[1] - 1
      take <- data.frame(Prop = NA, take[ , c(1,2,3)])
      take[ , 1] <- fate[corr, 'Property']
      colnames(take) <- nam
      fate <- fate[!is.na(fate[ , 'Interpretation']), ]
      fate <- rbind(fate, take)
    }
  }


  # degredation
  deg <-  try(readHTMLTable(getNodeSet(ttt, "//p[contains(.,'Degradation')]/following-sibling::table[1]")[[1]],
                            stringsAsFactors = FALSE),
              silent = TRUE)
  deg <- chk(deg)
  if (!length(deg) == 0) {
    nam <- c('Property', 'Value', 'Source/Quality', 'Interpretation')
    colnames(deg) <- nam
    deg <- deg[!grepl('Note', deg[ , 'Property']), ]

    if (any(deg[ , 'Value'] == 'Value')) {
      take <- deg[deg[ , 'Value'] == 'Value', ]
      take <- data.frame(take[ , -2], N = NA)
      colnames(take) <- colnames(deg)
      deg <- deg[!deg[ , 'Value'] == 'Value', ]
      deg <- rbind(deg, take)

    }
    corr <- deg[!is.na(deg[,5]), 1]
    deg[!is.na(deg[ , 5]), 1] <- paste(deg[!is.na(deg[ , 5]), 1], deg[!is.na(deg[ , 5]), 2])
    deg[!is.na(deg[ , 5]), ] <- c(deg[!is.na(deg[ , 5]), c(1, 3, 4, 5)], NA)
    deg[ , 5] <- NULL
  }



  # soil adsorption and mobility
  soil <- try(readHTMLTable(getNodeSet(ttt, "//p[contains(.,'Soil adsorption')]/following-sibling::table[1]")[[1]],
                            stringsAsFactors = FALSE),
              silent = TRUE)
  soil <- chk(soil)
  if (!length(soil) == 0) {
    nam <- c('Property', 'Value', 'Source/Quality', 'Interpretation')
    colnames(soil) <- nam
    soil <- soil[!grepl('Note', soil[ , 'Property']), ]
    corr <- soil[!is.na(soil[,5]), 1]
    soil[!is.na(soil[ , 5]), 1] <- paste(soil[!is.na(soil[ , 5]), 1], soil[!is.na(soil[ , 5]), 2])
    soil[!is.na(soil[ , 5]), ] <- c(soil[!is.na(soil[ , 5]), c(1, 3, 4, 5)], NA)
    soil[ , 5] <- NULL
  }


  # metabolites
  metab <- try(readHTMLTable(getNodeSet(ttt, "//p[contains(.,'Key metabolites')]/following-sibling::table[1]")[[1]],
                             stringsAsFactors = FALSE),
               silent = TRUE)
  metab <- chk(metab)

  # ecotoxicology
  etox <- try(readHTMLTable(getNodeSet(ttt, "//p[contains(.,'ECOTOXICOLOGY')]/following-sibling::table[1]")[[1]],
                            stringsAsFactors = FALSE),
              silent = TRUE)
  etox <- chk(etox)
  if (!length(etox) == 0) {
    nam <- c('Property', 'Value', 'Source/Quality', 'Interpretation')
    colnames(etox) <- nam
    etox <- etox[!grepl('Note', etox[ , 'Property']), ]
    corr <- etox[!is.na(etox[ , 5]), 1]
    etox[!is.na(etox[ , 5]), 1] <- paste(etox[!is.na(etox[ , 5]), 1], etox[!is.na(etox[ , 5]), 2])
    etox[!is.na(etox[ , 5]), ] <- c(etox[!is.na(etox[ , 5]), c(1, 3, 4, 5)], NA)
    etox[ , 5] <- NULL
  }

  # names
  names <- try(readHTMLTable(getNodeSet(ttt, "//p[contains(.,'TRANSLATIONS ')]/following-sibling::table[1]")[[1]],
                             stringsAsFactors = FALSE),
               silent = TRUE)
  names <- chk(names)

  out <- list(ec_regulation = ec_regulation,
       approved_in = approved_id,
       general = general,
       parents = parents,
       # formulation = formulation,
       fate = fate,
       deg = deg,
       soil = soil,
       metab = metab,
       etox = etox,
       names = names
       )
  return(out)
}