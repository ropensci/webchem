#' PPDB search index
#'
#' A dataset containing the matched strings and links of the PPDB  \url{http://sitem.herts.ac.uk/aeru/iupac/search.htm}.
#' This dataset has been created using code{\link{ppdb_buildidx}}
#'
#' @format A data frame with 1745 rows and 2 variables:
#' \describe{
#'   \item{cas}{cas}
#'   \item{link}{matched link}
#' }
#' @source  \url{http://sitem.herts.ac.uk/aeru/iupac/search.htm}
#' @details Retrieved using \code{\link{ppdb_idx}} on 8th January 2016.
#' @seealso \code{\link{ppdb_buildidx}}
"ppdb_idx"


#' Query PPDB search index
#'
#' This function queries the PPDB search index \url{http://sitem.herts.ac.uk/aeru/iupac/search.htm} and
#' is used to get an updated index.
#' This is used to build the index shipped with the webchem package - code{\link{ppdb_idx}}.
#'
#' @import xml2 stringr
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
#' # code used the build the index shipped with webchem
#' ppdb_idx <- ppdb_buildidx()
#' }
ppdb_buildidx <- function(){
  # query seach index
  qurl <- 'http://sitem.herts.ac.uk/aeru/iupac/search.htm'
  Sys.sleep(5)
  tt <- read_html(qurl)
  cont <- xml_text(xml_find_all(tt, "//script[contains(.,'# of titles present in the database')]"))
  Encoding(cont) <- "latin1"
  cont <- strsplit(cont, '\\n')
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
  ppdb_idx <- data.frame(cas = iconv(cas, from = "UTF-8", to = "ASCII"),
                      link = iconv(links, from = "UTF-8", to = "ASCII"),
                      stringsAsFactors = FALSE)
  # remove empty entries
  ppdb_idx <- ppdb_idx[!(ppdb_idx$cas == '' | is.na(ppdb_idx$cas)), ]
  # remove non-cas entries
  trm <- c( "AE1277106", "AE1394083", "AE-F130619", "ASU 70 480 1", "D-3598" ,
            "IN-EQW78", "IR5839" ,"methyl ester", "MON 0139", "None",  "sodium salt" )
  ppdb_idx <- ppdb_idx[!ppdb_idx$cas %in% trm, ]
  # remove duplicated cas entries
  ppdb_idx <- ppdb_idx[!duplicated(ppdb_idx$cas), ]

  # save(ppdb_idx, file = 'data/ppdb_idx.rda')
  return(ppdb_idx)
}


#' Query the ppdb for information
#'
#' This function queries the PPDB \url{http://sitem.herts.ac.uk/aeru/iupac/search.htm} for information.
#'
#' @import xml2
#' @importFrom rvest html_table
#' @importFrom stats rgamma
#'
#' @param cas character; CAS number to query.
#' @param verbose logical; print message during processing to console?
#' @param index A index object, as created by \code{\link{ppdb_buildidx}}.
#' If NULL (default), the index shipped with webchem is used \code{\link{ppdb_idx}}.
#' @return A list of 11 data.frames : ec_regulation, approved_in, general, parents, fate,
#' deg, soil, metab, etox, names and source_url.
#'
#' See also \url{http://sitem.herts.ac.uk/aeru/iupac/docs/Background_and_Support.pdf} for more information on the data
#'
#' @note Please read the Terms and Conditions for use: \url{http://sitem.herts.ac.uk/aeru/ppdb/en/docs/Conditions_of_use.pdf}.
#'
#' @author Eduard Szoecs, \email{eduardszoecs@@gmail.com}
#' @references Lewis, Kathleen A., John Tzilivakis, Douglas J. Warner, and Andrew Green 2016.
#' An International Database for Pesticide Risk Assessments and Management. Human and Ecological Risk Assessment: An International Journal
#' @export
#' @examples
#' \dontrun{
#' # might fail if Server is not available
#' gly <- ppdb_query('1071-83-6')
#' gly$approved_in
#'
#' # handle multiple CAS
#'  cas <- c('1071-83-6', '50-00-0')
#'  # check if these compounds are approved in germany
#'  foo <- function(y) {
#'    # query cas
#'    q <- ppdb_query(y)
#'    # extract status for germany
#'    q$approved_in$status[q$approved_in$country == 'DE']
#'  }
#'  sapply(cas, foo)
#' }
ppdb_query <- function(cas, verbose = TRUE, index = NULL){
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

  Sys.sleep( rgamma(1, shape = 15, scale = 1/10))
  ttt <- read_html(qurl, encoding = 'latin1')

  # ec regulation
  ec_regulation <- html_table(
    xml_find_all(ttt, "//p[contains(.,'EC Reg')]/following-sibling::table[1]"),
    header = TRUE)[[1]]

  # approved in contries
  status <- xml_attr(xml_find_all(ttt, "//*[contains(.,'Approved')]/following-sibling::table[1]/tr[2]/td/p/img"), 'src')
  approved_id <- data.frame(t(html_table(
    xml_find_all(ttt, "//p[contains(.,'Approved for use')]/following-sibling::table[1]"),
    header = TRUE)[[1]]), stringsAsFactors = FALSE)
  approved_id$status <- ifelse(status == 'tick.jpg', TRUE, FALSE)
  approved_id[ , 1] <- NULL
  approved_id$country <- rownames(approved_id)


  # general status
  general <- html_table(
    xml_find_all(ttt, "//p[contains(.,'General status')]/following-sibling::table[1]"),
    header = FALSE, fill = TRUE)[[1]]
  general <- general[ , 1:2]
  names(general) <- c('variable', 'value')

  # parents
  parents <- try(html_table(
    xml_find_all(ttt,  "//p[contains(.,'Can be a metabolite of')]/following-sibling::table[1]"),
    header = TRUE)[[1]],
    silent = TRUE)
  parents <- chk(parents)

#   # formulations
#   formulation <- tables[[8]]
#   names(formulation) <- c('variable', 'value')

  # fate
  fate <- try(html_table(
    xml_find_all(ttt,   "//p[contains(.,'ENVIRONMENTAL FATE')]/following-sibling::table[1]"),
    header = TRUE, fill = TRUE)[[1]],
    silent = TRUE)
  fate <- chk(fate)
  if (!length(fate) == 0) {
    take <- fate[!is.na(fate[ , 5]), ]
    take[ , 1] <- paste(take[ , 1], take[ , 2])
    take[ , 2] <- NULL
    fate <- fate[is.na(fate[ , 5]), 1:4]
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
  deg <- try(html_table(
    xml_find_all(ttt,   "//p[contains(.,'Degradation')]/following-sibling::table[1]"),
    header = TRUE, fill = TRUE)[[1]],
    silent = TRUE)
  deg <- chk(deg)
  if (!length(deg) == 0) {
    take <- deg[!is.na(deg[ , 5]), ]
    take[ , 1] <- paste(take[ , 1], take[ , 2])
    take[ , 2] <- NULL
    deg <- deg[is.na(deg[ , 5]), 1:4]
    nam <- c('Property', 'Value', 'Source/Quality', 'Interpretation')
    colnames(deg) <- nam
    colnames(take) <- nam
    deg <- rbind(deg, take)
    deg <- deg[!grepl('Note', deg[ , 'Property']), ]
    # if (any(deg[ , 'Value'] == 'Value')) {
    #   take <- deg[deg[ , 'Value'] == 'Value', ]
    #   take <- data.frame(take[ , -2], N = NA)
    #   colnames(take) <- colnames(deg)
    #   deg <- deg[!deg[ , 'Value'] == 'Value', ]
    #   deg <- rbind(deg, take)
    # }
  }



  # soil adsorption and mobility
  soil <- try(html_table(
    xml_find_all(ttt,   "//p[contains(.,'Soil adsorption')]/following-sibling::table[1]"),
    header = TRUE, fill = TRUE)[[1]],
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
  metab <- try(html_table(
    xml_find_all(ttt,   "//p[contains(.,'Key metabolites')]/following-sibling::table[1]"),
    header = TRUE, fill = TRUE)[[1]],
    silent = TRUE)
  metab <- chk(metab)

  # ecotoxicology
  etox <- try(html_table(
    xml_find_all(ttt,   "//p[contains(.,'ECOTOXICOLOGY')]/following-sibling::table[1]"),
    header = TRUE, fill = TRUE)[[1]],
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
  names <- try(html_table(
    xml_find_all(ttt,   "//p[contains(.,'TRANSLATIONS ')]/following-sibling::table[1]"),
    header = TRUE, fill = TRUE)[[1]],
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
       names = names,
       source_url = qurl
       )
  return(out)
}