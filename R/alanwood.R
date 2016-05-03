#' Query http://www.alanwood.net/pesticides
#'
#' Query Alan Woods Compendium of Pesticide Common Names \url{http://www.alanwood.net/pesticides}
#' @import xml2
#' @importFrom stats rgamma
#'
#' @param  query character; search string
#' @param type character; type of input ('cas' or 'commonname')
#' @param verbose logical; print message during processing to console?
#' @param idx data.frame; index to use. If NULL (default) the internal index \code{\link{aw_idx}} is used.
#' To rebuild the index use \code{\link{build_aw_idx}}.
#' @return A list of eight entries: common-name, status, preferredd IUPAC Name,
#'          IUPAC Name, cas, formula, activity, subactivity, inchikey, inchi and source url.
#'
#' @note for type = 'cas' only the first matched link is returned.
#' Please respect Copyright, Terms and Conditions \url{http://www.alanwood.net/pesticides/legal.html}!
#' @author Eduard Szoecs, \email{eduardszoecs@@gmail.com}
#' @export
#' @examples
#' \dontrun{
#' aw_query('Fluazinam', type = 'commonname')
#' out <- aw_query(c('Fluazinam', 'Diclofop'), type = 'com')
#' out
#' # extract subactivity from object
#' sapply(out, function(y) y$subactivity[1])
#'
#' # use CAS-numbers
#' aw_query("79622-59-6", type = 'cas')
#' }
aw_query <- function(query, type = c("commonname", "cas"), verbose = TRUE, idx = NULL){
  # query <- 'Fluazinam'
  foo <- function(query, type = c("commonname", "cas"), verbose, idx = NULL) {
    type <- match.arg(type)
    if (is.null(idx)) {
      idx <- webchem::aw_idx
    }
  # search links in indexes
    if (type == 'commonname' ) {
      links <- idx$links[idx$source == 'cn']
      names <- idx$linknames[idx$source == 'cn']
      cname <-  query
    }

    if (type == 'cas') {
      names <- idx$names[idx$source == 'rn']
      # select only first link
      links <- idx$links[idx$source == 'rn']
      linknames <- idx$linknames[idx$source == 'rn']
      cname <-  linknames[tolower(names) == tolower(query)]
    }

    takelink <- links[tolower(names) == tolower(query)]
    if (length(takelink) == 0) {
      message('Not found! Returning NA.\n')
      return(NA)
    }
    if (length(takelink) > 1) {
      takelink <- unique(takelink)
      if (length(takelink) > 1) {
        message('More then one link found! Returning first.\n')
        takelink <- takelink[1]
      }
    }
    if (verbose)
      message('Querying ', takelink)

    Sys.sleep( rgamma(1, shape = 15, scale = 1/10))
    ttt <- read_html(paste0('http://www.alanwood.net/pesticides/', takelink))

    status <- xml_text(xml_find_all(ttt, "//tr/th[@id='r1']/following-sibling::td"))
    pref_iupac_name <- xml_text(xml_find_all(ttt, "//tr/th[@id='r2']/following-sibling::td"))
    iupac_name <- xml_text(xml_find_all(ttt, "//tr/th[@id='r3']/following-sibling::td"))
    cas <- xml_text(xml_find_all(ttt, "//tr/th[@id='r5']/following-sibling::td"))
    formula <- xml_text(xml_find_all(ttt, "//tr/th[@id='r6']/following-sibling::td"))
    activity <- xml_text(xml_find_all(ttt, "//tr/th[@id='r7']/following-sibling::td"))
    subactivity <- trimws(strsplit(gsub('^.*\\((.*)\\)', '\\1', activity), ';')[[1]])
    activity <- gsub('^(.*) \\(.*\\)', '\\1', activity)
    inchikey_r <- xml_text(xml_find_all(ttt, "//tr/th[@id='r11']/following-sibling::td"))
    if (length(inchikey_r) == 0) {
      inchikey <- NA
    } else {
      if (grepl('isomer', inchikey_r)) {
        inchikey <- c(s_isomer = gsub('.*\\(S\\)-isomer:(.*)(minor component.*)', '\\1', inchikey_r),
          r_isomer = gsub('.*\\(R\\)-isomer:(.*)', '\\1', inchikey_r))
      }
      if (grepl('identifier', inchikey_r)) {
        inchikey <- c(gsub('(.*)identifier.*', '\\1', inchikey_r), gsub('.*identifier.*:(.*)', '\\1', inchikey_r))
        names(inchikey) <- c('inchikey',
                             gsub('.*(identifier.*:).*', '\\1', inchikey_r)
                             )
      }
      if (!grepl('isomer', inchikey_r) & !grepl('identifier', inchikey_r))
        inchikey <- inchikey_r
    }

    inchi <- xml_text(xml_find_all(ttt, "//tr/th[@id='r12']/following-sibling::td"))
    if (length(inchi) == 0) {
      inchi <- NA
    } else {
      if (grepl('isomer', inchi)) {
        inchi <- c(s_isomer = gsub('.*\\(S\\)-isomer:(.*)(minor component.*)', '\\1', inchi),
                   r_isomer = gsub('.*\\(R\\)-isomer:(.*)', '\\1', inchi))
      }
    }
    # add source url
    source_url <- paste0('http://www.alanwood.net/pesticides/', takelink)
    out <- list(cname = cname, status = status, pref_iupac_name = pref_iupac_name,
                iupac_name = iupac_name, cas = cas, formula = formula,
                activity = activity, subactivity = subactivity,
                inchikey = inchikey, inch = inchi, source_url = source_url)
    return(out)
  }
  out <- lapply(query, foo, type = type, verbose = verbose, idx = idx)
  out <- setNames(out, query)
  class(out) <- c('list', 'aw_query')
  return(out)
}


#' Function to build index
#'
#' @import xml2
#' @return a data.frame with three columns: cas, url, and name
#' @export
#' @seealso \code{\link{aw_query}}, \code{\link{aw_idx}}
#'@author Eduard Szoecs, \email{eduardszoecs@@gmail.com}
build_aw_idx <- function(){
  idx1 <- read_html('http://www.alanwood.net/pesticides/index_rn.html')
  idx2 <- read_html('http://www.alanwood.net/pesticides/index_rn1.html')
  idx3 <- read_html('http://www.alanwood.net/pesticides/index_rn2.html')
  prep_idx <- function(y) {
    names <- xml_text(xml_find_all(y, "//dl/dt"))
    links <- xml_attr(xml_find_all(y, '//dt/following-sibling::dd[1]/a[1]'), 'href')
    linknames <- xml_text(xml_find_all(y, '//dt/following-sibling::dd[1]/a[1]'))
    return(data.frame(names, links, linknames, stringsAsFactors = FALSE))
  }
  aw_idx <- rbind(prep_idx(idx1), prep_idx(idx2) ,prep_idx(idx3))
  aw_idx[['source']] <- 'rn'
  idx4 <- read_html('http://www.alanwood.net/pesticides/index_cn.html')
  n <- xml_find_all(idx4, '//a')
  names <- xml_text(n)
  rm <- names == ''
  names <- names[!rm]
  links <- xml_attr(n, 'href')
  links <- links[!rm]
  idx4 <- data.frame(names = NA, links = links, linknames = names,
                     source = 'cn', stringsAsFactors = FALSE)
  aw_idx <- rbind(aw_idx, idx4)

  # fix encoding
  ln <- aw_idx$linknames
  Encoding(ln) <- 'latin1'
  ln <- iconv(ln, from = 'latin1', to = 'ASCII', sub = '')
  aw_idx$linknames <- ln
  attr(aw_idx, 'date') <- Sys.Date()

  # save(aw_idx, file = 'data/aw_idx.rda')
  return(aw_idx)
}