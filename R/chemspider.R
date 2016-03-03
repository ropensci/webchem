#' Retrieve ChemSpider ID
#'
#' Return Chemspider ID (CSID) for a search query, see \url{http://www.chemspider.com/}.
#' @import xml2
#' @importFrom stats rgamma
#'
#' @param query charachter; search term.
#' @param token character; your security token.
#' @param first logical; If TRUE (default) return only first result.
#' @param verbose logical; should a verbose output be printed on the console?
#' @param ... currently not used.
#' @return if first = TRUE a character vector with ChemSpider IDs, otherwise a list.
#'
#' @note A security token is neeeded. Please register at RSC.
#' \url{https://www.rsc.org/rsc-id/register}
#' for a security token.
#' Please respect the Terms & conditions \url{http://www.rsc.org/help-legal/legal/terms-conditions/}.
#'
#' @author Eduard Szoecs, \email{eduardszoecs@@gmail.com}
#' @seealso \code{\link{cs_compinfo}} and \code{\link{cs_extcompinfo}} to
#' retrieve compound details from csid.
#' @export
#' @examples
#' \dontrun{
#' # Fails because no TOKEN is included
#' token <- '<YOUR-SECURITY-TOKEN>'
#' get_csid("Triclosan", token = token)[[1]]
#' # [1] "5363"
#' get_csid(c("Triclosan", "50-00-0"), token = token)
#' }
get_csid <- function(query, token = NULL, first = TRUE, verbose = TRUE,  ...){
  # token = '37bf5e57-9091-42f5-9274-650a64398aaf'
  foo <- function(query, token, first, verbose, ...){
    if (is.na(query))
      return(NA)
    baseurl <- 'http://www.chemspider.com/Search.asmx/SimpleSearch?'
    qurl <- paste0(baseurl, 'query=', query, '&token=', token)
    if (verbose)
      message(qurl, '\n')
    Sys.sleep( rgamma(1, shape = 15, scale = 1/10))
    h <- try(read_xml(qurl), silent = TRUE)
    if (inherits(h, "try-error")) {
      warning('Problem with web service encountered... Returning NA.')
      return(NA)
    }
    out <- xml_text(xml_find_all(h, './/*'))
    if (length(out) == 0) {
      message('No csid found... Returning NA.')
      return(NA)
    }
    if (first)
      out <- out[1]
    names(out) <- NULL
    return(out)
  }
  out <- lapply(query, foo, token = token, first = first, verbose = verbose)
  out <- setNames(out, query)
  if (first)
    out <- unlist(out)
  return(out)
}



#' Get record details (CSID, StdInChIKey, StdInChI, SMILES) by ChemSpider ID
#'
#' Get record details from ChemspiderId (CSID), see \url{http://www.chemspider.com/}
#' @import xml2
#' @importFrom stats rgamma
#'
#' @param csid character, ChemSpider ID.
#' @param token character; security token.
#' @param verbose logical; should a verbose output be printed on the console?
#' @param ... currently not used.
#' @return a data.frame with 5 colums csid (ChemSpider ID), inchi,
#'   inchikey, smiles, source_url and the query
#'
#' @note A security token is neeeded. Please register at RSC
#' \url{https://www.rsc.org/rsc-id/register}
#' for a security token.
#' Please respect the Terms & conditions \url{http://www.rsc.org/help-legal/legal/terms-conditions/}.
#' @author Eduard Szoecs, \email{eduardszoecs@@gmail.com}
#' @seealso \code{\link{get_csid}} to retrieve ChemSpider IDs,
#' \code{\link{cs_extcompinfo}} for extended compound information.
#' @export
#' @examples
#' \dontrun{
#' # Fails because no TOKEN is included
#' token <- '<YOUR-SECURITY-TOKEN>'
#' # convert CAS to CSID
#' csid <- get_csid("Triclosan", token = token)
#' cs_compinfo(csid, token)
#'
#' csids <- get_csid(c('Aspirin', 'Triclosan'), token = token)
#' cs_compinfo(csids, token = token)
#' }
cs_compinfo <- function(csid, token, verbose = TRUE, ...){
  # csid <- "5363"
  foo <- function(csid, token, verbose) {
    if (is.na(csid)) {
      return(list(csid = NA, inchi = NA, inchikey = NA, smiles = NA, source_url = NA))
    }
    baseurl <- 'http://www.chemspider.com/Search.asmx/GetCompoundInfo?'
    qurl <- paste0(baseurl, 'CSID=', csid, '&token=', token)
    if (verbose)
      message(qurl)
    Sys.sleep( rgamma(1, shape = 5, scale = 1/10))
    h <- try(read_xml(qurl), silent = TRUE)
    if (inherits(h, "try-error")) {
      warning('CSID not found... Returning NA.')
      return(NA)
    }
    out <- as.list(xml_text(xml_children(h)))
    names(out) <- c('csid', 'inchi', 'inchikey', 'smiles')
    source_url <- paste0('http://www.chemspider.com/Chemical-Structure.', csid, '.html')
    out[['source_url']] <- source_url
    return(out)
  }
  out <- sapply(csid, foo, token = token, verbose = verbose)
  out <- data.frame(t(out), row.names = seq_len(ncol(out)))
  out[['query']] <- rownames(out)
  out <- data.frame(t(apply(out, 1, unlist)), stringsAsFactors = FALSE)
  class(out) <- c('data.frame', 'cs_compinfo')
  return(out)
}



#' Get extended record details by ChemSpider ID
#'
#' Get extended info from Chemspider, see \url{http://www.chemspider.com/}
#' @import xml2
#' @importFrom stats rgamma
#' @param csid character,  ChemSpider ID.
#' @param token character; security token.
#' @param verbose logical; should a verbose output be printed on the console?
#' @param ... currently not used.
#' @return a data.frame with entries: 'csid', 'mf' (molecular formula), 'smiles', 'inchi' (non-standard),
#' 'inchikey' (non-standard), 'average_mass', 'mw' (Molecular weight), 'monoiso_mass' (MonoisotopicMass),
#' 'nominal_mass', 'alogp', 'xlogp', 'common_name' and 'source_url'
#' @note A security token is neeeded. Please register at RSC
#' \url{https://www.rsc.org/rsc-id/register}
#' for a security token.
#' Please respect the Terms & conditions \url{http://www.rsc.org/help-legal/legal/terms-conditions/}.
#' @author Eduard Szoecs, \email{eduardszoecs@@gmail.com}
#' @seealso \code{\link{get_csid}} to retrieve ChemSpider IDs,
#' \code{\link{cs_compinfo}} for extended compound information.
#' @note use \code{\link{cs_compinfo}} to retrieve standard inchikey.
#' @export
#' @examples
#' \dontrun{
#' # Fails because no TOKEN is included
#' token <- '<YOUR-SECURITY-TOKEN>'
#' # convert CAS to CSID
#' csid <- get_csid("Triclosan", token = token)
#' cs_extcompinfo(csid, token)
#'
#' csids <- get_csid(c('Aspirin', 'Triclosan'), token = token)
#' cs_compinfo(csids, token = token)
#' }
cs_extcompinfo <- function(csid, token, verbose = TRUE, ...){
  # csid <- "5363"
  foo <- function(csid, token, verbose) {
    if (is.na(csid)) {
      out <- as.list(rep(NA, 13))
      names(out) <- c('csid', 'mf', 'smiles', 'inchi', 'inchikey', 'average_mass',
                      'mw', 'monoiso_mass', 'nominal_mass', 'alogp', 'xlogp', 'common_name', 'source_url')
      return(out)
    }
    baseurl <- 'http://www.chemspider.com/MassSpecAPI.asmx/GetExtendedCompoundInfo?'
    qurl <- paste0(baseurl, 'CSID=', csid, '&token=', token)
    if (verbose)
      message(qurl)
    Sys.sleep( rgamma(1, shape = 5, scale = 1/10))
    h <- try(read_xml(qurl), silent = TRUE)
    if (inherits(h, "try-error")) {
      warning('CSID not found... Returning NA.')
      return(NA)
    }
    out <- as.list(xml_text(xml_children(h)))
    names(out) <- c('csid', 'mf', 'smiles', 'inchi', 'inchikey', 'average_mass',
                    'mw', 'monoiso_mass', 'nominal_mass', 'alogp', 'xlogp', 'common_name')
    # convert to numeric
    out[['average_mass']] <- as.numeric(out[['average_mass']])
    out[['mw']] <- as.numeric(out[['mw']])
    out[['monoiso_mass']] <- as.numeric(out[['monoiso_mass']])
    out[['nominal_mass']] <- as.numeric(out[['nominal_mass']])
    out[['alogp']] <- as.numeric(out[['alogp']])
    out[['xlogp']] <- as.numeric(out[['xlogp']])
    source_url <- paste0('http://www.chemspider.com/Chemical-Structure.', csid, '.html')
    out[['source_url']] <- source_url
    return(out)
  }
  out <- sapply(csid, foo, token = token, verbose = verbose)
  out <- data.frame(t(out), row.names = seq_len(ncol(out)))
  out[['query']] <- rownames(out)
  out <- data.frame(t(apply(out, 1, unlist)), stringsAsFactors = FALSE)
  class(out) <- c('data.frame', 'cs_extcompinfo')
  return(out)
}

#' Get predicted chemical properties from ChemSpider
#'
#' Get predicted (ACD/Labs and EPISuite) chemical properties from ChemSpider,
#' see \url{http://www.chemspider.com/}
#' @import xml2 stringr
#' @importFrom stats rgamma
#'
#' @param csid character,  ChemSpider ID.
#' @param verbose logical; should a verbose output be printed on the console?
#' @param ... currently not used.
#'
#' @return A list of lists with of three: acd (data.frame), epi (data.frame) and source_url.
#'
#' @note Please respect the Terms & conditions \url{http://www.rsc.org/help-legal/legal/terms-conditions/}.
#' @author Eduard Szoecs, \email{eduardszoecs@@gmail.com}
#' @seealso \code{\link{get_csid}} to retrieve ChemSpider IDs,
#' \code{\link{cs_compinfo}} for extended compound information.
#' @export
#' @examples
#' \dontrun{
#' out <- cs_prop(5363)
#' out[[1]]$epi
#'
#' out2 <- cs_prop(c(5363, 2157))
#' # extract Log Octanol-Water Partition Coef from EPI
#' sapply(out2, function(y){
#'   y$epi$value_pred[y$epi$prop == 'Log Octanol-Water Partition Coef']
#' })
#' }
cs_prop <- function(csid, verbose = TRUE, ...){
  foo <- function(csid, verbose){
    qurl <- paste0('http://www.chemspider.com/Chemical-Structure.', csid, '.html')
    if (verbose)
      message(qurl)
    Sys.sleep( rgamma(1, shape = 5, scale = 1/10))
    h <- try(read_html(qurl), silent = TRUE)
    if (inherits(h, "try-error")) {
      warning('CSID not found... Returning NA.')
      return(NA)
    }

    ### acd
    acd <- do.call(rbind, html_table(xml_find_all(h, '//div[@class="column two"]/table')))
    names(acd) <- c('variable', 'val')
    acd$variable <- gsub('^(.*)\\:$', '\\1', acd$variable)

    # ^ - Beginning of the line;
    # \\d* - 0 or more digits;
    # \\.? - An optional dot (escaped, because in regex, . is a special character);
    # \\d* - 0 or more digits (the decimal part);
    # $ - End of the line.
    acd$value <- as.numeric(gsub('^(\\d*\\.?\\d*).*$', '\\1', acd$val))
    acd$error <- as.numeric(ifelse(grepl('\u00B1' , acd$val), gsub('^\\d*\\.?\\d*\u00B1(\\d*\\.?\\d*)\\s.*$', '\\1', acd$val), NA))
    acd$unit <- ifelse(grepl('\\s.*\\d*$', acd$val),
           gsub('^.*\\d*\\s(.*\\d*)$', '\\1', acd$val),
           NA)
    acd$val <- NULL


    ### episuite
    epi <- data.frame(property = character(),
                      value_pred = numeric(),
                      unit_pred = character(),
                      source_pred = character(),
                      value_exp = numeric(),
                      unit_exp = character(),
                      source_exp = character())


    kow_raw <- xml_text(xml_find_all(h, '//div[@id="epiTab"]/pre'))
    ll <- str_split(kow_raw, '\n')
    ll <- sapply(ll, str_trim)
    ll <- ll[!ll == '']

    prop <- 'Log Octanol-Water Partition Coef'
    value_pred <- as.numeric(gsub('.* = \\s(.*)','\\1', ll[grepl('^Log Kow \\(KOWW', ll)]))
    unit_pred <- NA
    source_pred <- gsub('(.*) = \\s(.*)','\\1', ll[grepl('^Log Kow \\(KOWW', ll)])
    value_exp <- as.numeric(gsub('.* = \\s(.*)','\\1', ll[grepl('^Log Kow \\(Exper.', ll)]))
    unit_exp <- NA
    source_exp <- gsub('^.*\\:\\s(.*)','\\1', ll[which(grepl('^Log Kow \\(Exper.', ll)) + 1])

    prop <- c(prop, 'Boiling Point')
    value_pred <- c(value_pred, as.numeric(gsub('.*:\\s+([-+]?[0-9]*\\.?[0-9]+).*','\\1', ll[grepl('^Boiling Pt \\(deg C', ll)])))
    unit_pred <- c(unit_pred, 'deg C')
    source_pred <- c(source_pred, gsub('^.*\\((.*)\\)\\:$','\\1', ll[grepl('^Boiling Pt, ', ll)]))
    value_exp <- c(value_exp,   NA)
    unit_exp <- c(unit_exp, NA)
    source_exp <- c(source_exp, NA)

    prop <- c(prop, 'Melting Point')
    value_pred <- c(value_pred, as.numeric(gsub('.*:\\s+([-+]?[0-9]*\\.?[0-9]+).*','\\1', ll[grepl('^Melting Pt \\(deg C', ll)])))
    unit_pred <- c(unit_pred, 'deg C')
    source_pred <- c(source_pred, gsub('^.*\\((.*)\\)\\:$','\\1', ll[grepl('^Boiling Pt, ', ll)]))
    value_exp <- c(value_exp,   NA)
    unit_exp <- c(unit_exp, NA)
    source_exp <- c(source_exp, NA)
    # epi_mp_exp <- as.numeric(gsub('.*:\\s+([-+]?[0-9]*\\.?[0-9]+).*','\\1', ll[grepl('^MP\\s+\\(exp', ll)]))
    # epi_bp_exp <- as.numeric(gsub('.*:\\s+([-+]?[0-9]*\\.?[0-9]+).*','\\1', ll[grepl('^BP\\s+\\(exp', ll)]))

    prop <- c(prop, 'Water Solubility from KOW')
    value_pred <- c(value_pred, as.numeric(gsub('.*:\\s+([-+]?[0-9]*\\.?[0-9]+).*','\\1', ll[grepl('^Water Solubility at 25 deg C', ll)])))
    unit_pred <- c(unit_pred, 'mg/L (25 deg C)')
    source_pred <- c(source_pred, gsub('^.*\\((.*)\\)\\:$','\\1', ll[grepl('^Water Solubility Estimate from Log Kow', ll)]))
    value_exp <- c(value_exp,   as.numeric(gsub('.*=\\s+([-+]?[0-9]*\\.?[0-9]+).*','\\1', ll[grepl('^Water Sol \\(Exper. database match', ll)])))
    unit_exp <- c(unit_exp, gsub('.*=\\s+([-+]?[0-9]*\\.?[0-9]+)(.*)','\\2', ll[grepl('^Water Sol \\(Exper. database match', ll)]))
    source_exp <- c(source_exp, gsub('^.*\\:\\s(.*)','\\1', ll[which(grepl('^Water Sol \\(Exper. database match', ll)) + 1]))

    prop <- c(prop, 'Water Solubility from Fragments')
    value_pred <- c(value_pred, as.numeric(gsub('.*=\\s+([-+]?[0-9]*\\.?[0-9]+).*','\\1', ll[grepl('^Wat Sol \\(v1.01', ll)])))
    unit_pred <- c(unit_pred, 'mg/L')
    source_pred <- c(source_pred, NA)
    value_exp <- c(value_exp,  NA)
    unit_exp <- c(unit_exp, NA)
    source_exp <- c(source_exp, NA)

    prop <- c(prop, 'Log Octanol-Air Partition Coefficient (25 deg C)')
    value_pred <- c(value_pred, as.numeric(gsub('.*:\\s(.*)','\\1', ll[grepl('^Log Koa \\(KOAWIN', ll)])))
    unit_pred <- c(unit_pred, NA)
    source_pred <- c(source_pred, gsub('^.*\\[(.*)\\]\\:$','\\1', ll[grepl('^Log Octanol-Air Partition Coefficient', ll)]))
    value_exp <- c(value_exp,   as.numeric(gsub('^.*\\:(.*)','\\1', ll[grepl('^Log Koa \\(experimental database\\).*', ll)])))
    unit_exp <- c(unit_exp, NA)
    source_exp <- c(source_exp, NA)

    prop <- c(prop, 'Log Soil Adsorption Coefficient')
    value_pred <- c(value_pred, as.numeric(gsub('.*:\\s+([-+]?[0-9]*\\.?[0-9]+).*','\\1', ll[grepl('^Log Koc:', ll)])))
    unit_pred <- c(unit_pred, NA)
    source_pred <- c(source_pred, gsub('^.*\\((.*)\\)\\:$','\\1', ll[grepl('^Soil Adsorption Coefficient', ll)]))
    value_exp <- c(value_exp, NA)
    unit_exp <- c(unit_exp, NA)
    source_exp <- c(source_exp, NA)

    epi <- data.frame(prop, value_pred, unit_pred, source_pred,
                      value_exp, unit_exp, source_exp, stringsAsFactors = FALSE)


    out <- list(acd = acd,
                epi = epi,
                source_url = qurl)
    return(out)
  }
  out <- lapply(csid, foo, verbose = verbose)
  out <- setNames(out, csid)
  return(out)
}


#' Convert identifiers using ChemSpider
#'
#' @param query character; query ID.
#' @param from character; type of query ID.
#' @param to character; type to convert to.
#' @param token character; security token. Converting from csid to mol requires a token.
#' @param verbose logical; should a verbose output be printed on the console?
#' @param ... futher arguments passed. Currently onl \code{parse}, see also \code{\link{cs_csid_mol}}
#' @return Depends on to. if \code{to = 'mol'} then an RMol-Object, else a character string.
#'
#' @note A security token is neeeded for conversion to mol. Please register at RSC
#' \url{https://www.rsc.org/rsc-id/register} for a security token.
#'
#' @author Eduard Szoecs, \email{eduardszoecs@@gmail.com}
#' @export
#'
#' @seealso  There are many low level functions underlying, which are exported from
#' the package. The naming scheme is cs_from_to() here's a list and links to their manual pages:
#'\itemize{
#'  \item \code{\link{cs_csid_mol}}
#'  \item \code{\link{cs_inchikey_csid}}
#'  \item \code{\link{cs_inchikey_inchi}}
#'  \item \code{\link{cs_inchikey_mol}}
#'  \item \code{\link{cs_inchi_csid}}
#'  \item \code{\link{cs_inchi_inchikey}}
#'  \item \code{\link{cs_inchi_mol}}
#'  \item \code{\link{cs_inchi_smiles}}
#'  \item \code{\link{cs_smiles_inchi}}
#' }
#' Check \code{\link{parse_mol}} for a description of the Mol R Object.
#'
#' @examples
#' \donttest{
#' # might fail if API is not available
#' cs_convert('BQJCRHHNABKAKU-KBQPJGBKSA-N', from = 'inchikey', to = 'csid')
#' cs_convert(c('BQJCRHHNABKAKU-KBQPJGBKSA-N', 'BQJCRHHNABKAKU-KBQPJGBKSA-N'),
#'     from = 'inchikey', to = 'csid')
#' cs_convert('BQJCRHHNABKAKU-KBQPJGBKSA-N', from = 'inchikey', to = 'inchi')
#' cs_convert('BQJCRHHNABKAKU-KBQPJGBKSA-N', from = 'inchikey', to = 'mol')
#'}
cs_convert <- function(query, from = c('csid', 'inchikey', 'inchi', 'smiles'),
                       to = c('csid', 'inchikey', 'inchi', 'smiles', 'mol'),
                       verbose = TRUE, token = NULL, ...) {
  from <- match.arg(from)
  to <- match.arg(to)
  from_to <- paste(from, to , sep = '_')
  if (from_to == 'csid_mol' & is.null(token)) {
    stop('Need token for this conversion!')
  }
  # allowed combinations
  comb <- c('csid_mol', 'inchikey_csid', 'inchikey_inchi', 'inchikey_mol',
            'inchi_csid', 'inchi_inchikey', 'inchi_mol', 'inchi_smiles','smiles_inchi')
  if (!from_to %in% comb) {
    stop('Conversion from ', from, ' to ', to, ' currently not supported')
  }
  foo <- function(query, from_to, verbose, token, ...){
    out <- switch(from_to,
           csid_mol = cs_csid_mol(csid = query, token = token, verbose = verbose, ...),
           inchikey_csid = cs_inchikey_csid(inchikey = query, verbose = verbose, ...),
           inchikey_inchi = cs_inchikey_inchi(inchikey = query, verbose = verbose, ...),
           inchikey_mol = cs_inchikey_mol(inchikey = query, verbose = verbose, ...),
           inchi_csid = cs_inchi_csid(inchi = query, verbose = verbose, ...),
           inchi_inchikey = cs_inchi_inchikey(inchi = query, verbose = verbose, ...),
           inchi_mol = cs_inchi_mol(inchi = query, verbose = verbose, ...),
           inchi_smiles = cs_inchi_smiles(inchi = query, verbose = verbose, ...),
           smiles_inchi = cs_smiles_inchi(smiles = query, verbose = verbose, ...)
           )
    return(out)
  }
  res <- lapply(query, foo, from_to = from_to, verbose = verbose, token = token, ...)
  return(res)
}



#' Convert a CSID to a Molfile
#' @import xml2
#' @importFrom stats rgamma
#'
#' @param csid character,  ChemSpiderID.
#' @param token character; security token.
#' @param parse should the molfile be parsed to a R object?
#' If \code{FALSE} the raw mol is returned as string.
#' @param verbose logical; should a verbose output be printed on the console?
#' @param ... currently not used.
#'
#' @return If parse = FALSE then a charactersting,
#'   else a RMol-object (from \code{\link{parse_mol}})
#'
#' @seealso This is a low level function. Please see \code{\link{cs_convert}} for the top level function.
#'
#' @note A security token is neeeded. Please register at RSC
#' \url{https://www.rsc.org/rsc-id/register}
#' for a security token.
#' @author Eduard Szoecs, \email{eduardszoecs@@gmail.com}
#' @export
#' @examples
#' \dontrun{
#' # Fails because no TOKEN is included
#' token <- '<YOUR-SECURITY-TOKEN>'
#' # convert CAS to CSID
#' tric_mol <- cs_csid_mol(5363, token = token)
#' tric_mol
#' cs_csid_mol(5363, token = token, parse = FALSE)
#' }
cs_csid_mol <- function(csid, token, parse = TRUE, verbose = TRUE, ...){
  if (length(csid) > 1) {
    stop('Cannot handle multiple input strings.')
  }
  baseurl <- 'http://www.chemspider.com/InChI.asmx/CSIDToMol?'
  qurl <- paste0(baseurl, 'csid=', csid, '&token=', token)
  if (verbose)
    message(qurl)
  Sys.sleep( rgamma(1, shape = 5, scale = 1/10))
  h <- try(read_xml(qurl), silent = TRUE)
  if (inherits(h, "try-error")) {
    warning('CSID not found... Returning NA.')
    out <- NA
  } else {
    mol <- xml_text(h)
    if (!parse) {
      out <- mol
    } else {
      out <- parse_mol(mol)
    }
  }
  return(out)
}



#' Convert a InChIKey to CSID
#' @import xml2
#' @importFrom stats rgamma
#'
#' @param inchikey character,  InChIKey
#' @param verbose logical; should a verbose output be printed on the console?
#' @param ... currently not used.
#'
#' @return A CSID.
#'
#' @seealso This is a low level function. Please see \code{\link{cs_convert}} for the top level function.
#'
#' @author Eduard Szoecs, \email{eduardszoecs@@gmail.com}
#' @export
#' @examples
#' \donttest{
#' # might fail if API is not available
#' # convert CAS to CSID
#' cs_inchikey_csid('BQJCRHHNABKAKU-KBQPJGBKSA-N')
#' }
cs_inchikey_csid <- function(inchikey, verbose = TRUE, ...){
  # inchikey <- 'BQJCRHHNABKAKU-KBQPJGBKSA-N'
  # inchikey <- 'KYOUEHWYDNYHAL-IOORBXIBSA-N'
  if (length(inchikey) > 1) {
    stop('Cannot handle multiple input strings.')
  }
  baseurl <- 'http://www.chemspider.com/InChI.asmx/InChIKeyToCSID?'
  qurl <- paste0(baseurl, 'inchi_key=', inchikey)
  if (verbose)
    message(qurl)
  Sys.sleep( rgamma(1, shape = 5, scale = 1/10))
  h <- try(read_xml(qurl), silent = TRUE)
  if (inherits(h, "try-error")) {
    warning('inchikey not found... Returning NA.')
    return(NA)
  } else {
    out <- xml_text(h)
  }
  if (out == '') {
    warning('inchikey not found... Returning NA.')
    return(NA)
  }
  return(out)
}


#' Convert a InChIKey to InChI
#' @import xml2
#' @importFrom stats rgamma
#'
#' @param inchikey character,  InChIKey
#' @param verbose logical; should a verbose output be printed on the console?
#' @param ... currently not used.
#' @return character; InChI
#'
#' @seealso This is a low level function. Please see \code{\link{cs_convert}} for the top level function.
#'
#' @author Eduard Szoecs, \email{eduardszoecs@@gmail.com}
#' @export
#' @examples
#' \donttest{
#' # might fail if API is not available
#' cs_inchikey_inchi('BQJCRHHNABKAKU-KBQPJGBKSA-N')
#' }
cs_inchikey_inchi <- function(inchikey, verbose = TRUE, ...){
  # inchikey <- 'BQJCRHHNABKAKU-KBQPJGBKSA-N'
  if (length(inchikey) > 1) {
    stop('Cannot handle multiple input strings.')
  }
  baseurl <- 'http://www.chemspider.com/InChI.asmx/InChIKeyToInChI?'
  qurl <- paste0(baseurl, 'inchi_key=', inchikey)
  if (verbose)
    message(qurl)
  Sys.sleep( rgamma(1, shape = 5, scale = 1/10))
  h <- try(read_xml(qurl), silent = TRUE)
  if (inherits(h, "try-error")) {
    warning('inchikey not found... Returning NA.')
    out <- NA
  } else {
    out <- xml_text(h)
  }
  return(out)
}


#' Convert a InChIkey to a Molfile
#' @import xml2
#' @importFrom stats rgamma
#'
#' @param inchikey character,  A InChIKey.
#' @param parse should the molfile be parsed to a R object?
#' If \code{FALSE} the raw mol is returned as string.
#' @param verbose logical; should a verbose output be printed on the console?
#' @param ... currently not used.
#'
#' @return If parse = FALSE then a charactersting,
#'   else a RMol-object (from \code{\link{parse_mol}})
#'
#' @seealso This is a low level function. Please see \code{\link{cs_convert}} for the top level function.
#' @author Eduard Szoecs, \email{eduardszoecs@@gmail.com}
#' @export
#' @examples
#' \donttest{
#' # might fail if API is not available
#' tric_mol <- cs_inchikey_mol('BQJCRHHNABKAKU-KBQPJGBKSA-N')
#' tric_mol
#' cs_inchikey_mol('BQJCRHHNABKAKU-KBQPJGBKSA-N',parse = FALSE)
#' }
cs_inchikey_mol <- function(inchikey, parse = TRUE, verbose = TRUE, ...){
  # inchikey <- 'BQJCRHHNABKAKU-KBQPJGBKSA-N'
  if (length(inchikey) > 1) {
    stop('Cannot handle multiple input strings.')
  }
  baseurl <- 'http://www.chemspider.com/InChI.asmx/InChIKeyToMol?'
  qurl <- paste0(baseurl, 'inchi_key=', inchikey)
  if (verbose)
    message(qurl)
  Sys.sleep( rgamma(1, shape = 5, scale = 1/10))
  h <- try(read_xml(qurl), silent = TRUE)
  if (inherits(h, "try-error")) {
    warning('inchikey not found... Returning NA.')
    out <- NA
  } else {
    mol <- xml_text(h)
    if (!parse) {
      out <- mol
    } else {
      out <- parse_mol(mol)
    }
  }
  return(out)
}


#' Convert a InChI to CSID
#' @import xml2 httr
#' @importFrom stats rgamma
#'
#' @param inchi character,  InChI
#' @param verbose logical; should a verbose output be printed on the console?
#' @param ... currently not used.
#'
#' @return A CSID.
#'
#' @seealso This is a low level function. Please see \code{\link{cs_convert}} for the top level function.
#'
#' @author Eduard Szoecs, \email{eduardszoecs@@gmail.com}
#' @export
#' @examples
#' \donttest{
#' # might fail if API is not available
#' inchi <-  "InChI=1S/C17H19NO3/c1-18-7-6-17-10-3-5-13(20)16(17)21-15-12(19)4-
#' 2-9(14(15)17)8-11(10)18/h2-5,10-11,13,16,19-20H,6-8H2,1H3/t10-,11+,13-,16-,17-/m0/s1"
#' # convert InChI to CSID
#' cs_inchi_csid(inchi)
#' }
cs_inchi_csid <- function(inchi, verbose = TRUE, ...){
  if (length(inchi) > 1) {
    stop('Cannot handle multiple input strings.')
  }
  baseurl <- 'http://www.chemspider.com/InChI.asmx/InChIToCSID'
  if (verbose)
    message('Querrying ', baseurl)
  Sys.sleep( rgamma(1, shape = 5, scale = 1/10))
  res <- try(POST(baseurl, body = list(inchi = inchi), encode = 'form'),
             silent = TRUE)
  if (inherits(res, "try-error")) {
    warning('Problem with service... Returning NA.')
    out <- NA
  } else {
    out <- try(read_xml(content(res, 'raw')), silent = TRUE)
    if (inherits(out, "try-error")) {
      warning('inchi not found... Returning NA.')
      out <- NA
    } else {
      out <- xml_text(out)
    }
  }
  return(out)
}


#' Convert a InChI to InChiKey
#' @import xml2 httr
#' @importFrom stats rgamma
#'
#' @param inchi character,  InChI
#' @param verbose logical; should a verbose output be printed on the console?
#' @param ... currently not used.
#'
#' @return A InChiKey.
#'
#' @seealso This is a low level function. Please see \code{\link{cs_convert}} for the top level function.
#'
#' @author Eduard Szoecs, \email{eduardszoecs@@gmail.com}
#' @export
#' @examples
#' \donttest{
#' # might fail if API is not available
#' inchi <-  "InChI=1S/C17H19NO3/c1-18-7-6-17-10-3-5-13(20)16(17)21-15-12(19)4-
#' 2-9(14(15)17)8-11(10)18/h2-5,10-11,13,16,19-20H,6-8H2,1H3/t10-,11+,13-,16-,17-/m0/s1"
#' # convert InChI to CSID
#' cs_inchi_inchikey(inchi)
#' }
cs_inchi_inchikey <- function(inchi, verbose = TRUE, ...){
  if (length(inchi) > 1) {
    stop('Cannot handle multiple input strings.')
  }
  baseurl <- 'http://www.chemspider.com/InChI.asmx/InChIToInChIKey'
  if (verbose)
    message('Querrying ', baseurl)
  Sys.sleep( rgamma(1, shape = 5, scale = 1/10))
  res <- try(POST(baseurl, body = list(inchi = inchi), encode = 'form'),
             silent = TRUE)
  if (inherits(res, "try-error")) {
    warning('Problem with service... Returning NA.')
    out <- NA
  } else {
    out <- try(read_xml(content(res, 'raw')), silent = TRUE)
    if (inherits(out, "try-error")) {
      warning('inchi not found... Returning NA.')
      out <- NA
    } else {
      out <- xml_text(out)
    }
  }
  return(out)
}



#' Convert a InChI to Molfile
#' @import xml2 httr
#' @importFrom stats rgamma
#'
#' @param inchi character,  InChI
#' @param parse should the molfile be parsed to a R object?
#' If \code{FALSE} the raw mol is returned as string.
#' @param verbose logical; should a verbose output be printed on the console?
#' @param ... currently not used.
#'
#' @return If parse = FALSE then a charactersting,
#'   else a RMol-object (from \code{\link{parse_mol}})
#'
#' @seealso This is a low level function. Please see \code{\link{cs_convert}} for the top level function.
#'
#' @author Eduard Szoecs, \email{eduardszoecs@@gmail.com}
#' @export
#' @examples
#' \donttest{
#' # might fail if API is not available
#' inchi <-  paste0("InChI=1S/C17H19NO3/c1-18-7-6-17-10-3-5-13(20)16(17)21-15-12(19)4-",
#' "2-9(14(15)17)8-11(10)18/h2-5,10-11,13,16,19-20H,6-8H2,1H3/t10-,11+,13-,16-,17-/m0/s1")
#' # convert InChI to CSID
#' cs_inchi_mol(inchi)
#' cs_inchi_mol(inchi, parse = FALSE)
#' }
cs_inchi_mol <- function(inchi, parse = TRUE, verbose = TRUE, ...){
  if (length(inchi) > 1) {
    stop('Cannot handle multiple input strings.')
  }
  baseurl <- 'http://www.chemspider.com/InChI.asmx/InChIToMol'
  if (verbose)
    message('Querrying ', baseurl)
  Sys.sleep( rgamma(1, shape = 5, scale = 1/10))
  res <- try(POST(baseurl, body = list(inchi = inchi), encode = 'form'),
             silent = TRUE)
  if (inherits(res, "try-error")) {
    warning('Problem with service... Returning NA.')
    out <- NA
  } else {
    out <- try(read_xml(content(res, 'raw')), silent = TRUE)
    if (inherits(out, "try-error")) {
      warning('inchi not found... Returning NA.')
      out <- NA
    } else {
      out <- xml_text(out)
      if (parse) {
        out <- parse_mol(out)
      }
    }
  }
  return(out)
}


#' Convert a InChI to SMILES
#' @import xml2 httr
#' @importFrom stats rgamma
#'
#' @param inchi character,  InChI
#' @param verbose logical; should a verbose output be printed on the console?
#' @param ... currently not used.
#'
#' @return A SMILES string.
#'
#' @seealso This is a low level function. Please see \code{\link{cs_convert}} for the top level function.
#'
#' @author Eduard Szoecs, \email{eduardszoecs@@gmail.com}
#' @export
#' @examples
#' \donttest{
#' # might fail if API is not available
#' inchi <-  "InChI=1S/C17H19NO3/c1-18-7-6-17-10-3-5-13(20)16(17)21-15-12(19)4-
#' 2-9(14(15)17)8-11(10)18/h2-5,10-11,13,16,19-20H,6-8H2,1H3/t10-,11+,13-,16-,17-/m0/s1"
#' # convert InChI to CSID
#' cs_inchi_smiles(inchi)
#' }
cs_inchi_smiles <- function(inchi, verbose = TRUE, ...){
  if (length(inchi) > 1) {
    stop('Cannot handle multiple input strings.')
  }
  baseurl <- 'http://www.chemspider.com/InChI.asmx/InChIToSMILES'
  if (verbose)
    message('Querrying ', baseurl)
  Sys.sleep( rgamma(1, shape = 5, scale = 1/10))
  res <- try(POST(baseurl, body = list(inchi = inchi), encode = 'form'),
             silent = TRUE)
  if (inherits(res, "try-error")) {
    warning('Problem with service... Returning NA.')
    out <- NA
  } else {
    out <- try(read_xml(content(res, 'raw')), silent = TRUE)
    if (inherits(out, "try-error")) {
      warning('inchi not found... Returning NA.')
      out <- NA
    } else {
      out <- xml_text(out)
    }
  }
  return(out)
}



#' Convert a SMILES to InChI
#' @import xml2 httr
#' @importFrom stats rgamma
#'
#' @param smiles character, A SMILES string
#' @param verbose logical; should a verbose output be printed on the console?
#' @param ... currently not used.
#'
#' @return A SMILES string
#'
#' @seealso This is a low level function. Please see \code{\link{cs_convert}} for the top level function.
#'
#' @author Eduard Szoecs, \email{eduardszoecs@@gmail.com}
#' @export
#' @examples
#' \donttest{
#' # might fail if API is not available
#' smiles <- "CN1CC[C@]23[C@H]4C=C[C@@H]([C@@H]3Oc3c(ccc(C[C@@H]14)c23)O)O"
#' # convert smiles to inchi
#' cs_smiles_inchi(smiles)
#' }
cs_smiles_inchi <- function(smiles, verbose = TRUE, ...){
  if (length(smiles) > 1) {
    stop('Cannot handle multiple input strings.')
  }
  baseurl <- 'http://www.chemspider.com/InChI.asmx/SMILESToInChI'
  if (verbose)
    message('Querrying ', baseurl)
  Sys.sleep( rgamma(1, shape = 5, scale = 1/10))
  res <- try(POST(baseurl, body = list(smiles = smiles), encode = 'form'),
             silent = TRUE)
  if (inherits(res, "try-error")) {
    warning('Problem with service... Returning NA.')
    out <- NA
  } else {
    out <- try(read_xml(content(res, 'raw')), silent = TRUE)
    if (inherits(out, "try-error")) {
      warning('inchi not found... Returning NA.')
      out <- NA
    } else {
      out <- xml_text(out)
    }
  }
  return(out)
}
