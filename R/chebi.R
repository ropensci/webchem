#' Retrieve Lite Entity (identifiers) from ChEBI
#'
#' Returns a data.frame with a ChEBI entity ID (chebiid),
#' a ChEBI entity name (chebiasciiname), a search score (searchscore) and
#' stars (stars) using the SOAP protocol:
#' \url{https://www.ebi.ac.uk/chebi/webServices.do}
#' @import httr xml2
#' @importFrom stats rgamma
#' @importFrom stats setNames
#'
#' @param query character; search term.
#' @param from character; type of input, can be one of 'ALL', 'CHEBI ID',
#' 'CHEBI NAME', 'DEFINITION', 'ALL NAMES', 'IUPAC NAME', 'CITATIONS',
#' 'REGISTRY NUMBERS', 'MANUAL XREFS', 'AUTOMATIC XREFS', 'FORMULA', 'MASS',
#' 'MONOISOTOPIC MASS', 'CHARGE', 'INCHI/INCHI KEY', 'SMILES', 'SPECIES'.
#' @param match character; How should multiple hits be handled?,
#' \code{"all"} all matches are returned,
#' \code{"best"} the best matching (by the ChEBI searchscore) is returned,
#' \code{"ask"} enters an interactive mode and the user is asked for input,
#' \code{"na"} returns NA if multiple hits are found.
#' @param max_res integer; maximum number of results to be retrieved from the
#' web service
#' @param stars character; type of input can be one of 'ALL', 'TWO ONLY',
#' 'THREE ONLY'.
#' @param verbose logical; should a verbose output be printed on the console?
#' @param ... optional arguments
#' @return returns a list of data.frames containing a chebiid, a chebiasciiname,
#' a searchscore and stars if matches were found.
#' If not, data.frame(NA) is returned
#'
#' @references Hastings J, Owen G, Dekker A, Ennis M, Kale N, Muthukrishnan V,
#'   Turner S, Swainston N, Mendes P, Steinbeck C. (2016). ChEBI in 2016:
#'   Improved services and an expanding collection of metabfolites. Nucleic
#'   Acids Res.
#'
#'   Hastings, J., de Matos, P., Dekker, A., Ennis, M., Harsha, B., Kale, N.,
#'   Muthukrishnan, V., Owen, G., Turner, S., Williams, M., and Steinbeck, C.
#'   (2013) The ChEBI reference database and ontology for biologically relevant
#'   chemistry: enhancements for 2013. Nucleic Acids Res.
#'
#'   de Matos, P., Alcantara, R., Dekker, A., Ennis, M., Hastings, J., Haug, K.,
#'   Spiteri, I., Turner, S., and Steinbeck, C. (2010) Chemical entities of
#'   biological interest: an update. Nucleic Acids Res. Degtyarenko, K.,
#'   Hastings, J., de Matos, P., and Ennis, M. (2009). ChEBI: an open
#'   bioinformatics and cheminformatics resource. Current protocols in
#'   bioinformatics / editoral board, Andreas D. Baxevanis et al., Chapter 14.
#'
#'   Degtyarenko, K., de Matos, P., Ennis, M., Hastings, J., Zbinden, M.,
#'   McNaught, A., Alcántara, R., Darsow, M., Guedj, M. and Ashburner, M. (2008)
#'   ChEBI: a database and ontology for chemical entities of biological
#'   interest. Nucleic Acids Res. 36, D344–D350.
#' @references Eduard Szöcs, Tamás Stirling, Eric R. Scott, Andreas Scharmüller,
#' Ralf B. Schäfer (2020). webchem: An R Package to Retrieve Chemical
#' Information from the Web. Journal of Statistical Software, 93(13).
#' <doi:10.18637/jss.v093.i13>.
#' @author Andreas Scharmüller, \email{andschar@@protonmail.com}
#' @export
#' @examples
#' \donttest{
#' # might fail if API is not available
#' get_chebiid('Glyphosate')
#' get_chebiid('BPGDAMSIGCZZLK-UHFFFAOYSA-N')
#'
#' # multiple inputs
#' comp <- c('Iron', 'Aspirin', 'BPGDAMSIGCZZLK-UHFFFAOYSA-N')
#' get_chebiid(comp)
#'
#' }
get_chebiid <- function(query,
                        from = 'ALL',
                        match = c("all", "best", "ask", "na"),
                        max_res = 200,
                        stars = 'ALL',
                        verbose = TRUE,
                        ...) {
  match <- match.arg(match)
  foo <- function(query, match, from, max_res, stars, verbose, ...) {
    if (is.na(query)) return(data.frame(chebiid = NA_character_,
                                        query = NA_character_,
                                        stringsAsFactors = FALSE))
    from_all <- c('ALL', 'CHEBI ID', 'CHEBI NAME', 'DEFINITION', 'ALL NAMES',
                  'IUPAC NAME', 'CITATIONS', 'REGISTRY NUMBERS', 'MANUAL XREFS',
                  'AUTOMATIC XREFS', 'FORMULA', 'MASS', 'MONOISOTOPIC MASS',
                  'CHARGE', 'INCHI/INCHI KEY', 'SMILES', 'SPECIES')
    from <- match.arg(from, from_all)
    stars_all <- c('ALL', 'TWO ONLY', 'THREE ONLY')
    stars <- match.arg(stars, stars_all)
    # query
    url <- 'http://www.ebi.ac.uk:80/webservices/chebi/2.0/webservice'
    headers <- c(Accept = 'text/xml',
                 Accept = 'multipart/*',
                 `Content-Type` = 'text/xml; charset=utf-8',
                 SOAPAction = '')
    body <- paste0('
    <soapenv:Envelope
     xmlns:soapenv="http://schemas.xmlsoap.org/soap/envelope/"
     xmlns:chebi="https://www.ebi.ac.uk/webservices/chebi">
      <soapenv:Header/>
        <soapenv:Body>
          <chebi:getLiteEntity>
            <chebi:search>', query, '</chebi:search>
            <chebi:searchCategory>', from, '</chebi:searchCategory>
            <chebi:maximumResults>', max_res, '</chebi:maximumResults>
            <chebi:stars>', stars, '</chebi:stars>
          </chebi:getLiteEntity>
        </soapenv:Body>
     </soapenv:Envelope>')
    Sys.sleep(rgamma(1, shape = 5, scale = 1/10))
    if (verbose)
      message(query, ': ', url)
    res <- POST(url,
                add_headers(headers),
                body = body)
    if (res$status_code == 200) {
      cont <- try(content(res, type = 'text/xml', encoding = 'utf-8'),
                  silent = TRUE)
      out <- l2df(as_list(xml_children(xml_find_first(cont, '//d1:return'))))
      out <- setNames(out, tolower(names(out)))
      if (nrow(out) == 0) {
        message('No result found. \n')
        return(data.frame(chebiid = NA_character_,
                          query = query,
                          stringsAsFactors = FALSE))
      }
      if (nrow(out) > 0) out$query <- query
      if (nrow(out) == 1) return(out)
      if (match == 'all') {
        return(out)
      }
      if (match == 'best') {
        if (verbose)
          message('Returning best match. \n')
        out <- out[with(out, order(searchscore, decreasing = TRUE)), ]
        return(out[which.max(out$searchscore), ])
      }
      if (match == "ask") {
        matched <- chooser(out$chebiid, 'all')
        return(out[out$chebiid == matched, ])
      }
      if (match == 'na') {
        return(data.frame(chebiid = NA_character_,
                          query = query,
                          stringsAsFactors = FALSE))
      }
    } else {
      out <- data.frame(chebiid = NA_character_,
                        query = query,
                        stringsAsFactors = FALSE)
      message('Returning NA (', http_status(res)$message, '). \n')

      return(out)
    }
  }
  out <- lapply(query,
                foo,
                match = match,
                from = from,
                max_res = max_res,
                stars = stars,
                verbose = verbose)
  out <- setNames(out, query)
  out <- as_tibble(bind_rows(out))
  return(out)
}



#' Retrieve Complete Entity from ChEBI
#'
#' Returns a list of Complete ChEBI entities.
#' ChEBI data are parsed as data.frames ("properties", "chebiid_snd",
#' "synonyms", "iupacnames", "formulae", "regnumbers", "citations", "dblinks",
#' "parents", "children", "comments", "origins") or
#' as a list ("chem_structure") in the list.
#' The SOAP protocol is used \url{https://www.ebi.ac.uk/chebi/webServices.do}.
#'
#' @import httr xml2
#' @importFrom stats rgamma
#' @importFrom stats setNames
#'
#' @param chebiid character; search term (i.e. chebiid).
#' @param verbose logical; should a verbose output be printed on the console?
#' @param ... optional arguments
#' @return returns a list of data.frames or lists containing a complete ChEBI
#' entity
#'
#' @references Hastings J, Owen G, Dekker A, Ennis M, Kale N, Muthukrishnan V,
#'   Turner S, Swainston N, Mendes P, Steinbeck C. (2016). ChEBI in 2016:
#'   Improved services and an expanding collection of metabolites. Nucleic Acids
#'   Res.
#'
#'   Hastings, J., de Matos, P., Dekker, A., Ennis, M., Harsha, B., Kale, N.,
#'   Muthukrishnan, V., Owen, G., Turner, S., Williams, M., and Steinbeck, C.
#'   (2013) The ChEBI reference database and ontology for biologically relevant
#'   chemistry: enhancements for 2013. Nucleic Acids Res.
#'
#'   de Matos, P., Alcantara, R., Dekker, A., Ennis, M., Hastings, J., Haug, K.,
#'   Spiteri, I., Turner, S., and Steinbeck, C. (2010) Chemical entities of
#'   biological interest: an update. Nucleic Acids Res. Degtyarenko, K.,
#'   Hastings, J., de Matos, P., and Ennis, M. (2009). ChEBI: an open
#'   bioinformatics and cheminformatics resource. Current protocols in
#'   bioinformatics / editoral board, Andreas D. Baxevanis et al., Chapter 14.
#'
#'   Degtyarenko, K., de Matos, P., Ennis, M., Hastings, J., Zbinden, M.,
#'   McNaught, A., Alcántara, R., Darsow, M., Guedj, M. and Ashburner, M. (2008)
#'   ChEBI: a database and ontology for chemical entities of biological
#'   interest. Nucleic Acids Res. 36, D344–D350.
#' @references Eduard Szöcs, Tamás Stirling, Eric R. Scott, Andreas Scharmüller,
#' Ralf B. Schäfer (2020). webchem: An R Package to Retrieve Chemical
#' Information from the Web. Journal of Statistical Software, 93(13).
#' <doi:10.18637/jss.v093.i13>.
#' @author Andreas Scharmüller, \email{andschar@@protonmail.com}
#' @export
#' @examples
#' \donttest{
#' # might fail if API is not available
#' chebi_comp_entity('CHEBI:27744')
#'
#' # multiple inputs
#' comp <- c('CHEBI:27744', 'CHEBI:27744')
#' chebi_comp_entity(comp)
#'
#' }
chebi_comp_entity <- function(chebiid, verbose = TRUE, ...) {

  foo <- function(chebiid, verbose, ...) {
    # chebiid = c('CHEBI:27744', 'CHEBI:17790'); verbose = TRUE # debuging
    if (is.na(chebiid)) return(NA)
    url <- 'http://www.ebi.ac.uk:80/webservices/chebi/2.0/webservice'
    headers <- c(Accept = 'text/xml',
                 Accept = 'multipart/*',
                 `Content-Type` = 'text/xml; charset=utf-8',
                 SOAPAction = '')
    body <- paste0('
    <soapenv:Envelope
     xmlns:soapenv="http://schemas.xmlsoap.org/soap/envelope/"
     xmlns:chebi="https://www.ebi.ac.uk/webservices/chebi">
      <soapenv:Header/>
        <soapenv:Body>
          <chebi:getCompleteEntity>
            <chebi:chebiId>', chebiid, '</chebi:chebiId>
          </chebi:getCompleteEntity>
        </soapenv:Body>
     </soapenv:Envelope>')
    if (verbose)
      message(chebiid, ': ', url)
    Sys.sleep(rgamma(1, shape = 5, scale = 1/10))
    res <- POST(url,
                add_headers(headers),
                body = body)
    if (res$status_code != 200) {
      warning(http_status(res)$message)
      return(NA)
    } else {
      cont <- content(res, type = 'text/xml', encoding = 'utf-8')
      # restricted to one entry
      properties <- data.frame(
        chebiid = trimws(xml_text(xml_find_first(cont, '//d1:chebiId'))),
        chebiasciiname = trimws(xml_text(
          xml_find_first(cont, '//d1:chebiAsciiName'))),
        definition = trimws(xml_text(xml_find_first(cont, '//d1:definition'))),
        status = trimws(xml_text(xml_find_first(cont, '//d1:status'))),
        smiles = trimws(xml_text(xml_find_first(cont, '//d1:smiles'))),
        inchi = trimws(xml_text(xml_find_first(cont, '//d1:inchi'))),
        inchikey = trimws(xml_text(xml_find_first(cont, '//d1:inchiKey'))),
        charge = trimws(xml_text(xml_find_first(cont, '//d1:charge'))),
        mass = trimws(xml_text(xml_find_first(cont, '//d1:mass'))),
        monoisotopicmass = trimws(xml_text(
          xml_find_first(cont, '//d1:monoisotopicMass'))),
        entitystar = trimws(xml_text(xml_find_first(cont, '//d1:entityStar'))),
        stringsAsFactors = FALSE
      )
      # multiple entries possible
      chebiid_snd <- data.frame(
        chebiids = trimws(xml_text(
          xml_find_all(cont, '//d1:SecondaryChEBIIds'))),
        stringsAsFactors = FALSE
      )
      synonyms <- l2df(as_list(xml_find_all(cont, '//d1:Synonyms')))
      iupacnames <- l2df(as_list(xml_find_all(cont, '//d1:IupacNames')))
      formulae <- l2df(as_list(xml_find_all(cont, '//d1:Formulae')))
      regnumbers <- l2df(as_list(xml_find_all(cont, '//d1:RegistryNumbers')))
      citations <- l2df(as_list(xml_find_all(cont, '//d1:Citations')))
      chem_structure <- as_list(xml_find_all(cont, '//d1:ChemicalStructures'))
      dblinks <- l2df(as_list(xml_find_all(cont, '//d1:DatabaseLinks')))
      parents <- l2df(as_list(xml_find_all(cont, '//d1:OntologyParents')))
      children <- l2df(as_list(xml_find_all(cont, '//d1:OntologyChildren')))
      comments <- l2df(as_list(xml_find_all(cont, '//d1:GeneralComments')))
      origins <- l2df(as_list(xml_find_all(cont, '//d1:CompoundOrigins')))

      # output
      out <- list(
        properties = properties,
        chebiid_snd = chebiid_snd,
        chem_structure = chem_structure,
        synonyms = synonyms,
        iupacnames = iupacnames,
        formulae = formulae,
        regnumbers = regnumbers,
        citations = citations,
        dblinks = dblinks,
        parents = parents,
        children = children,
        comments = comments,
        origins = origins
      )

      return(out)
    }
  }
  out <- lapply(chebiid, foo, verbose = verbose)
  out <- setNames(out, chebiid)
  class(out) <- c("chebi_comp_entity", "list")
  return(out)
}



#' Helper function to parse some ChEBI data
#'
#' @param x list; a list to bind into a data.frame
#' @return a data.frame
#' @seealso \code{\link{chebi_comp_entity}}
#' @author Andreas Scharmüller, \email{andschar@@protonmail.com}
#' @noRd
#'
l2df <- function(x) {
  out <- data.frame(rbind_named_fill(lapply(x, unlist)),
                    row.names = NULL,
                    stringsAsFactors = FALSE)

  return(out)
}



#' Helper function replacing do.call(rbind, list())
#' to address the issue of different column lengths in a list of data.frames
#' taken from:
#' https://stackoverflow.com/questions/17308551/do-callrbind-list-for-uneven-number-of-column
#' @param x list; a list to bind into a data.frame
#' @seealso \code{\link{l2df}}
#' @author Andreas Scharmüller, \email{andschar@@protonmail.com}
#' @noRd
#'
rbind_named_fill <- function(x) {
  nam <- lapply(x, names)
  unam <- unique(unlist(nam))
  len <- lapply(x, length)
  out <- vector("list", length(len))
  for (i in seq_along(len)) {
    out[[i]] <- unname(x[[i]])[match(unam, nam[[i]])]
  }
  setNames(as.data.frame(do.call(rbind, out), stringsAsFactors = FALSE), unam)
}
