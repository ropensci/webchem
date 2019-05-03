#' Retrieve Lite Entity from ChEBI
#'
#' Returns a data.frame with a ChEBI entity ID (chebiid), a ChEBI entity name (chebiasciiname), a search scroe (searchscore) and stars (stars) using the SOAP protocol \url{https://www.ebi.ac.uk/chebi/webServices.do}
#' @import httr xml2
#' @importFrom stats rgamma
#' @importFrom stats setNames
#'
#' @param query character; search term.
#' @param category charatcer; type of input, can be one of 'ALL', 'CHEBI ID', 'CHEBI NAME', 'DEFINITION', 'ALL NAMES', 'IUPAC NAME', 'CITATIONS', 'REGISTRY NUMBERS', 'MANUAL XREFS', 'AUTOMATIC XREFS', 'FORMULA', 'MASS', 'MONOISOTOPIC MASS', 'CHARGE', 'INCHI/INCHI KEY', 'SMILES', 'SPECIES'.
#' @param max_res integer; maximum number of results to be retrieved from the web service
#' @param stars character; type of input can be one of 'ALL', 'TWO ONLY', 'THREE ONLY'.
#' @param verbose logical; should a verbose output be printed on the console?
#' @param ... optional arguments
#' @return returns a list of data.frames containing a chebiid, a chebiasciiname, a searchscore and stars if matches were found. If not, data.frame(NA) is returned
#'
#' @references Hastings J, Owen G, Dekker A, Ennis M, Kale N, Muthukrishnan V, Turner S, Swainston N, Mendes P, Steinbeck C. (2016). ChEBI in 2016: Improved services and an expanding collection of metabolites. Nucleic Acids Res.
#' Hastings, J., de Matos, P., Dekker, A., Ennis, M., Harsha, B., Kale, N., Muthukrishnan, V., Owen, G., Turner, S., Williams, M., and Steinbeck, C. (2013) The ChEBI reference database and ontology for biologically relevant chemistry: enhancements for 2013. Nucleic Acids Res.
#' de Matos, P., Alcantara, R., Dekker, A., Ennis, M., Hastings, J., Haug, K., Spiteri, I., Turner, S., and Steinbeck, C. (2010) Chemical entities of biological interest: an update. Nucleic Acids Res.
#' Degtyarenko, K., Hastings, J., de Matos, P., and Ennis, M. (2009). ChEBI: an open bioinformatics and cheminformatics resource. Current protocols in bioinformatics / editoral board, Andreas D. Baxevanis et al., Chapter 14.
#' Degtyarenko, K., de Matos, P., Ennis, M., Hastings, J., Zbinden, M., McNaught, A., Alcántara, R., Darsow, M., Guedj, M. and Ashburner, M. (2008) ChEBI: a database and ontology for chemical entities of biological interest. Nucleic Acids Res. 36, D344–D350.
#'
#' @author Andreas Scharmueller, \email{andschar@@protonmail.com}
#' @export
#' @examples
#' \donttest{
#' # might fail if API is not available
#' get_lite_entity('Glyphosate')
#' get_lite_entity('BPGDAMSIGCZZLK-UHFFFAOYSA-N')
#'
#' # multiple inputs
#' comp <- c('Iron', 'Aspirin', 'BPGDAMSIGCZZLK-UHFFFAOYSA-N')
#' get_lite_entity(comp)
#'
#' }
get_lite_entity <- function(query, category = 'ALL', max_res = 200, stars = 'ALL', verbose = TRUE, ...) {

  foo <- function(query, category, max_res, stars, verbose, ...) {
    # query = 'Isoproturon'; category = 'ALL'; max_res = 200; stars = 'ALL'; verbose = T # debuging
    # arguments
    category_all <- c('ALL', 'CHEBI ID', 'CHEBI NAME', 'DEFINITION', 'ALL NAMES', 'IUPAC NAME', 'CITATIONS', 'REGISTRY NUMBERS', 'MANUAL XREFS', 'AUTOMATIC XREFS', 'FORMULA', 'MASS', 'MONOISOTOPIC MASS', 'CHARGE', 'INCHI/INCHI KEY', 'SMILES', 'SPECIES')
    category <- match.arg(category, category_all)
    stars_all <- c('ALL', 'TWO ONLY', 'THREE ONLY')
    stars <- match.arg(stars, stars_all)
    # query
    url <- 'http://www.ebi.ac.uk:80/webservices/chebi/2.0/webservice'
    headers <- c(Accept = 'text/xml', Accept = 'multipart/*', 'Content-Type' = 'text/xml; charset=utf-8', SOAPAction = '')
    body <- paste0('<soapenv:Envelope
                   xmlns:soapenv="http://schemas.xmlsoap.org/soap/envelope/"
                   xmlns:chebi="https://www.ebi.ac.uk/webservices/chebi">
                    <soapenv:Header/>
                      <soapenv:Body>
                        <chebi:getLiteEntity>
                          <chebi:search>', query, '</chebi:search>
                          <chebi:searchCategory>', category, '</chebi:searchCategory>
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
      cont <- try(content(res, type = 'text/xml', encoding = 'utf-8'), silent = TRUE)
      out <- l2df(as_list(xml_children(xml_find_first(cont, '//d1:return'))))
      out <- setNames(out, tolower(names(out)))

      return(out)
    } else {
      out <- data.frame(chebiid = NA)
      warning(http_status(res)$message)

      return(out)
    }
  }
  out <- lapply(query, foo, category = category, max_res = max_res, stars = stars, verbose = verbose)
  out <- setNames(out, query)

  return(out)
}



#' Retrieve Complete Entity from ChEBI
#'
#' Returns a list of Complete ChEBI entities. ChEBI data are parsed as data.frames ("properties", "chebiid_snd", "synonyms", "iupacnames", "formulae", "regnumbers", "citations", "dblinks", "parents", "children", "comments", "origins") or as a list ("chem_structure") in the list. The SOAP protocol is used \url{https://www.ebi.ac.uk/chebi/webServices.do}.
#'
#' @import httr xml2
#' @importFrom stats rgamma
#' @importFrom stats setNames
#'
#' @param chebiid character; search term (i.e. chebiid).
#' @param verbose logical; should a verbose output be printed on the console?
#' @param ... optional arguments
#' @return returns a list of data.frames or lists containing a complete ChEBI entity
#'
#' @references Hastings J, Owen G, Dekker A, Ennis M, Kale N, Muthukrishnan V, Turner S, Swainston N, Mendes P, Steinbeck C. (2016). ChEBI in 2016: Improved services and an expanding collection of metabolites. Nucleic Acids Res.
#' Hastings, J., de Matos, P., Dekker, A., Ennis, M., Harsha, B., Kale, N., Muthukrishnan, V., Owen, G., Turner, S., Williams, M., and Steinbeck, C. (2013) The ChEBI reference database and ontology for biologically relevant chemistry: enhancements for 2013. Nucleic Acids Res.
#' de Matos, P., Alcantara, R., Dekker, A., Ennis, M., Hastings, J., Haug, K., Spiteri, I., Turner, S., and Steinbeck, C. (2010) Chemical entities of biological interest: an update. Nucleic Acids Res.
#' Degtyarenko, K., Hastings, J., de Matos, P., and Ennis, M. (2009). ChEBI: an open bioinformatics and cheminformatics resource. Current protocols in bioinformatics / editoral board, Andreas D. Baxevanis et al., Chapter 14.
#' Degtyarenko, K., de Matos, P., Ennis, M., Hastings, J., Zbinden, M., McNaught, A., Alcántara, R., Darsow, M., Guedj, M. and Ashburner, M. (2008) ChEBI: a database and ontology for chemical entities of biological interest. Nucleic Acids Res. 36, D344–D350.
#'
#' @author Andreas Scharmueller, \email{andschar@@protonmail.com}
#' @export
#' @examples
#' \donttest{
#' # might fail if API is not available
#' get_comp_entity('CHEBI:27744')
#'
#' # multiple inputs
#' comp <- c('CHEBI:27744', 'CHEBI:27744')
#' get_comp_entity(comp)
#'
#' }
get_comp_entity <- function(chebiid, verbose = TRUE, ...) {

  foo <- function(chebiid, verbose, ...) {
    # chebiid = c('CHEBI:27744', 'CHEBI:17790'); verbose = TRUE # debuging
    url <- 'http://www.ebi.ac.uk:80/webservices/chebi/2.0/webservice'
    headers <- c(Accept = 'text/xml', Accept = 'multipart/*', 'Content-Type' = 'text/xml; charset=utf-8', SOAPAction = '')
    body <- paste0('<soapenv:Envelope
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
      out <- data.frame(chebiid = NA)
      warning(http_status(res)$message)

      return(out)
    } else {
      cont <- content(res, type = 'text/xml', encoding = 'utf-8')
      # restricted to one entry
      properties <- data.frame(
        chebiid = trimws(xml_text(xml_find_first(cont, '//d1:chebiId'))),  # d1: due to namespace
        chebiasciiname = trimws(xml_text(xml_find_first(cont, '//d1:chebiAsciiName'))),
        definition = trimws(xml_text(xml_find_first(cont, '//d1:definition'))),
        status = trimws(xml_text(xml_find_first(cont, '//d1:status'))),
        smiles = trimws(xml_text(xml_find_first(cont, '//d1:smiles'))),
        inchi = trimws(xml_text(xml_find_first(cont, '//d1:inchi'))),
        inchikey = trimws(xml_text(xml_find_first(cont, '//d1:inchiKey'))),
        charge = trimws(xml_text(xml_find_first(cont, '//d1:charge'))),
        mass = trimws(xml_text(xml_find_first(cont, '//d1:mass'))),
        monoisotopicmass = trimws(xml_text(xml_find_first(cont, '//d1:monoisotopicMass'))),
        entitystar = trimws(xml_text(xml_find_first(cont, '//d1:entityStar'))),
        stringsAsFactors = FALSE
      )
      # multiple entries possible
      chebiid_snd <- data.frame(
        chebiids = trimws(xml_text(xml_find_all(cont, '//d1:SecondaryChEBIIds'))),
        stringsAsFactors = FALSE
      )
      synonyms <- l2df(as_list(xml_find_all(cont, '//d1:Synonyms')))
      iupacnames <- l2df(as_list(xml_find_all(cont, '//d1:IupacNames')))
      formulae <- l2df(as_list(xml_find_all(cont, '//d1:Formulae')))
      regnumbers <- l2df(as_list(xml_find_all(cont, '//d1:RegistryNumbers')))
      citations <- l2df(as_list(xml_find_all(cont, '//d1:Citations')))
      struct <- as_list(xml_find_all(cont, '//d1:ChemicalStructures'))
      dblinks <- l2df(as_list(xml_find_all(cont, '//d1:DatabaseLinks')))
      parents <- l2df(as_list(xml_find_all(cont, '//d1:OntologyParents')))
      children <- l2df(as_list(xml_find_all(cont, '//d1:OntologyChildren')))
      comments <- l2df(as_list(xml_find_all(cont, '//d1:GeneralComments')))
      origins <- l2df(as_list(xml_find_all(cont, '//d1:CompoundOrigins')))

      # output
      out <- list(
        properties = properties,
        chebiid_snd = chebiid_snd,
        chem_structure = struct,
        synonyms = synonyms,
        iupacnames = iupacnames,
        formulae = formulae,
        regnumbers = regnumbers,
        citations = citations,
        struct = struct,
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

  return(out)
}



#' Helper function to parse some ChEBI data
#'
#' @return a data.frame
#' @seealso \code{\link{get_comp_entity}}
#' @author Andreas Scharmueller, \email{andschar@@protonmail.com}
#'
#'
l2df <- function(x) {
  out <- data.frame(rbind.named.fill(lapply(x, unlist)),
                    row.names = NULL,
                    stringsAsFactors = FALSE)

  return(out)
}



#' Helper function replacing do.call(rbind, list())
#' to address the issue of different column lengths in a list of data.frames
#' taken from: https://stackoverflow.com/questions/17308551/do-callrbind-list-for-uneven-number-of-column
rbind.named.fill <- function(x) {
  nam <- lapply(x, names)
  unam <- unique(unlist(nam))
  len <- lapply(x, length)
  out <- vector("list", length(len))
  for (i in seq_along(len)) {
    out[[i]] <- unname(x[[i]])[match(unam, nam[[i]])]
  }
  setNames(as.data.frame(do.call(rbind, out), stringsAsFactors = FALSE), unam)
}