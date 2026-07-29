
#' Retrieve data from PubChem content pages
#'
#' When you search for an entity at \url{https://pubchem.ncbi.nlm.nih.gov/},
#' e.g. a compound or a substance, and select the record you are interested in,
#' you will be forwarded to a PubChem content page. When you look at a PubChem
#' content page, you can see that chemical information is organised into
#' sections, subsections, etc. The chemical data live at the lowest levels of
#' these sections. Use this function to retrieve the lowest level information
#' from PubChem content pages.
#' @param id numeric or character; a vector of PubChem identifiers to search
#' for.
#' @param section character; the section of the content page to be imported.
#' @param domain character; the query domain. Can be one of \code{"compound"},
#' \code{"substance"}, \code{"assay"}, \code{"gene"}, \code{"protein"} or
#' \code{"patent"}.
#' @param verbose logical; should a verbose output be printed on the console?
#' @return Returns a tibble of query results. In the returned tibble,
#' \code{SourceName} is the name of the depositor, and \code{SourceID} is the
#' ID of the search term within the depositor's database. You can browse
#' \url{https://pubchem.ncbi.nlm.nih.gov/sources/} for more information about
#' the depositors.
#' @details \code{section} is not case sensitive but it is sensitive to typing
#' errors and it requires the full name of the section as it is printed on the
#' content page. The PubChem Table of Contents Tree can also be found at
#' \url{https://pubchem.ncbi.nlm.nih.gov/classification/#hid=72}.
#' @note Please respect the Terms and Conditions of the National Library of
#' Medicine, \url{https://www.nlm.nih.gov/databases/download.html} the data
#' usage policies of National Center for Biotechnology Information,
#' \url{https://www.ncbi.nlm.nih.gov/home/about/policies/},
#' \url{https://pubchem.ncbi.nlm.nih.gov/docs/programmatic-access}, and the data
#' usage policies of the individual data sources
#' \url{https://pubchem.ncbi.nlm.nih.gov/sources/}.
#' @references Kim, S., Thiessen, P.A., Cheng, T. et al. PUG-View: programmatic
#' access to chemical annotations integrated in PubChem. J Cheminform 11, 56
#' (2019). \doi{10.1186/s13321-019-0375-2}.
#' @seealso \code{\link{get_cid}}, \code{\link{pc_prop}}
#' @examples
#' # might fail if API is not available
#' \dontrun{
#' pc_sect(176, "Dissociation Constants")
#' pc_sect(c(176, 311), "density")
#' pc_sect(2231, "depositor-supplied synonyms", "substance")
#' pc_sect(780286, "modify date", "assay")
#' pc_sect(9023, "Ensembl ID", "gene")
#' pc_sect("1ZHY_A", "Sequence", "protein")
#' }
#' @export
pc_sect <- function(id,
                    section,
                    domain = c("compound", "substance", "assay", "gene",
                                "protein", "patent"),
                    verbose = getOption("verbose")) {
  domain <- match.arg(domain)
  section <- tolower(gsub(" +", "+", section))
  if (section %in% c("standard non-polar",
                     "Semi-standard non-polar",
                     "Standard polar")) {
    stop("use nist_ri() to obtain more information on this.")
  }
  res <- lapply(id, function(x) {
    cont <- pc_page(x, section, domain, verbose)
    tree <- data.tree::as.Node(cont, nameName = "TOCHeading")
    tree$Do(function(node) node$name <- tolower(node$name))
    return(tree)
  })
  names(res) <- id
  attr(res, "id") <- switch(
    domain,
    compound = "CID",
    substance = "SID",
    assay = "AID",
    gene = "GeneID",
    protein = "pdbID",
    patent = "PatentID"
  )
  out <- pc_extract(res, section)
  return(out)
}

#' Import PubChem content pages
#'
#' @importFrom jsonlite fromJSON
#' @importFrom data.tree as.Node Do
#' @param id numeric or character; a vector of identifiers to search for.
#' @param section character; the section of the content page to be imported.
#' @param domain character; the query domain. Can be one of \code{"compound"},
#' \code{"substance"}, \code{"assay"}, \code{"gene"}, \code{"protein"} or
#' \code{"patent"}.
#' @return A named list of content pages where each element is either a
#' data.tree or NA.
#' @details \code{section} can be any section of a PubChem content page, e.g.
#' \code{section = "solubility"} will import the section on solubility, or
#' \code{section = "experimental properties"} will import all experimental
#' properties. The \code{section} argument is not case sensitive but it
#' is sensitive to typing errors and it requires the full name of the section as
#' it is printed on the content page. The PubChem Table of Contents Tree can
#' also be found at
#' \url{https://pubchem.ncbi.nlm.nih.gov/classification/#hid=72}.
#' @references Kim, S., Thiessen, P.A., Cheng, T. et al. PUG-View: programmatic
#' access to chemical annotations integrated in PubChem. J Cheminform 11, 56
#' (2019). \doi{10.1186/s13321-019-0375-2}.
#' @examples
#' # might fail if API is not available
#' \dontrun{
#' pc_page(176, "Dissociation Constants")
#' pc_page(49854366, "external id", domain = "substance")
#' }
#' @noRd
pc_page <- function(
  id,
  section,
  domain = c("compound", "substance", "assay", "gene", "protein", "patent"),
  verbose = getOption("verbose")
) {

  if (!ping_service("pc")) stop(webchem_message("service_down"))

  domain <- match.arg(domain)
  section <- tolower(gsub(" +", "+", section))
  if (is.na(id)) {
    if (verbose) webchem_message("na")
    return(NA)
  }
  qurl <- paste0("https://pubchem.ncbi.nlm.nih.gov/rest/pug_view/data/",
                 domain, "/", id, "/JSON?heading=", section)
  if (verbose) webchem_message("query", id, appendLF = FALSE)
  webchem_sleep(type = 'API')
  res <- try(httr::RETRY("GET",
                         qurl,
                         user_agent(webchem_url()),
                         terminate_on = 404,
                         quiet = TRUE), silent = TRUE)
  if (inherits(res, "try-error")) {
    if (verbose) webchem_message("service_down")
    return(NA)
  }
  if (verbose) message(httr::message_for_status(res))
  if (res$status_code == 200) {
    cont <- httr::content(res, type = "text", encoding = "UTF-8")
    # Intercepting any NA cont before it gets to fromJSON.
    if(is.na(cont)) {
      return(NA)
    }
    cont <- jsonlite::fromJSON(cont, simplifyDataFrame = FALSE)
    return(cont)
  }
  else {
    return(NA)
  }

}

#' Extract data from PubChem content pages
#'
#' This function takes a list of PubChem content pages, and extracts the
#' required information from them.
#' @importFrom data.tree FindNode
#' @importFrom dplyr bind_rows
#' @importFrom tibble as_tibble
#' @param pages list; a list of PubChem content pages.
#' @param section character; the lowest level section of the data to be
#' accessed.
#' @return A tibble of chemical information with references.
#' @details When you look at a PubChem content page, you can see that chemical
#' information is organised into sections, subsections, etc. The chemical data
#' live at the lowest levels of these sections. Use this function to extract the
#' lowest level information from PubChem content pages, e.g. IUPAC Name, Boiling
#' Point, Lower Explosive Limit (LEL).
#' @details The \code{section} argument is not case sensitive, but it is
#' sensitive to typing errors, and requires the full name of the section as it
#' is printed on the content page. The PubChem Table of Contents Tree can also
#' be found at \url{https://pubchem.ncbi.nlm.nih.gov/classification/#hid=72}.
#' @references Kim, S., Thiessen, P.A., Cheng, T. et al. PUG-View: programmatic
#' access to chemical annotations integrated in PubChem. J Cheminform 11, 56
#' (2019). \doi{10.1186/s13321-019-0375-2}.
#' @examples
#' # might fail if API is not available
#' \dontrun{
#' comps <- pc_page(c(176, 311), "Dissociation Constants")
#' pc_extract(comps, "Dissociation Constants")
#' subs <- pc_page(49854366, "external id", domain = "substance")
#' pc_extract(subs, "external id")
#' }
#' @noRd
pc_extract <- function(page, section) {
  section <- tolower(section)
  ids <- names(page)
  foo <- function(i, section) {
    tree <- page[[i]]
    if (length(tree) == 1 && is.na(tree)) return(tibble(ID = ids[i]))
    node <- FindNode(tree, "information")
    if (is.null(node)) return(tibble(ID = ids[i],
                                     Name = tree$record$RecordTitle))
    info <- lapply(node, function(y) {
      lownode <- data.tree::FindNode(data.tree::as.Node(y), "stringwithmarkup")
      if (is.null(lownode)) {
        info <- tibble(Result = paste(y$value, collapse = " "),
                       ReferenceNumber = y$ReferenceNumber)
        return(info)
      }
      else{
        string <- sapply(lownode, function(z) z$String)
        info <- tibble(Result =  string,
                       ReferenceNumber = y$ReferenceNumber)
      }
    })
    info <- dplyr::bind_rows(info)
    info <- tibble(ID = ids[i],
                   Name = tree$record$RecordTitle,
                   info)
    node <- FindNode(tree, "reference")
    if (is.null(node)) return(tibble(info, SourceName = NA, SourceID = NA))
    ref <- lapply(node, function(y) {
      ref <- tibble(ReferenceNumber = y$ReferenceNumber,
                    SourceName = y$SourceName,
                    SourceID = y$SourceID)
      return(ref)
    })
    ref <- dplyr::bind_rows(ref)
    info$SourceName <- sapply(info$ReferenceNumber, function(x) {
      ref$SourceName[ref$ReferenceNumber == x]
    })
    info$SourceID <- sapply(info$ReferenceNumber, function(x) {
      ref$SourceID[ref$ReferenceNumber == x]
    })
    return(info)
  }
  info <- lapply(seq_along(page), function(x) foo(x, section))
  info <- dplyr::bind_rows(info)
  info <- info[, -which(names(info) == "ReferenceNumber")]
  names(info)[1] <- attr(page, "id")
  return(info)
}
