
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
#' pc_sect("1ZHY_A", "Sequence", "protein", parser = "sequence")
#' }
#' @export
pc_sect <- function(
  id,
  section,
  domain = c("compound", "substance", "assay", "gene", "protein", "patent"),
  parser = c("table", "sequence"),
  verbose = getOption("verbose")
) {
  domain <- match.arg(domain)
  section <- tolower(section)
  if (section %in% c(
    "standard non-polar",
    "semi-standard non-polar",
    "standard polar")) {
    stop("use nist_ri() to obtain more information on this.")
  }
  res <- lapply(id, function(x) pc_page(x, section, domain, verbose))
  PARSEFUN <- paste0("pc_parse_", match.arg(parser))
  out <- lapply(res, function(x) {
    do.call(PARSEFUN, args = list(
      pg = x,
      section = section
    ))
  })
  out <- dplyr::bind_rows(out)
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
  section <- tolower(section)
  if (is.na(id)) {
    if (verbose) webchem_message("na")
    return(NA)
  }
  qurl <- paste0("https://pubchem.ncbi.nlm.nih.gov/rest/pug_view/data/",
                 domain, "/", id, "/JSON?heading=", gsub(" +", "+", section))
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


#' Find a section in a PubChem content page
#' 
#' This function searches for a specific section in a PubChem content page and
#' returns the content of that section.
#' @param pg list; a PubChem content page.
#' @param section character; the name of the section to be found. Not case 
#' sensitive
#' @return A list containing the content of the specified section, or NA if
#' the section is not found.
#' @noRd
pc_find_section <- function(pg, section) {
  if (is.na(pg)) return(NULL)
  search_sections <- function(section_list, target) {
    if (!is.list(section_list)) return(NULL)
    for (i in seq_along(section_list)) {
      item <- section_list[[i]]
      if (is.list(item) && !is.null(item$TOCHeading)) {
        if (tolower(item$TOCHeading) == tolower(target)) {
          return(item)
        }
        if (!is.null(item$Section)) {
          result <- search_sections(item$Section, target)
          if (!is.null(result)) return(result)
        }
      }
    }
    return(NULL)
  }
  result <- search_sections(pg$Record$Section, section)
  return(result)
}

#' Parse data from a PubChem content page section into a tibble
#' 
#' Extracts tabular data from a PubChem content page section by retrieving
#' values, reference numbers, and source information from the "Information"
#' field. Returns a tibble with NA values if the section is not found or
#' contains no information.
#' @param pg list; a PubChem content page.
#' @param section character; the name of the section from which to extract the
#' table. Not case sensitive.
#' @return A tibble containing the ID, name, result value, source name, and 
#' source ID for each piece of information found in the specified section, or a 
#' tibble with NA values if the section is not found or contains no information.
#' @noRd
pc_parse_table <- function(pg, section) {
  name <- pg$Record$RecordTitle
  id <- pg$Record$RecordNumber |> as.integer()
  domain <- pg$Record$RecordType
  empty_row <- data.frame(
    ID = id,
    Name = name,
    Result = NA_character_,
    SourceName = NA_character_,
    SourceID = NA_character_
  )
  names(empty_row)[1] <- domain
  sect <- pc_find_section(pg, section)
  if (is.null(sect)) return(empty_row)
  if (!is.null(sect$Information)) {
    info <- lapply(sect$Information, function(x) {
      refnum <- if (!is.null(x$ReferenceNumber)) x$ReferenceNumber else NA
      if (!is.null(x$Value$StringWithMarkup)) {
        values <- sapply(x$Value$StringWithMarkup, function(y) y$String)
      } else {
        values <- NA_character_
      }
      data.frame(value  = values, refnum = refnum)
    })
    info <- do.call(rbind, info)
    if (!is.null(pg$Record$Reference)) {
      refs <- lapply(pg$Record$Reference, function(x) {
        data.frame(
          ReferenceNumber = x$ReferenceNumber,
          SourceName = x$SourceName,
          SourceID = x$SourceID
        )
      })
      refs <- do.call(rbind, refs)
      info <- dplyr::left_join(
        info,
        refs,
        by = c("refnum" = "ReferenceNumber")
      )
    } else {
      info$SourceName <- NA_character_
      info$SourceID <- NA_character_
    }
    info <- info[, -which(names(info) == "refnum")]
    out <- tibble::tibble(
      ID = id,
      Name = name,
      Result = info$value,
      SourceName = info$SourceName,
      SourceID = info$SourceID
    )
    names(out)[1] <- domain
    return(out)
  } else {
    return(empty_row)
  }
}

#' Parse sequence data from a PubChem content page
#' 
#' Extracts sequence data from a PubChem content page section.
#' @param pg list; a PubChem content page.
#' @param section character; the name of the section from which to extract the
#' table. Not case sensitive.
#' @return A tibble containing the ID, name, sequence name, sequence, 
#' source name, and source ID for each piece of information found in the 
#' specified section, or a  tibble with NA values if the section is not found or
#' contains no information.
#' @noRd
pc_parse_sequence <- function(pg, section) {
  name <- pg$Record$RecordTitle
  id <- pg$Record$RecordAccession
  domain <- pg$Record$RecordType
  empty_row <- data.frame(
    ID = id,
    Name = name,
    Header = NA_character_,
    Sequence = NA_character_,
    SourceName = NA_character_,
    SourceID = NA_character_
  )
  names(empty_row)[1] <- domain
  sect <- pc_find_section(pg, section)
  if (is.null(sect)) return(empty_row)
  if (!is.null(sect$Information)) {
    info <- lapply(sect$Information, function(x) {
      refnum <- if (!is.null(x$ReferenceNumber)) x$ReferenceNumber else NA
      if (!is.null(x$Value$StringWithMarkup)) {
        values <- sapply(x$Value$StringWithMarkup, function(y) y$String)
        if (length(values) == 2 & grepl(">", values[1])) {
          header <- values[1]
          sequence <- values[2]
        } else {
          header <- NA_character_
          sequence <- NA_character_
        }
      } else {
        header <- NA_character_
        sequence <- NA_character_
      }
      data.frame(header = header, sequence = sequence, refnum = refnum)
    })
    info <- do.call(rbind, info)
    if (!is.null(pg$Record$Reference)) {
      refs <- lapply(pg$Record$Reference, function(x) {
        data.frame(
          ReferenceNumber = x$ReferenceNumber,
          SourceName = x$SourceName,
          SourceID = x$SourceID
        )
      })
      refs <- do.call(rbind, refs)
      info <- dplyr::left_join(
        info,
        refs,
        by = c("refnum" = "ReferenceNumber")
      )
    } else {
      info$SourceName <- NA_character_
      info$SourceID <- NA_character_
    }
    info <- info[, -which(names(info) == "refnum")]
    out <- tibble::tibble(
      ID = id,
      Name = name,
      Header = info$header,
      Sequence = info$sequence,
      SourceName = info$SourceName,
      SourceID = info$SourceID
    )
    names(out)[1] <- domain
    return(out)
  } else {
    return(empty_row)
  }
}
