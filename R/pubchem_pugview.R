
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
  parser = c("string", "table", "sequence"),
  verbose = getOption("verbose")
) {
  domain <- match.arg(domain)
  section <- tolower(section)
  if (section %in% c(
    "kovats retention index",
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

#' Decompose a pointer from PubChem content pages
#'
#' Some resources are accessible through PubChem, but the PUG-View page does not
#' contain the data itself, only a pointer to the data. The pointer contains
#' instructions that can be used to build a query to PubChem's SDQ
#' (Structured Data Query) service. This function decomposes a pointer into its
#' components.
#' @param pointer character; a pointer string from a PubChem content page.
#' @return A named list containing the components of the pointer.
#' @examples
#' pc_decompose_pointer("collection=chemidplus&query_type=sid&query=134972565")
#' @noRd
pc_decompose_pointer <- function(pointer) {
  if (length(pointer) == 1 && !grepl("=", pointer, fixed = TRUE)) {
    return(list(collection = pointer))
  }
  pointers <- strsplit(pointer, "&",fixed = TRUE)[[1]]
  stats::setNames(
    as.list(sub("^[^=]+=", "", pointers)),
    sub("=.*$", "", pointers)
  )
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

#' Determine if a data frame needs to be pivoted wider
#' 
#' This function checks if a data frame needs to be pivoted wider based on the 
#' specified form and the structure of the data frame.
#' @param df data frame; the data frame to check.
#' @param form character; the form of the output. Can be one of \code{"auto"}, 
#' \code{"long"} or \code{"wide"}. If \code{"auto"}, the function will determine 
#' if pivoting is necessary based on the structure of the data frame.
#' @return logical; TRUE if the data frame needs to be pivoted wider, FALSE otherwise.
#' @noRd
pc_needs_pivot_wider <- function(df, form) {
  if (form == "long") return(FALSE)
  if (form == "wide") return(TRUE)
  if (ncol(df) != 3) return(FALSE)
  if (any(!c("Result", "Name", "refnum") %in% names(df))) return(FALSE)
  if (any(is.na(df$Name))) return(FALSE)
  if (any(df$Name == "")) return(FALSE)
  return(TRUE)
}

#' Normalise values from PubChem content pages
#'
#' PubChem pages are retrieved as deeply nested lists. However, there is always
#' an "Information" field which contains one or more elements. Each element is
#' itself a list. This function parses the data in an "Information" element
#' into a flat data frame which is easier to work with.
#' @param x list; an "Information" element from a PubChem content page.
#' @return A character string containing the normalised value, or NA if the
#' input is NULL or contains no information.
#' @examples
#' \dontrun{
#' pg <- pc_page(176, "Dissociation Constants")
#' sect <- pc_find_section(pg, "Dissociation Constants")
#' pc_parse_information_element(sect$Information[[1]], 176, "cid")
#' }
#' @noRd
pc_parse_information_element <- function(x, id, domain) {
  domain <- tolower(domain)
  if (is.null(x$Value)) return(NA_character_)
  if ("Number" %in% names(x$Value)) {
    out <- x$Value$Number
  } else if ("DateISO8601" %in% names(x$Value)) {
    out <- x$Value$DateISO8601
  } else if ("StringWithMarkup" %in% names(x$Value)) {
    if (!is.null(x$Name) && x$Name == "Top 5 Peaks") {
      out <- lapply(x$Value$StringWithMarkup, function(A) {
        gsub(" ", ":", A$String)
      }) |> unlist() |> paste(collapse = ", ")
    } else if (!is.null(x$Name) && x$Name == "1D NMR Spectra") {
      if (length(x$Value$StringWithMarkup) > 1) {
        stop("This case is not yet supported. Please open an issue.")
      }
      aux <- x$Value$StringWithMarkup[[1]]
      if (!"Markup" %in% names(aux)) {
        stop("This case is not yet supported. Please open an issue.")
      }
      if (length(aux$Markup) > 1) {
        stop("This case is not yet supported. Please open an issue.")
      }
      out <- aux$Markup[[1]]$URL
    } else {
      out <- lapply(x$Value$StringWithMarkup, function(A) {
        A$String
      }) |> unlist()
      is_sequence <- length(out) == 2 &&
        grepl(">", out[1]) &&
        all(strsplit(out[2], "")[[1]] %in% LETTERS)
      if (is_sequence) {
        out <- data.frame(Header = out[1], Sequence = out[2])
      }
      if (!is.null(x$Value$Unit)) {
        out <- paste(out, x$Value$Unit)
      }
    }
  } else if ("ExternalDataURL" %in% names(x$Value)) {
    out <- x$Value$ExternalDataURL
  } else if ("ExternalTableName" %in% names(x$Value)) {
    pointer <- pc_decompose_pointer(x$Value$ExternalTableName)
    if (!"collection" %in% names(pointer)) {
      stop(sprintf(
        "'ExternalTableName' not implemented for '%s'. Please open an issue.", 
        x$Value$ExternalTableName
      ))
    }
    out <- pc_sdq_query(
      collection = pointer$collection,
      idtype = ifelse(!is.null(pointer$query_type), pointer$query_type, domain),
      query = ifelse(!is.null(pointer$query), pointer$query, id)
    )
  } else {
    stop("Unknown value type: ", names(x$Value))
  }
  if (length(out) == 1 && out == "") out <- NA_character_
  out <- tibble::as_tibble(out)
  if (ncol(out) == 1 && names(out) == "value") {
    out$value <- as.character(out$value)
    names(out) <- "Result"
  }
  if (!is.null(x$Name)) out$Name <- x$Name
  if (!is.null(x$ReferenceNumber)) {
    out$refnum <- x$ReferenceNumber
  } else {
    out$refnum <- NA_integer_
  }
  return(out)
}

#' Parse data from a PubChem content page
#'
#' This function parses the data from a PubChem content page into a flat tibble.
#' @param pg list; a PubChem content page.
#' @param section character; the name of the section to be parsed.
#' @param form character; the form of the output. Can be one of \code{"auto"},
#' \code{"long"} or \code{"wide"}.
#' @return A tibble containing the parsed data from the specified section of the
#' PubChem content page.
#' @noRd
pc_parse_all <- function(pg, section, form) {
  if (is.na(pg)) return(NA)
  name <- pg$Record$RecordTitle
  id <- pg$Record$RecordNumber
  if (!is.null(id)) {
    id <- as.character(id)
  } else {
    id <- pg$Record$RecordAccession
  }
  if (is.null(id)) {
    id <- NA_character_
  }
  domain <- pg$Record$RecordType
  sect <- pc_find_section(pg, section)
  if (is.null(sect)) return(NA)
  if (is.null(sect$Information)) return(NA)
  info <- lapply(sect$Information, function(x) {
    pc_parse_information_element(x, id, domain)
  }) |> dplyr::bind_rows()
  if (pc_needs_pivot_wider(info, form)) {
    info <- info |>
      dplyr::group_by(!!rlang::sym("refnum")) |>
      tidyr::pivot_wider(
        names_from = !!rlang::sym("Name"),
        values_from = !!rlang::sym("Result")
      ) |>
      dplyr::ungroup()
  } else if ("Name" %in% names(info)) {
    info <- info |> dplyr::select(-!!rlang::sym("Name"))
  }
  if (!is.null(pg$Record$Reference)) {
    refs <- lapply(pg$Record$Reference, as.data.frame) |> dplyr::bind_rows()
    info <- dplyr::left_join(
      info,
      refs,
      by = c("refnum" = "ReferenceNumber")
    )
  }
  out <- info |>
    dplyr::select(-rlang::sym("refnum")) |>
    dplyr::mutate(
      Section = section,
      ID = id,
      Name = name
    ) |>
    dplyr::relocate(
      !!rlang::sym("Section"), !!rlang::sym("ID"), !!rlang::sym("Name")
    )
  names(out)[2] <- domain
  return(out)
}

#' Build a SDQ query for PubChem
#'
#' Some resources are accessible through PubChem, but the PUG-View page does not
#' contain the data itself, only a pointer to the data. This function constructs
#' a json query that can be used to retrive the data using PubChem's SDQ
#' (Structured Data Query) service.
#' @param collection character; the collection to query (e.g., "chemidplus").
#' @param idtype character; the type of identifier to query (e.g., "cid", "sid").
#' @param query character; the identifier value to query.
#' @param limit numeric; the maximum number of results to return (default is 10000000).
#' @return A JSON string representing the SDQ query.
#' @noRd
pc_build_sdq_query <- function(
    collection,
    idtype,
    query,
    limit = 10000000
) {
  where_clause <- list()
  where_clause[[idtype]] <- query
  sdq <- list(
    download = "*",
    collection = collection,
    order = list("relevancescore,desc"),
    start = 1,
    limit = limit,
    downloadfilename = paste0(
      "pubchem_",
      idtype,
      "_",
      query,
      "_",
      collection
    ),
    where = list(
      ands = list(
        where_clause
      )
    )
  )
  jsonlite::toJSON(
    sdq,
    auto_unbox = TRUE
  )
}

#' Query PubChem SDQ service
#'
#' Some resources are accessible through PubChem, but the PUG-View page does not
#' contain the data itself, only a pointer to the data. This function sends a
#' query to PubChem's SDQ (Structured Data Query) service and retrieves the
#' results in CSV format.
#' @param collection character; the collection to query (e.g., "chemidplus").
#' @param idtype character; the type of identifier to query (e.g., "cid", "sid").
#' @param query character; the identifier value to query.
#' @param limit numeric; the maximum number of results to return (default is 10000000).
#' @return A tibble containing the results of the SDQ query.
#' @noRd
pc_sdq_query <- function(
    collection,
    idtype,
    query,
    limit = 10000000
) {
  base_url <- "https://pubchem.ncbi.nlm.nih.gov/sdq/sphinxql.cgi"
  sdq_json <- pc_build_sdq_query(
    collection,
    idtype,
    query,
    limit
  )
  response <- httr::GET(
    base_url,
    query = list(
      infmt = "json",
      outfmt = "csv",
      query = sdq_json,
      showcolumndisplayname = 1
    )
  )
  httr::stop_for_status(response)
  utils::read.csv(text = content(response, "text")) |> tibble::as_tibble()
}
