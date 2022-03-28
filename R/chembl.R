#' Query ChEMBL using ChEMBL IDs
#'
#' Retrieve ChEMBL data using a vector of ChEMBL IDs.
#' @param query character; a vector of ChEMBL IDs.
#' @param resource character; the ChEMBL resource to query. Use
#' \code{chembl_resources()} to see all available resources.
#' @param verbose logical; should a verbose output be printed on the console?
#' @return The function return a list of lists, where each element of the list
#' contains a list of respective query results. Results are simplified, if
#' possible.
#' @details Each entry in ChEMBL has a unique ID. Data in ChEMBL is organized in
#' databases called resources. An entry may or may not have a record in a
#' particular resource. An entry may have a record in more than resources, e.g.
#' a compound may be present in both the "molecule" and the "drug" resource.
#' This function queries a vector of ChEMBL IDs from a specific ChEMBL resource.
#' Use this function with the \code{"chembl_id_lookup"} resource to find the
#' appropriate resource for a ChEMBL ID.
#' @note
#' Links to the webservice documentation:
#' \itemize{
#'      \item \url{https://chembl.gitbook.io/chembl-interface-documentation},
#'      \item \url{https://www.ebi.ac.uk/chembl/api/data/docs}
#' }
#' @examples
#' \donttest{
#' # Might fail if API is not available
#'
#' # Search molecules
#' chembl_query("CHEMBL1082", resource = "molecule")
#' chembl_query(c("CHEMBL25", "CHEMBL1082"), resource = "molecule")
#'
#' # Look up ChEMBL IDs in ChEMBL "resources", return one resource per query.
#' chembl_query("CHEMBL771355", "chembl_id_lookup")
#'
#' # Search assays
#' chembl_query("CHEMBL771355", resource = "assay")
#' }
#' @importFrom httr RETRY message_for_status content
#' @export
chembl_query <- function(query,
                         resource = "molecule",
                         cache = FALSE,
                         verbose = getOption("verbose")) {
  resource <- match.arg(resource, chembl_resources())
  stem <- "https://www.ebi.ac.uk/chembl/api/data"
  if (cache) {
    if (file.exists("query_results.rda")) load("query_results.rda") else {
      query_results <- list()
    }
  }
  foo <- function(query, verbose) {
    if (is.na(query)) {
      if (verbose) webchem_message("na")
      return(NA)
    }
    if (grepl("^CHEMBL[0-9]+", query) == FALSE) {
      if (verbose) message("Query is not a ChEMBL ID. Returning NA.")
      return(NA)
    }
    if (verbose) webchem_message("query", query, appendLF = FALSE)
    url <- paste0(stem, "/", resource, "/", query, ".json")
    webchem_sleep(type = 'API')
    res <- try(httr::RETRY("GET",
                           url,
                           httr::user_agent(webchem_url()),
                           terminate_on = 404,
                           quiet = TRUE), silent = TRUE)
    if (inherits(res, "try-error")) {
      if (verbose) webchem_message("service_down")
      return(NA)
    }
    if (res$status_code != 200) {
      if (verbose) message(httr::message_for_status(res))
      return(NA)
    }
    if (verbose) message(httr::message_for_status(res))
    cont <- httr::content(res, type = "application/json")
    if ("atc_classifications" %in% names(cont)) {
      cont$atc_classifications <- unlist(cont$atc_classifications)
    }
    if ("cross_references" %in% names(cont)) {
      cont$cross_references <- dplyr::bind_rows(cont$cross_references)
    }
    if ("molecule_hierarchy" %in% names(cont)) {
      cont$molecule_hierarchy <- dplyr::bind_rows(cont$molecule_hierarchy)
    }
    if ("molecule_properties" %in% names(cont)) {
      cont$molecule_properties <- dplyr::bind_rows(cont$molecule_properties)
    }
    if ("molecule_structures" %in% names(cont)) {
      cont$molecule_structures <- dplyr::bind_rows(cont$molecule_structures)
    }
    if ("molecule_synonyms" %in% names(cont)) {
      cont$molecule_synonyms <- dplyr::bind_rows(cont$molecule_synonyms)
    }
    cont$molecule_synonyms <- dplyr::bind_rows(cont$molecule_synonyms)
    return(cont)
  }
  out <- lapply(query, function(x) {
    if (cache) {
      if (x %in% names(query_results)) {
        if (verbose) webchem_message("query", x, appendLF = FALSE)
        if (verbose) message("Already retrieved.")
        return(query_results[[x]])
      } else {
        new <- foo(x, verbose)
        if (length(new[[1]]) == 1 && is.na(new[[1]][1])) {
          return(new)
        } else {
          query_results[[x]] <<- new
          save(query_results, file = "query_results.rda")
          return(new)
        }
      }
    } else {
      foo(x, verbose)
    }
  })
  return(out)
}

#' List ChEMBL Resources
#'
#' Data in ChEMBL is organized in databases called resources. This function
#' lists available ChEMBL resources.
#' @export
chembl_resources <- function() {
  resources <- c(
    "activity", "activity_supplementary_data_by_activity", "assay",
    "assay_class", "atc_class", "binding_site", "biotherapeutic", "cell_line",
    "chembl_id_lookup", "compound_record", "compound_structural_alert",
    "document", "document_similarity", "drug", "drug_indication",
    "drug_warning", "go_slim", "image", "mechanism",  "metabolism",
    "molecule", "molecule_form", "organism", "protein_class", "similarity",
    "source", "status", "substructure", "target", "target_component",
    "target_relation", "tissue", "xref_source"
  )
  return(resources)
}

#' Retrieve all ATC classes
#'
#' Retrieve all available classes within the Anatomical Therapeutic Chemical
#' (ATC) classification system.
#' @param verbose logical; should a verbose output be printed on the console?
#' \donttest{
#' # Might fail if API is not available
#'
#' atc <- atc_classes()
#' }
#' @importFrom httr RETRY user_agent message_for_status content
#' @importFrom tibble as_tibble
#' @importFrom dplyr bind_rows
#' @export
atc_classes <- function(verbose = getOption("verbose")) {
  url <- "https://www.ebi.ac.uk/chembl/api/data/atc_class.json?limit=1000&offset=0"
  webchem_sleep(type = 'API')
  res <- try(httr::RETRY("GET",
                         url,
                         httr::user_agent(webchem_url()),
                         terminate_on = 404,
                         quiet = TRUE), silent = TRUE)
  if (inherits(res, "try-error")) {
    if (verbose) webchem_message("service_down")
    return(NA)
  }
  if (res$status_code != 200) {
    if (verbose) message(httr::message_for_status(res))
    return(NA)
  }
  if (verbose) message(httr::message_for_status(res))
  cont <- httr::content(res, type = "application/json")
  atc_classes <- lapply(cont$atc, function(x) tibble::as_tibble(x))
  atc_classes <- dplyr::bind_rows(atc_classes)
  next_page <- cont$page_meta$`next`
  while(!is.null(next_page)) {
    url <- paste0("https://www.ebi.ac.uk", next_page)
    webchem_sleep(type = 'API')
    res <- try(httr::RETRY("GET",
                           url,
                           httr::user_agent(webchem_url()),
                           terminate_on = 404,
                           quiet = TRUE), silent = TRUE)
    if (inherits(res, "try-error")) {
      if (verbose) webchem_message("service_down")
      return(NA)
    }
    if (res$status_code != 200) {
      if (verbose) message(httr::message_for_status(res))
      return(NA)
    }
    if (verbose) message(httr::message_for_status(res))
    cont <- httr::content(res, type = "application/json")
    new_classes <- lapply(cont$atc, function(x) tibble::as_tibble(x))
    new_classes <- dplyr::bind_rows(new_classes)
    atc_classes <- dplyr::bind_rows(atc_classes, new_classes)
    next_page <- cont$page_meta$`next`
  }
  atc_classes <- atc_classes[,c(
    "who_name",
    "level1", "level1_description",
    "level2", "level2_description",
    "level3", "level3_description",
    "level4", "level4_description",
    "level5"
  )]
  return(atc_classes)
}