#' Query ChEMBL using ChEMBL IDs
#'
#' Retrieve ChEMBL data using a vector of ChEMBL IDs.
#' @param query character; a vector of ChEMBL IDs.
#' @param resource character; the ChEMBL resource to query. Use
#' [chembl_resources()] to see all available resources.
#' @param cache_file character; the name of the cache file without the file
#' extension. If \code{NULL}, results are not cached.
#' @param verbose logical; should a verbose output be printed on the console?
#' @param test_service_down logical; this argument is only used for testing.
#' @return The function returns a list of lists, where each element of the list
#' contains a list of respective query results. Results are simplified, if
#' possible.
#' @details Each entry in ChEMBL has a unique ID. Data in ChEMBL is organized in
#' databases called resources. An entry may or may not have a record in a
#' particular resource. An entry may have a record in more than one resource,
#' e.g. a compound may be present in both the "molecule" and the "drug"
#' resource. This function queries a vector of ChEMBL IDs from a specific ChEMBL
#' resource.
#' @details If you are unsure which ChEMBL resource contains your ChEMBL ID,
#' use this function with the \code{"chembl_id_lookup"} resource to find the
#' appropriate resource for a ChEMBL ID. Note that \code{"chembl_id_lookup"} is
#' not a separate function but a resource used by \code{chembl_query}.
#' @details If \code{cache_file} is not \code{NULL} the function creates a
#' cache directory in the working directory and a cache file in the cache
#' directory. This file is used in subsequent calls of the function. The
#' function first tries to retrieve query results from the cache file and only
#' accesses the webservice if the ChEMBL ID cannot be found in the cache file.
#' The cache file is extended as new ChEMBL ID-s are queried during the session.
#' @note
#' Links to the webservice documentation:
#' \itemize{
#'      \item \url{https://chembl.gitbook.io/chembl-interface-documentation},
#'      \item \url{https://www.ebi.ac.uk/chembl/api/data/docs}
#' }
#' @references Gaulton, A., Bellis, L. J., Bento, A. P., Chambers, J.,
#' Davies, M., Hersey, A., ... & Overington, J. P. (2012). ChEMBL: a large-scale
#' bioactivity database for drug discovery. Nucleic acids research, 40(D1),
#' D1100-D1107.
#' @examples
#' \dontrun{
#' # Might fail if API is not available
#'
#' # Search molecules
#' chembl_query("CHEMBL1082", resource = "molecule")
#' chembl_query(c("CHEMBL25", "CHEMBL1082"), resource = "molecule")
#'
#' # Look up ChEMBL IDs in ChEMBL "resources", returns one resource per query.
#' chembl_query("CHEMBL771355", "chembl_id_lookup")
#'
#' # Search assays
#' chembl_query("CHEMBL771355", resource = "assay")
#' }
#' @importFrom httr RETRY message_for_status content
#' @export
chembl_query <- function(query,
                         resource = "molecule",
                         cache_file = NULL,
                         verbose = getOption("verbose"),
                         test_service_down = FALSE) {
  resource <- match.arg(resource, chembl_resources())
  stem <- "https://www.ebi.ac.uk/chembl/api/data"
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
    url <- ifelse(
      test_service_down, "", paste0(stem, "/", resource, "/", query, ".json"))
    webchem_sleep(type = "API")
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
    cont <- format_chembl(cont)
    return(cont)
  }
  if (is.null(cache_file)) {
    out <- lapply(query, function(x) foo(x, verbose = verbose))
  } else {
    if (!dir.exists("cache")) dir.create("cache")
    cfpath <- paste0("cache/", cache_file, ".rds")
    if (file.exists(cfpath)) {
      query_results <- readRDS(file = cfpath)
    } else {
      query_results <- list()
    }
    out <- lapply(query, function(x) {
      if (x %in% names(query_results)) {
        if (verbose) webchem_message("query", x, appendLF = FALSE)
        if (verbose) message("Already retrieved.")
        return(query_results[[x]])
      } else {
        new <- foo(x, verbose = verbose)
        if (!is.na(x)) {
          query_results[[x]] <<- new
          saveRDS(query_results, file = cfpath)
        }
        return(new)
        }
      })
  }
  return(out)
}

#' Retrieve all ATC classes
#'
#' Retrieve all available classes within the Anatomical Therapeutic Chemical
#' (ATC) classification system.
#' @param verbose logical; should a verbose output be printed on the console?
#' @param test_service_down logical; this argument is only used for testing.
#' @references Gaulton, A., Bellis, L. J., Bento, A. P., Chambers, J.,
#' Davies, M., Hersey, A., ... & Overington, J. P. (2012). ChEMBL: a large-scale
#' bioactivity database for drug discovery. Nucleic acids research, 40(D1),
#' D1100-D1107.
#' @examples
#' \dontrun{
#' # Might fail if API is not available
#'
#' atc <- atc_classes()
#' }
#' @importFrom httr RETRY user_agent message_for_status content
#' @importFrom tibble tibble as_tibble
#' @importFrom dplyr bind_rows
#' @export
chembl_atc_classes <- function(verbose = getOption("verbose"),
                               test_service_down = FALSE) {
  i = 0
  next_page <- "/chembl/api/data/atc_class.json?limit=1000&offset=0"
  atc_classes <- tibble::tibble()
  while (!is.null(next_page)) {
    i = i + 1
    url <- ifelse(
      test_service_down, "", paste0("https://www.ebi.ac.uk", next_page))
    webchem_sleep(type = "API")
    if (verbose) webchem_message("query", paste0("Page ", i), appendLF = FALSE)
    res <- try(httr::RETRY("GET",
                           url,
                           httr::user_agent(webchem_url()),
                           terminate_on = 404,
                           quiet = TRUE), silent = TRUE)
    if (inherits(res, "try-error")) {
      if (verbose) webchem_message("service_down")
      return(NA)
    }
    if (verbose) message(httr::message_for_status(res))
    cont <- httr::content(res, type = "application/json")
    new_classes <- lapply(cont$atc, function(x) tibble::as_tibble(x))
    new_classes <- dplyr::bind_rows(new_classes)
    atc_classes <- dplyr::bind_rows(atc_classes, new_classes)
    next_page <- cont$page_meta$`next`
  }
  atc_classes <- atc_classes[, c(
    "who_name",
    "level1", "level1_description",
    "level2", "level2_description",
    "level3", "level3_description",
    "level4", "level4_description",
    "level5"
  )]
  return(atc_classes)
}

#' List ChEMBL Resources
#'
#' Data in ChEMBL is organized in databases called resources. This function
#' lists available ChEMBL resources.
#' @note The list was compiled manually using the following url: \url{
#' https://chembl.gitbook.io/chembl-interface-documentation/web-services/
#' chembl-data-web-services}
#' @references Gaulton, A., Bellis, L. J., Bento, A. P., Chambers, J.,
#' Davies, M., Hersey, A., ... & Overington, J. P. (2012). ChEMBL: a large-scale
#' bioactivity database for drug discovery. Nucleic acids research, 40(D1),
#' D1100-D1107.
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

#' Format ChEMBL results
#'
#' Format ChEMBL results by collapsing nested lists into tibbles.
#' @param cont list; json output from \code{chembl_query()}
#' @noRd
format_chembl <- function(cont) {
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
  return(cont)
}
