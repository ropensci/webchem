#' Query ChEMBL using ChEMBL IDs
#'
#' Retrieve ChEMBL data using a vector of ChEMBL IDs.
#' @param query character; a vector of ChEMBL IDs.
#' @param resource character; the ChEMBL resource to query. Use
#' \code{chembl_resources()} to see all available resources.
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
#' @importFrom jsonlite fromJSON
#' @export
chembl_query <- function(query, resource = "molecule") {
  resource <- match.arg(resource, chembl_resources())
  stem <- "https://www.ebi.ac.uk/chembl/api/data"
  if (length(query) == 1) {
    url <- paste0(stem, "/", resource, "/", query)
    res <- jsonlite::fromJSON(url)
  }
  if (length(query) > 1) {
    queries <- paste(query, collapse = ";")
    url <- paste0(stem, "/", resource, "/set/", queries)
    res <- jsonlite::fromJSON(url)
  }
  return(res)
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
