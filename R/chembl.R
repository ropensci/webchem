#' Query ChEMBL using ChEMBL IDs
#'
#' Retrieve ChEMBL data using a vector of ChEMBL IDs.
#' @param query character; a vector of ChEMBL IDs.
#' @param resource character; the ChEMBL resource to query.
#' @return The function return a list of lists, where each element of the list
#' contains a list of respective query results. Results are simplified, if
#' possible.
#' @examples
#' \donttest {
#' # might fail if API is not available
#' chembl_query("CHEMBL1082", resource = "molecule")
#' chembl_query(c("CHEMBL25", "CHEMBL1082"), resource = "molecule")
#' chembl_query("CHEMBL771355", resource = "assay")
#' }
#' @importFrom jsonlite fromJSON
#' @export
chembl_query <- function(query, resource = "molecule") {
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
  resource <- match.arg(resource, resources)
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
