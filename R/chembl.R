#' Retrieve FTP URL for ChEMBL database files
#'
#' @param version character; release version
#' @return FTP URL for ChEMBL databas files
#' @examples {
#' chembl_dir_url(version = "latest")
#' chembl_dir_url(version = "34")
#' chembl_dir_url(version = "24.1")
#' }
#' @noRd
chembl_dir_url <- function(version = "latest") {
  version <- validate_chembl_version(version = version)
  if (version %in% c("22", "24")) {
    url <- paste0(
      "https://ftp.ebi.ac.uk/pub/databases/chembl/ChEMBLdb/releases/chembl_",
      version, "/archived"
    )
  } else {
    url <- paste0(
      "https://ftp.ebi.ac.uk/pub/databases/chembl/ChEMBLdb/releases/chembl_",
      version
    )
  }
  if (url_exists(url)) {
    return(url)
  } else {
    msg <- paste0("ChEMBL URL not found: ", url)
    stop(msg)
  }
}

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
#' # Resource: "activity" - requires activity ID
#' chembl_query("31863", resource = "activity")
#' # Resource: "assay" - requires assay ChEMBL ID
#' chembl_query("CHEMBL615117", resource = "assay")
#' # Resource: "atc_class" - requires ATC class ID
#' chembl_query("A01AA01", resource = "atc_class")
#' # Resource: binding_site - requires site ID
#' chembl_query(2, resource = "binding_site")
#' # Resource: biotherapeutic - requires ChEMBL ID
#' chembl_query("CHEMBL448105", resource = "biotherapeutic")
#' # Resource: cell_line - requires ChEMBL ID
#' chembl_query("CHEMBL3307241", resource = "cell_line")
#' # Resource: chembl_id_lookup - requires ChEMBL ID
#' chembl_query("CHEMBL1", resource = "chembl_id_lookup")
#' # Resource: compound_record - requires record ID
#' chembl_query("1", resource = "compound_record")
#' # Resource: compound_structural_alert - requires compound structural alert ID
#' chembl_query("79048021", resource = "compound_structural_alert")
#' # Resource: document - requires document ChEMBL ID
#' chembl_query("CHEMBL1158643", resource = "document")
#' # Resource: document_similarity - requires document 1 ChEMBL ID
#' chembl_query("CHEMBL1148466", resource = "document_similarity")
#' # Resource: drug - requires ChEMBL ID
#' chembl_query("CHEMBL2", resource = "drug")
#' # Resource: drug_indication - requires drug indication ID
#' chembl_query("22606", resource = "drug_indication")
#' # Resource: drug_warning - requires warning ID
#' chembl_query("1", resource = "drug_warning")
#' # Resource: go_slim - requires GO ID
#' chembl_query("GO:0000003", resource = "go_slim")
#' # Resource: mechanism - requires mechanism ID
#' chembl_query("13", resource = "mechanism")
#' # Resource: metabolism - requires metabolism ID
#' chembl_query("119", resource = "metabolism")
#' # Resource: molecule - requires ChEMBL ID
#' chembl_query("CHEMBL1082", resource = "molecule")
#' chembl_query(c("CHEMBL25", "CHEMBL1082"), resource = "molecule")
#' # Resource: molecule_form - requires ChEMBL ID
#' chembl_query("CHEMBL6329", resource = "molecule_form")
#' # Resource: organism - requires organism class ID (not taxid)
#' chembl_query("1", resource = "organism")
#' # Resource: protein_classification - requires protein class ID
#' chembl_query("1", resource = "protein_classification")
#' # Resource: similarity - requires SMILES
#' chembl_query("CC(=O)Oc1ccccc1C(=O)O/70", resource = "similarity")
#' # Resource: source - requires source ID
#' chembl_query("1", resource = "source")
#' # Resource: substructure - requires SMILES
#' chembl_query("CN(CCCN)c1cccc2ccccc12", resource = "substructure")
#' # Resource: target - requires target ChEMBL ID
#' chembl_query("CHEMBL2074", resource = "target")
#' # Resource: target_component - requires target component ID
#' chembl_query("1", resource = "target_component")
#' # Resource: target_relation - requires target ChEMBL ID
#' chembl_query("CHEMBL2251", resource = "target_relation")
#' # Resource: tissue - requires tissue ChEMBL ID
#' chembl_query("CHEMBL3988026", resource = "tissue")
#' # Resource: xref_source - requires the name of the resource
#' chembl_query("AlphaFoldDB", resource = "xref_source")
#' }
#' @importFrom httr RETRY message_for_status content
#' @export
chembl_query <- function(query,
                         resource = "molecule",
                         cache_file = NULL,
                         verbose = getOption("verbose"),
                         test_service_down = FALSE) {
  resource <- match.arg(resource, chembl_resources())
  if (resource == "image") {
    stop("To download images, please use chembl_img().")
  }
  stem <- "https://www.ebi.ac.uk/chembl/api/data"
  foo <- function(query, verbose) {
    if (is.na(query)) {
      if (verbose) webchem_message("na")
      return(NA)
    }
    query <- chembl_validate_query(query, resource, verbose)
    if (is.na(query)) return(NA)
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

#' Download images from ChEMBL
#'
#' Retrieve images of substances from ChEMBL and save them in SVG format.
#' @param query character; a vector of ChEMBL IDs.
#' @param dir character; the directory in which to save the image(s). If the
#' directory does not exist, it will be created. If `NULL` (default), the
#' working directory will be used.
#' @param verbose logical; should verbose messages be printed to the console?
#' @param test_service_down logical; for internal testing only.
#' @return Creates the download directory if needed and downloads one or more
#' SVG files.
#' @examples
#' \dontrun{
#' # download a single image
#' chembl_img("CHEMBL1")
#' # download a single image to another directory
#' chembl_img("CHEMBL1", dir = tempdir())
#' # download multiple images
#' chembl_img(c("CHEMBL1", "CHEMBL2"))
#' }
#' @export
chembl_img <- function(
  query,
  dir = NULL,
  verbose = getOption("verbose"),
  test_service_down = FALSE
  ) {
  stem <- "https://www.ebi.ac.uk/chembl/api/data"
  foo <- function(query, verbose) {
    if (verbose) webchem_message("query", query, appendLF = FALSE)
    query <- chembl_validate_query(query, "image", verbose)
    if (is.na(query)) return(NA)
    webchem_sleep(type = "API")
    url <- ifelse(
      test_service_down,
      "",
      paste0(stem, "/image/", query)
    )
    if (is.null(dir)) {
      path <- paste0(query, ".svg")
    } else {
      if(!dir.exists(dir)) dir.create(dir, recursive = TRUE)
      path <- paste0(dir, "/", query, ".svg")
    }
    if (file.exists(path)) {
      if (verbose) message("Already downloaded.")
      return()
    }
    res <- try(httr::RETRY("GET",
                           url,
                           httr::user_agent(webchem_url()),
                           terminate_on = 404,
                           httr::write_disk(path, overwrite = TRUE),
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
    return(NA)
  }
  out <- lapply(query, function(x) foo(x, verbose = verbose))
}

chembl_validate_query <- function(query, resource, verbose) {
  resource <- match.arg(resource, chembl_resources())
  if (is.na(query)) {
    if (verbose) webchem_message("na")
    return(NA)
  }
  if (resource %in% c(
    "activity",
    "binding_site",
    "compund_record",
    "compound_structural_alert",
    "drug_indication",
    "drug_warning",
    "mechanism",
    "metabolism",
    "organism",
    "protein_classification",
    "source"
  )) {
    query_numeric <- suppressWarnings(as.numeric(query))
    if (is.na(query_numeric)) {
      if (verbose) message("Query must be coercible to numeric. Returning NA.")
      return(NA)
    }
  } else if (resource %in% c(
    "assay",
    "biotherapeutic",
    "chembl_id_lookup",
    "document",
    "document_similarity",
    "drug",
    "image",
    "molecule",
    "molecule_form",
    "target",
    "tissue"
  )) {
    if (!grepl("^CHEMBL[0-9]+", query)) {
      if (verbose) message("Query must be a ChEMBL ID. Returning NA.")
      return(NA)
    }
  }
  return(query)
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
#' @note The list was compiled manually using the following url:
#' \url{https://chembl.gitbook.io/chembl-interface-documentation/web-services/chembl-data-web-services}
#' @references Gaulton, A., Bellis, L. J., Bento, A. P., Chambers, J.,
#' Davies, M., Hersey, A., ... & Overington, J. P. (2012). ChEMBL: a large-scale
#' bioactivity database for drug discovery. Nucleic acids research, 40(D1),
#' D1100-D1107.
#' @export
chembl_resources <- function() {
  resources <- c(
    "activity", "assay", "atc_class", "binding_site", "biotherapeutic", "cell_line",
    "chembl_id_lookup", "compound_record", "compound_structural_alert",
    "document", "document_similarity", "drug", "drug_indication",
    "drug_warning", "go_slim", "image", "mechanism",  "metabolism",
    "molecule", "molecule_form", "organism", "protein_classification", "similarity",
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

#' Validate ChEMBL version
#'
#' @description Validates the provided ChEMBL version. If "latest" (default),
#' returns the number of the lastest supported version (as a string). If the
#' provided version is lower than the earliest supported version, stops with
#' an error.
#' @param version character; the ChEMBL version to validate.
#' @return Validated version number as a string.
#' @noRd
validate_chembl_version <- function(version = "latest") {
  assert(version, "character")
  stopifnot(length(version) == 1)
  if (version == "latest") version <- "35"
  version_num <- suppressWarnings(as.numeric(version))
  if (is.na(version_num)) {
    stop("Version must be 'latest' or coercible to numeric.")
  }
  if (version_num < 20) {
    stop("Version not supported. Try a more recent version.")
  }
  if (version_num %% 1 != 0) version <- gsub("\\.", "_", version)
  return(version)
}
