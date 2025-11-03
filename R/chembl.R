#' Download ChEMBL database
#'
#' Download a version of the ChEMBL database for offline access.
#' @param version character, the database release version.
#' @param verbose logical; should verbose messages be printed to the console?
#' @return Downloads the requested database files.
#' @note If a checksum file is available for the requested version it will be
#' used to check data integrity. To save storage space, webchem only retrieves
#' those files that are used by the package. If you need other files as well,
#' please download them manually.
#' @references You can find more information about ChEMBL releases at
#' \url{https://chembl.gitbook.io/chembl-interface-documentation/downloads}
#' @examples
#' \dontrun{
#' db_download_chembl(version = "35", verbose = TRUE)
#' }
#' @export
db_download_chembl <- function(
    version = "latest",
    verbose = getOption("verbose")
) {
  # input validation
  if (!inherits(version, "chembl_version")) {
    version <- validate_chembl_version(version = version)
  }
  stopifnot(is.logical(verbose), length(verbose) == 1)
  # get paths
  paths <- db_files("chembl", version = version)
  paths$url_exists <- url_exists(paths$url)
  if (!paths$url_exists[1]) {
    warning("Checksum file not found. Data integrity will not be checked.")
  }
  if (any(!paths$url_exists[-1])) {
    msg <- paste0(
      "The following ChEMBL URLs were not found:\n",
      paste(paths$url[which(!paths$url_exists)], collapse = "\n")
    )
    stop(msg)
  }
  dir_path <- file.path(
    wc_cache$cache_path_get(),
    "chembl",
    paste0("chembl_", version$version_path)
  ) |> path.expand()
  checksum_file <- file.path(
    dir_path,
    paths$file[which(paths$type == "checksum")]
  )
  # download data
  for (i in 1:nrow(paths)) {
    if (verbose) {
      msg <- paste0("Downloading ", paths$url[i], ". ")
      message(msg, appendLF = FALSE)
    }
    if (!paths$url_exists[i]) {
      if (verbose) message("URL not found.")
      next()
    }
    download_path <- file.path(dir_path, basename(paths$url[i]))
    if (file.exists(download_path)) {
      if (verbose) message("Already downloaded.")
      next()
    }
    # make db dir if not already present
    if (!dir.exists(dir_path)) dir.create(dir_path, recursive = TRUE)
    # download data
    curl::curl_download(paths$url[i], download_path, quiet = TRUE)
    if (verbose) message("Done.")
    # check data integrity
    if (file.exists(checksum_file) && paths$type[i] != "checksum") {
      if (verbose) message("  Checking data integrity. ", appendLF = FALSE)
      df <- read.table(checksum_file, skip = 2)
      names(df) <- c("checksum", "file")
      if (nchar(df$checksum[1]) != 64) {
        names(df) <- c("file", "checksum")
      }
      sha256sum <- digest::digest(file = download_path, algo = "sha256")
      check <- df$checksum[which(df$file == basename(paths$url[i]))]
      if (sha256sum != check) {
        msg <- paste0("Checksum error, data may be corrupted: ", basename(paths$url[i]))
        stop(msg)
      }
      if (verbose) message("Done.")
    }
    # post processing
    if (paths$type[i] == "sqlite") {
      final_file <- file.path(dir_path, paths$file[i])
      if (file.exists(final_file)) next()
      if (verbose) message("  Unzipping SQLite database. ", appendLF = FALSE)
      utils::untar(download_path, exdir = dir_path)
      if (verbose) message("Done.")
      if (verbose) message(
        "  Checking SQLite database final location. ", appendLF = FALSE)
      if (!file.exists(final_file)) {
        warning("Unexpected archive contents. Please raise an issue at https://github.com/ropensci/webchem.")
        next()
      }
      if (verbose) message("Done.")
    }
  }
}

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
  if (!inherits(version, "chembl_version")) {
    version <- validate_chembl_version(version = version)
  }
  if (version$version_path %in% c("22", "24")) {
    url <- paste0(
      "https://ftp.ebi.ac.uk/pub/databases/chembl/ChEMBLdb/releases/chembl_",
      version$version_path, "/archived"
    )
  } else {
    url <- paste0(
      "https://ftp.ebi.ac.uk/pub/databases/chembl/ChEMBLdb/releases/chembl_",
      version$version_path
    )
  }
  if (url_exists(url)) {
    return(url)
  } else {
    msg <- paste0("ChEMBL URL not found: ", url)
    stop(msg)
  }
}

#' Retrieve paths for ChEMBL database files
#'
#' @param version character; version of the database. Either "latest" (default)
#' or a specific version number, e.g. "30".
#' @return a data frame with three columns "url", "file" and "type". "url" is
#' the download URL. "file" is the final path to the file within the download
#' directory of the requested database version. "type" is the file type which
#' guides further processing.
#' @examples
#' chembl_files("chembl", version = "latest")
#' chembl_files("chembl", version = "30")
#' @noRd
chembl_files <- function(version = "latest") {
  if (!inherits(version, "chembl_version")) {
    version <- validate_chembl_version(version = version)
  }
  dir_url <- chembl_dir_url(version = version)
  # set default paths
  path_list <- list(
    checksum = data.frame(
      url = file.path(dir_url, "checksums.txt"),
      file = "checksums.txt"
    ),
    sqlite = data.frame(
      url = file.path(
        dir_url, paste0("chembl_", version$version_path, "_sqlite.tar.gz")),
      file = file.path(
        paste0("chembl_", version$version_path),
        paste0("chembl_", version$version_path, "_sqlite"),
        paste0("chembl_", version$version_path, ".db")
      )
    )
  )
  # override defaults paths
  if (as.numeric(version$version_base) <= 22) {
    path_list$sqlite$file <-  file.path(
      paste0("chembl_", version$version_path, "_sqlite"),
      paste0("chembl_", version$version_base, ".db")
    )
  } else if (version$version == "24.1") {
    path_list$sqlite$file <-  file.path(
      paste0("chembl_", version$version_base),
      paste0("chembl_", version$version_base, "_sqlite"),
      paste0("chembl_", version$version_base, ".db")
    )
  }
  out <- do.call(rbind, Map(cbind, path_list, type = names(path_list)))
  rownames(out) <- NULL
  out <- out |> dplyr::relocate("type", "file", "url")
  return(out)
}

#' Query ChEMBL using ChEMBL IDs
#'
#' Retrieve ChEMBL data using a vector of ChEMBL IDs.
#' @param query character; a vector of ChEMBL IDs.
#' @param resource character; the ChEMBL resource to query. Use
#' [chembl_resources()] to see all available resources.
#' @param tidy logical; attempt to convert output to a simpler structure.
#' @param cache_file character; the name of the cache file without the file
#' extension. If \code{NULL}, results are not cached.
#' @param verbose logical; should a verbose output be printed on the console?
#' @param similarity numeric; similarity threshold for similarity searches.
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
#' # Resource: compound_structural_alert - requires compound ChEMBL ID
#' chembl_query("CHEMBL266429", resource = "compound_structural_alert")
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
#' # By default, the function will use 70 as similarity threshold
#' chembl_query("CC(=O)Oc1ccccc1C(=O)O", resource = "similarity")
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
                         tidy = TRUE,
                         cache_file = NULL,
                         similarity = 70,
                         verbose = getOption("verbose"),
                         test_service_down = FALSE) {
  resource <- match.arg(resource, chembl_resources())
  if (resource == "image") {
    stop("To download images, please use chembl_img().")
  }
  if (resource == "status") {
    stop("To retrieve webservise status, please use chembl_status().")
  }
  if (resource == "similarity") {
    warning("Similarity search currently returns no more than 20 results.")
  }
  stem <- "https://www.ebi.ac.uk/chembl/api/data"
  foo <- function(query, verbose, similarity) {
    if (is.na(query)) {
      if (verbose) webchem_message("na")
      return(NA)
    }
    query <- chembl_validate_query(query, resource, verbose)
    if (is.na(query)) return(NA)
    if (verbose) webchem_message("query", query, appendLF = FALSE)
    if (test_service_down) {
      url <- ""
    } else if (resource == "similarity") {
      url <- paste0(stem, "/", resource, "/", query, "/", similarity, ".json")
    } else if (resource == "compound_structural_alert") {
      url <- paste0(stem, "/", resource, ".json?molecule_chembl_id=", query)
    } else {
      url <- paste0(stem, "/", resource, "/", query, ".json")
    }

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
    if (tidy) {
      cont <- format_chembl(cont)
    }
    return(cont)
  }
  if (is.null(cache_file)) {
    out <- lapply(query, function(x) {
      foo(x, verbose = verbose, similarity = similarity)
    })
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
        new <- foo(x, verbose = verbose, similarity = similarity)
        if (!is.na(x)) {
          query_results[[x]] <<- new
          saveRDS(query_results, file = cfpath)
        }
        return(new)
        }
      })
  }
  names(out) <- query
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

#' Retrieve ChEMBL webservice status
#'
#' Retrieve status information about the ChEMBL webservice: database version,
#' release date, status, and count data for various entities.
#' @param verbose logical; should verbose messages be printed to the console?
#' @param test_service_down logical; for internal testing only.
#' @export
chembl_status <- function(
    verbose = getOption("verbose"),
    test_service_down = FALSE) {
  url <- ifelse(test_service_down, "",
    "https://www.ebi.ac.uk/chembl/api/data/status")
  webchem_sleep(type = "API")
  if (verbose) message("Querying ChEMBL status. ", appendLF = FALSE)
  res <- try(httr::RETRY("GET",
                         url,
                         httr::user_agent(webchem_url()),
                         httr::add_headers(Accept = "application/json"),
                         terminate_on = 404,
                         quiet = TRUE), silent = TRUE)
  if (inherits(res, "try-error")) {
    if (verbose) webchem_message("service_down")
    return(NA)
  }
  if (verbose) message(httr::message_for_status(res))
  if (res$status_code != 200) {
    return(NA)
  } else {
    return(httr::content(res, type = "application/json"))
  }
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
    "compound_structural_alert",
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
  # Names of elements to convert to data frames (using dplyr::bind_cols)
  df_names <- c(
    "action_type",
    "activity_properties",
    "assay_classifications",
    "assay_parameters",
    "atc_classification",
    "biotherapeutic",
    "biocomponents",
    "chembl_release",
    "compound_structural_alerts",
    "cross_references",
    "go_slims",
    "indication_refs",
    "ligand_efficiency",
    "mechanism_refs",
    "metabolism_refs",
    "molecule_forms",
    "molecule_hierarchy",
    "molecule_properties",
    "molecule_structures",
    "molecule_synonyms",
    "page_meta",
    "protein_classifications",
    "site_components",
    "target_component_synonyms",
    "target_component_xrefs",
    "targets",
    "variant_sequence",
    "warning_refs"
  )
  flat_names <- c(
    "applicants",
    "atc_classifications",
    "research_codes",
    "synonyms"
  )

  # Helper to flatten a single entity
  flatten_entity <- function(entity) {
    for (nm in df_names) {
      # e.g. activity is sometimes a list sometimes a character string
      # only flatten if it is a list to prevent error.
      if (nm %in% names(entity)) {
        if (inherits(entity[[nm]], "list")) {
          entity[[nm]] <- dplyr::bind_rows(entity[[nm]])
        }
      }
    }
    for (nm in flat_names) {
      if (nm %in% names(entity)) {
        entity[[nm]] <- unlist(entity[[nm]])
      }
    }
    entity
  }

  # If 'molecule' is present and is a list, flatten its children
  if ("molecules" %in% names(cont)) {
    for (i in 1:length(cont[["molecules"]])) {
      cont[["molecules"]][[i]] <- flatten_entity(cont[["molecules"]][[i]])
    }
  }

  if ("target_components" %in% names(cont)) {
    for (i in 1:length(cont[["target_components"]])) {
      cont[["target_components"]][[i]] <- flatten_entity(cont[["target_components"]][[i]])
    }
  }

  # Also flatten top-level if needed
  cont <- flatten_entity(cont)

  # map NULL to NA in R
  cont <- lapply(cont, function(x) {
    if (is.null(x)) {
      return(NA_character_)
    } else {
      return(x)
    }
  })

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
  version_base <- as.character(floor(version_num))
  if (is.na(version_num)) {
    stop("Version must be 'latest' or coercible to numeric.")
  }
  if (version_num < 20) {
    stop("Version not supported. Try a more recent version.")
  }
  if (version_num %% 1 != 0) {
    version_path <- gsub("\\.", "_", version)
  } else {
    version_path <- version
  }
  out <- list(
    version = version,
    version_path = version_path,
    version_base = version_base
  )
  class(out) <- c("chembl_version", class(version))
  return(out)
}

#' Get ChEMBL Resource Structure
#'
#' Query a ChEMBL resource with a representative example and convert the
#' response into a structured table.
#'
#' @importFrom rlang .data
#' @param resource character; the ChEMBL resource to query. Use
#' [chembl_resources()] to see all available resources.
#' @param verbose logical; should verbose messages be printed to the console?
#' @return A tibble with columns "name", "value", and "parent" representing the
#' structure of the response.
#' @examples
#' \dontrun{
#' # Example for the "molecule" resource
#' get_chembl_ws_schema("molecule")
#' }
#' @noRd
get_chembl_ws_schema <- function(resource, verbose = getOption("verbose")) {
  # validate resource and get example query
  query <- chembl_example_query(resource)
  response <- chembl_query(query, resource = resource, verbose = verbose)
  process_element <- function(res, element, parent = NA) {
    value <- res[[element]]
    cls <- class(value)[1]
    out <- tibble::tibble(
      date = Sys.Date(),
      resource = resource,
      field = element,
      class = cls,
      parent = parent
    )
    if (is.atomic(value)) {
      out$value <- as.character(value[1])
    } else if (is.data.frame(value)) {
      df_fields <- tibble::tibble(
        date = Sys.Date(),
        resource = resource,
        field = names(value),
        class = vapply(value, function(col) class(col)[1], character(1)),
        parent = element,
        value = vapply(value, function(col) {
          non_na <- col[!is.na(col)]
          if (length(non_na) > 0) as.character(non_na[1]) else NA_character_
        }, character(1))
      )
      out <- dplyr::bind_rows(out, df_fields)
    } else {
      stop()
    }
    return(out)
  }
  all <- tibble::tibble()
  for (i in query) {
    result <- lapply(names(response[[i]]), function(x) {
      tryCatch(
      process_element(
        res = response[[i]],
        element = x
        ),
        error = function(e) {
          tibble::tibble(resource = resource, query = i, field = x)
        }
      )
    }) |> dplyr::bind_rows()
    result$query <- i
    result <- try(result |> dplyr::relocate(query, .after = .data$parent))
    if (inherits(result, "try-error")) {
      print(query)
      print(resource)
      stop()
    }
    all <- dplyr::bind_rows(all, result)
  }
  all <- all |>
    dplyr::group_by(.data$field, .data$parent) |>
    dplyr::filter(
      if (any(!is.na(.data$value))) {
        dplyr::row_number() == which(!is.na(.data$value))[1]
      } else {
        dplyr::row_number() == 1
      }
    ) |>
    dplyr::ungroup()
  all <- all |>
    dplyr::mutate(
      parent_row = match(.data$parent, .data$field),
      order_index = ifelse(is.na(.data$parent_row), dplyr::row_number(), .data$parent_row + 0.1)
    ) |>
    dplyr::arrange(.data$order_index) |>
    dplyr::select(-.data$parent_row, -.data$order_index)

  if ("value" %in% names(all)) {
    na_fields <- all$field[is.na(all$value) & all$class != "tbl_df"]
    if (length(na_fields) > 0) {
      warning("Found NA values in atomic fields. Add more example queries.")
      msg <- paste0("Resource with NA values: ", resource, "; ", paste(na_fields, collapse = ", "))
      print(msg)
    }
  }
  return(all)
}

chembl_example_query <- function(resource) {
  resource <- match.arg(resource, chembl_resources())
  resource <- resource[!resource %in% c("image", "status")]
  example_queries <- list(
    activity = c("31863", "32190", "624419", "31910", "31864", "3269724", "17805339"),
    assay = c("CHEMBL615117", "CHEMBL1061852", "CHEMBL5445082", "CHEMBL5441382", "CHEMBL2184458"),
    atc_class = "A01AA01",
    binding_site = "2",
    biotherapeutic = c("CHEMBL8234","CHEMBL448105"),
    cell_line = c("CHEMBL3307241", "CHEMBL3307242"),
    chembl_id_lookup = "CHEMBL1",
    compound_record = "1",
    compound_structural_alert = "CHEMBL266429",
    document = c("CHEMBL1158643", "CHEMBL1132398", "CHEMBL5303573", "CHEMBL3639173"),
    document_similarity = "CHEMBL1148466",
    drug = "CHEMBL2",
    drug_indication = "22606",
    drug_warning = "1",
    go_slim = "GO:0000003",
    image = "CHEMBL1",
    mechanism = "13",
    metabolism = c("119", "623", "180"),
    molecule = c("CHEMBL1082", "CHEMBL8234"),
    molecule_form = "CHEMBL6329",
    organism = "1",
    protein_classification = "1",
    similarity = "CC(=O)Oc1ccccc1C(=O)O",
    source = c("1", "5"),
    substructure = "CN(CCCN)c1cccc2ccccc12",
    target = "CHEMBL2074",
    target_component = "1",
    target_relation = "CHEMBL2251",
    tissue = "CHEMBL3988026",
    xref_source = "AlphaFoldDB"
  )
  if (!resource %in% names(example_queries)) {
    stop(paste0("No example query available for this resource: ", resource))
  }
  example_queries[[resource]]
}
