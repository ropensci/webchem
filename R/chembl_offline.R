#' examples
#' \dontrun{
#' chembl_query_offline(query = "CHEMBL1082", resource = "molecule")
#' }
chembl_query_offline <- function(
    query,
    resource = "molecule",
    version = "latest",
    verbose = getOption("verbose")
) {
  resource <- match.arg(resource, chembl_resources())
  if (!inherits(version, "chembl_version")) {
    version <- validate_chembl_version(version = version)
  }
  FUN <- paste0("chembl_offline_", resource)
  if (!exists(FUN)) {
    msg <- paste0(
      "Offline query for resource '", resource, "' is not implemented.")
    stop(msg)
  }
  do.call(FUN, args = list(
    query = query,
    version = version,
    verbose = verbose
  ))
}

#' Replicate ChEMBL API activity resource using a local ChEMBL database
#'
#' @param query character; Activity ID of the molecule to retrieve
#' @param version character; version of the ChEMBL database
#' @param verbose logical; print verbose messages to the console?
#' examples
#' \dontrun{
#' chembl_offline_activity("31863")
#' }
#' #' @noRd
chembl_offline_activity <- function(
    query,
    version = "latest",
    verbose = getOption("verbose")) {
  if (!inherits(version, "chembl_version")) {
    version <- validate_chembl_version(version = version)
  }
  # connect to local database
  con <- connect_chembl(version = version)
  activities <- tbl(con, "activities") |>
    dplyr::filter(activity_id == query) |>
    dplyr::select(
      "action_type",
      "activity_comment",
      "activity_id",
      "assay_id",
      "bao_endpoint",
      "data_validity_comment",
      "doc_id",
      "molregno",
      "pchembl_value",
      "potential_duplicate",
      "qudt_units",
      "record_id",
      "relation",
      "src_id",
      "standard_flag",
      "standard_relation",
      "standard_text_value",
      "standard_type",
      "standard_units",
      "standard_upper_value",
      "standard_value",
      "text_value",
      "toid",
      "type",
      "units",
      "uo_units",
      "upper_value",
      "value"
    ) |>
    dplyr::collect()
  activity_properties
  assay_chembl_id
  assay_description
  # NOTE schema png suggested activities::assay_id - assays::doc_id connection
  # but activities::assay_id - assays::assay_id seems to be the right connection
  # I used the variable "assay_id" for checking.
  assays <- tbl(con, "assays") |>
    dplyr::filter(assay_id == activities$activity_id) |>
    dplyr::select(

    ) |>
    dplyr::collect()

}

#' Replicate ChEMBL API molecule resource using a local ChEMBL database
#'
#' @param query character; ChEMBL ID of the molecule to retrieve
#' @param version character; version of the ChEMBL database
#' @param verbose logical; print verbose messages to the console?
#' examples
#' \dontrun{
#' chembl_offline_molecule(query = "CHEMBL1082")
#' }
#' @noRd
chembl_offline_molecule <- function(
    query,
    version = "latest",
    verbose = getOption("verbose")
  ){
  # validate input: if all na, stop, if any na remove, there is also a function.
  if (!inherits(version, "chembl_version")) {
    version <- validate_chembl_version(version = version)
  }
  # connect to local database
  con <- connect_chembl(version = version)
  # confirm chembl_id
  entity_type <- tbl(con, "chembl_id_lookup") |>
    dplyr::filter(.data$chembl_id %in% query) |>
    dplyr::select("chembl_id", "entity_type") |>
    dplyr::collect()
  # if missing, verbose message, NA

  for (i in 1:nrow(entity_type)) {
    if (entity_type$entity_type[i] != "COMPOUND") {
      msg <- paste0(
        entity_type$chembl_id[i], " is not a COMPOUND. It is a ",
        entity_type$entity_type[i], ".")
      stop(msg)
    }
  }
  ids <- tbl(con, "molecule_dictionary") |>
    dplyr::filter(.data$chembl_id %in% query) |>
    dplyr::select("chembl_id", "molregno") |>
    dplyr::collect()
  molecule_dictionary <- tbl(con, "molecule_dictionary") |>
    dplyr::filter(.data$chembl_id %in% query) |>
    dplyr::select(
      "availability_type",
      "black_box_warning",
      "chebi_par_id",
      "chembl_id",
      "chemical_probe",
      "chirality",
      "dosed_ingredient",
      "first_approval",
      "first_in_class",
      "indication_class",
      "inorganic_flag",
      "max_phase",
      "molecule_type",
      "natural_product",
      "oral",
      "orphan",
      "parenteral",
      "polymer_flag",
      "pref_name",
      "prodrug",
      "structure_type",
      "therapeutic_flag",
      "topical",
      "usan_stem",
      "usan_stem_definition",
      "usan_substem",
      "usan_year",
      "withdrawn_flag"
    ) |>
    dplyr::collect()
  atc_classifications <- tbl(con, "molecule_atc_classification") |>
    dplyr::filter(.data$molregno %in% ids$molregno) |>
    dplyr::select("molregno", "level5")
  extract_variable <- function(df, var) {
    if (nrow(df) == 0) {
      return(NULL)
    } else {
      return(df[[var]])
    }
  }
  biotherapeutics <- tbl(con, "biotherapeutics") |>
    dplyr::filter(.data$molregno %in% ids$molregno) |>
    dplyr::collect()
  # description, helm notation!
  molecule_hierarchy_raw <- tbl(con, "molecule_hierarchy") |>
    dplyr::filter(molregno %in% ids$molregno) |>
    dplyr::collect()
  molecule_properties <- tbl(con, "compound_properties") |>
    dplyr::filter(.data$molregno %in% ids$molregno) |>
    dplyr::select(
      "alogp",
      "aromatic_rings",
      "cx_logd",
      "cx_logp",
      "cx_most_apka",
      "cx_most_bpka",
      "full_molformula",
      "full_mwt",
      "hba",
      "hba_lipinski",
      "hbd",
      "hbd_lipinski",
      "heavy_atoms",
      "molecular_species",
      "molregno",
      "mw_freebase",
      "mw_monoisotopic",
      "np_likeness_score",
      "num_lipinski_ro5_violations",
      "num_ro5_violations",
      "psa",
      "qed_weighted",
      "ro3_pass",
      "rtb"
    ) |>
    dplyr::collect()
    molecule_structures <- tbl(con, "compound_structures") |>
      dplyr::filter(.data$molregno %in% ids$molregno) |>
      dplyr::select(
        "canonical_smiles",
        "molfile",
        "molregno",
        "standard_inchi",
        "standard_inchi_key"
      ) |>
      dplyr::collect()
    molecule_synonyms <- tbl(con, "molecule_synonyms") |>
      dplyr::filter(.data$molregno %in% ids$molregno) |>
      dplyr::select("molregno", "synonyms","syn_type") |>
      collect() |>
      dplyr::rename(molecule_synonym = synonyms) |>
      dplyr::arrange(molecule_synonym)
    out <- list()
    for (i in seq_along(query)) {
      if (!query[i] %in% ids$chembl_id) {
        if (verbose) {
          # Not found
        }
        out[i] <- NA_character_
        next()
      }
      query_molregno <- ids$molregno[which(ids$chembl_id == query[i])]
      # NULL TO NA CONVERSION!
      molecule_dictionary2 <- molecule_dictionary |>
        dplyr::filter(.data$chembl_id == query[i]) |>
        dplyr::select(-"chembl_id")
      atc_classifications2 <- atc_classifications |>
        dplyr::filter(.data$molregno == query_molregno) |>
        dplyr::select(-"molregno")
      biotherapeutics2 <- biotherapeutics |>
        dplyr::filter(.data$molregno == query_molregno) |>
        dplyr::select(-"molregno")


      molecule_hierarchy_raw2 <- molecule_hierarchy_raw |>
        dplyr::filter(.data$molregno == query_molregno) |>
        dplyr::select(-"molregno")
      molecule_hierarchy <- tibble::tibble(
        "active_chembl_id" = query,
        "molecule_chembl_id" = query,
        "parent_chembl_id" = tbl(con, "molecule_dictionary") |>
          dplyr::filter(.data$molregno == molecule_hierarchy_raw2$parent_molregno) |>
          dplyr::pull(.data$chembl_id)
      )

      molecule_properties2 <- molecule_properties |>
        dplyr::filter(.data$molregno == query_molregno) |>
        dplyr::select(-"molregno")

      molecule_structures2 <- molecule_structures |>
        dplyr::filter(.data$molregno == query_molregno) |>
        dplyr::select(-"molregno")

      molecule_synonyms2 <- molecule_synonyms |>
        dplyr::filter(.data$molregno == query_molregno) |>
        dplyr::select(-"molregno")

      out[[i]] <- c(
        molecule_dictionary2,
        list(
          "atc_classifications" = atc_classifications2,
          #"biotherapeutic" = biotherapeutic,
          #"helm_notation" = helm_notation,
          "molecule_chembl_id" = query[i],
          "molecule_hierarchy" = molecule_hierarchy,
          "molecule_properties" = molecule_properties2,
          "molecule_structures" = molecule_structures2,
          "molecule_synonyms" = molecule_synonyms2
        )
      )
      names(out)[i] <- query[i]
      out[[i]] <- out[[i]][sort(names(out[[i]]))]
    }
    return(out)
}

#' Retrieve schema information from a local ChEMBL database
#'
#' This function connects to a local ChEMBL database and retrieves schema
#' information: table names, field names, field types, and (non-NA) example
#' values for each field. This is an internal function to help construct offline
#' functions that mimic the schema of the web service.
#' @param version character; version of the ChEMBL database.
#' @return A data frame with columns: \code{table}, \code{field}, \code{type}
#' and \code{example}.
#' @noRd
chembl_offline_schema <- function(version = "latest") {
  con <- connect_chembl(version = version)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  tables <- DBI::dbListTables(con)
  fields <- lapply(tables, function(x) {
  data.frame(field = DBI::dbListFields(con, x))
  })
  names(fields) <- tables
  out <- dplyr::bind_rows(fields, .id = "table")
  out$type <- mapply(function(x, y) {
  res <- DBI::dbSendQuery(
    con,
    paste("SELECT", y, "FROM", x, "LIMIT 0", sep = " ")
  )
  A <- DBI::dbColumnInfo(res)$type
  DBI::dbClearResult(res)
  return(A)
  }, x = out$table, y = out$field)
  out$example <- mapply(function(x, y) {
  query <- sprintf("SELECT %s FROM %s WHERE %s IS NOT NULL LIMIT 1", y, x, y)
  res <- tryCatch(DBI::dbGetQuery(con, query)[[1]], error = function(e) NA)
  if (length(res) == 0) return(NA)
  return(res)
  }, x = out$table, y = out$field)
  return(out)
}


#' Compare results from the ChEMBL webservice and offline database.
#'
#' Query the ChEMBL webservice and offline database for a given resource using
#' an example query, to compare their results. List differences.
#'
#' @param resource character; ChEMBL resource to query.
#' @param version character; version of the ChEMBL database.
#' @param verbose logical; print verbose messages to the console?
#' @noRd
chembl_compare_service <- function(
  resource,
  version = "latest",
  verbose = getOption("verbose")
  ) {
  # validate resource and get example query
  query <- chembl_example_query(resource)
  # Query the webservice
  ws_result <- tryCatch(
    chembl_query(
      query = query,
      resource = resource,
      verbose = verbose
    )[[1]],
    error = function(e) NA
  )
  # Query the offline database
  offline_result <- tryCatch(
    chembl_query_offline(
      query = query,
      resource = resource,
      version = version,
      verbose = verbose
    )[[1]],
    error = function(e) NA
  )

  out <- compare_service_lists(x = ws_result, y = offline_result)

  return(out)
}

#' Compare Two Service Result Lists
#'
#' Compares two lists (typically from a webservice and an offline source) to
#' check for differences in their structure and content. The function checks for
#' missing or failed queries, ensures both inputs are lists, compares the names
#' and order of elements, and checks for differences in atomic elements and
#' data frames within the lists.
#' @param ws list; result from a webservice query.
#' @param offline list; result from an offline query.
#' @return A list with the comparison status and any unique elements found in
#' either input.
#' @nord
compare_service_lists <- function(ws, offline) {
  if (all(is.na(c(ws, offline)))) {
    return(list(
      status = "both failed",
      ws_unique_element = character(),
      offline_unique_element = character()
    ))
  }
  if (is.na(ws) && !is.na(offline)) stop("Webservice query failed.")
  if (!is.na(ws) && is.na(offline)) stop("Offline query failed.")
  if (!inherits(ws, "list")) stop("Webservice result is not a list.")
  if (!inherits(offline, "list")) stop("Offline result is not a list.")
  ws_unique_element <- setdiff(names(ws), names(offline))
  offline_unique_element <- setdiff(names(offline), names(ws))
  common_names <- intersect(names(ws), names(offline))
  ws_common_order <- names(ws)[names(ws) %in% common_names]
  offline_common_order <- names(offline)[names(offline) %in% common_names]
  if (!identical(ws_common_order, offline_common_order)) {
    stop(sprintf(
      "Names of common elements are not in the same order:\n  x: %s\n  y: %s",
      paste(x_common_order, collapse = ", "),
      paste(y_common_order, collapse = ", ")
    ))
  }
  for (n in names(common_names)) {
    ws_elem <- ws[[n]]
    off_elem <- offline[[n]]
    if (!identical(class(ws_elem), class(off_elem))) {
      stop(sprintf(
        "Element '%s' has different classes: webservice = %s, offline = %s.",
        n,
        paste(class(ws_elem), collapse = ", "),
        paste(class(off_elem), collapse = ", ")
      ))
    }
    if (is.atomic(ws_elem)) {
      if (!identical(ws_elem, off_elem)) {
        stop(sprintf(
          "Atomic element '%s' differs between webservice and offline.\n",
          "Webservice value: %s\nOffline value: %s",
          n,
          paste0(capture.output(print(ws_elem)), collapse = "\n"),
          paste0(capture.output(print(off_elem)), collapse = "\n")
        ))
      }
    } else if (is.data.frame(ws_elem)) {
      element_comparison <- compare_atomic_lists(ws_elem, off_elem)
      if (length(element_comparison$unique_to_x) > 0) {
        ws_unique_element = c(ws_unique_element, paste0(
          n, "$", element_comparison$unique_to_x
        ))
      }
      if (length(element_comparison$unique_to_y) > 0) {
        offline_unique_element = c(offline_unique_element, paste0(
          n, "$", element_comparison$unique_to_y
        ))
      }
    } else {
      stop(sprintf("Element '%s' should be either atomic or a data frame.", n))
    }
    return(list(
      status = "OK",
      ws_extra = ws_unique_element,
      offline_extra = offline_unique_element
    ))
  }
}

#' Compare Two Named Lists of Atomic Elements
#'
#' Compares two named lists (`x` and `y`) containing atomic elements. The
#' function checks for elements unique to each list (by name);  common elements
#' with the same names, ensuring their order matches; whether all common
#' elements are atomic and identical. If the names of common elements are not
#' in the same order, or if any common element is not atomic or differs between
#' the lists, the function throws an error.
#'
#' @param x list; a named list of atomic elements.
#' @param y list; a named list of atomic elements.
#' @return A list with two components:
#'   \itemize{
#'     \item{unique_to_x}{Names present only in \code{x}.}
#'     \item{unique_to_y}{Names present only in \code{y}.}
#'   }
#' @examples
#' x <- list(a = 1, b = 2, c = 3)
#' y <- list(a = 1, b = 2, d = 4)
#' compare_atomic_lists(x, y)
#' @noRd
compare_atomic_lists <- function(x, y) {
  unique_a <- setdiff(names(x), names(y))
  unique_b <- setdiff(names(y), names(x))
  common_names <- intersect(names(x), names(y))
  x_common_order <- names(x)[names(x) %in% common_names]
  y_common_order <- names(y)[names(y) %in% common_names]
  if (!identical(x_common_order, y_common_order)) {
    stop(sprintf(
      "Names of common elements are not in the same order:\n  x: %s\n  y: %s",
      paste(x_common_order, collapse = ", "),
      paste(y_common_order, collapse = ", ")
    ))
  }
  for (n in common_names) {
    elem_a <- x[[n]]
    elem_b <- y[[n]]
    if (!is.atomic(elem_a) || !is.atomic(elem_b)) {
      stop(sprintf("Element '%s' is not atomic in one or both lists.", n))
    }
    if (!identical(elem_a, elem_b)) {
      stop(sprintf(
        "Element '%s' differs between lists.\nx: %s\ny: %s",
        n,
        paste0(capture.output(print(elem_a)), collapse = "\n"),
        paste0(capture.output(print(elem_b)), collapse = "\n")
      ))
    }
  }
  return(list(
    unique_to_x = unique_a,
    unique_to_y = unique_b
  ))
}
