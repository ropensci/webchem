#' Replicate ChEMBL resource using a local ChEMBL database
#'
#' @param query character; activity ID to retrieve
#' @param resource character; ChEMBL resource to query
#' @param verbose logical; print verbose messages to the console?
#' @param version character; version of the ChEMBL database
#' @examples
#' \dontrun{
#' chembl_query_offline(query = "CHEMBL1082", resource = "molecule")
#' }
#' @noRd
chembl_query_offline <- function(
    query,
    resource = "molecule",
    verbose = getOption("verbose"),
    similarity = 70,
    version = "latest"
) {
  resource <- match.arg(resource, chembl_resources())
  if (!inherits(version, "chembl_version")) {
    version <- validate_chembl_version(version = version)
  }
  con <- connect_chembl(version = version)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  FUN <- paste0("chembl_offline_", resource)
  if (!exists(FUN)) {
    msg <- paste0(
      "Offline query for resource '", resource, "' is not implemented.")
    stop(msg)
  }
  if (resource == "similarity") {
    do.call(FUN, args = list(
      query = query,
      verbose = verbose,
      similarity = similarity,
      version = version,
      con = con
    ))
  } else {
    do.call(FUN, args = list(
      query = query,
      verbose = verbose,
      version = version,
      con = con
    ))
  }
}

#' activity resource
#' 
#' @examples
#' \dontrun{
#' chembl_query_offline(query = "31863", resource = "activity")
#' }
#' @noRd
chembl_offline_activity <- function(
  query,
  verbose = getOption("verbose"),
  version = "latest",
  con
  ){
  stop("Offline 'activity' queries are not yet implemented.")
}

#' assay resource
#'
#' @examples
#' \dontrun{
#' chembl_query_offline(query = "CHEMBL615117", resource = "assay")
#' }
#' @noRd
chembl_offline_assay <- function(
  query,
  verbose = getOption("verbose"),
  version = "latest",
  con
  ){
  stop("Offline 'assay' queries are not yet implemented.")
}

#' atc_class resource
#'
#' @examples
#' \dontrun{
#' chembl_query_offline(query = "A01AA01", resource = "atc_class")
#' }
#' @noRd
chembl_offline_atc_class <- function(
  query,
  verbose = getOption("verbose"),
  version = "latest",
  con
  ){
  stop("Offline 'atc_class' queries are not yet implemented.")
}

#' binding_site resource
#'
#' @examples
#' \dontrun{
#' chembl_query_offline(query = 2, resource = "binding_site")
#' }
#' @noRd
chembl_offline_binding_site <- function(
  query,
  verbose = getOption("verbose"),
  version = "latest",
  con
  ){
  stop("Offline 'binding_site' queries are not yet implemented.")
}

#' biotherapeutic resource
#'
#' @examples
#' \dontrun{
#' chembl_query_offline(query = "CHEMBL448105", resource = "biotherapeutic")
#' }
#' @noRd
chembl_offline_biotherapeutic <- function(
    query,
    verbose = getOption("verbose"),
    version = "latest",
    con
  ){
  stop("Offline 'biotherapeutic' queries are not yet implemented.")
}

#' cell_line resource using a local ChEMBL database
#'
#' @examples
#' \dontrun{
#' chembl_query_offline(query = "CHEMBL3307241", resource = "cell_line")
#' }
#' @noRd
chembl_offline_cell_line <- function(
  query,
  verbose = getOption("verbose"),
  version = "latest",
  con
  ){
  stop("Offline 'cell_line' queries are not yet implemented.")
}

#' chembl_id_lookup resource
#'
#' @examples
#' \dontrun{
#' chembl_query_offline(query = "CHEMBL1", resource = "chembl_id_lookup")
#' }
#' @noRd
chembl_offline_chembl_id_lookup <- function(
  query,
  target,
  verbose = getOption("verbose"),
  version = "latest",
  con
  ){
  fetch_table(
    con = con,
    table = "chembl_id_lookup",
    id_col = "chembl_id",
    ids = query,
  )
}

#' compound_record resource
#'
#' @examples
#' \dontrun{
#' chembl_query_offline(query = "1", resource = "compound_record")
#' }
#' @noRd
chembl_offline_compound_record <- function(
  query,
  verbose = getOption("verbose"),
  version = "latest",
  con
  ){
  stop("Offline 'compound_record' queries are not yet implemented.")
}

#' compound_structural_alert resource
#'
#' @examples
#' \dontrun{
#' chembl_query_offline(query = "CHEMBL266429", resource = "compound_structural_alert")
#' }
#' @noRd
chembl_offline_compound_structural_alert <- function(
  query,
  verbose = getOption("verbose"),
  version = "latest",
  con
  ){
  stop("Offline 'compound_structural_alert' queries are not yet implemented.")
}

#' document resource
#'
#' @examples
#' \dontrun{
#' chembl_query_offline(query = "CHEMBL1158643", resource = "document")
#' }
#' @noRd
chembl_offline_document <- function(
  query,
  verbose = getOption("verbose"),
  version = "latest",
  con
  ){
  stop("Offline 'document' queries are not yet implemented.")
}

#' document_similarity resource
#'
#' @examples
#' \dontrun{
#' chembl_query_offline(query = "CHEMBL1148466", resource = "document_similarity")
#' }
#' @noRd
chembl_offline_document_similarity <- function(
  query,
  verbose = getOption("verbose"),
  version = "latest",
  con
  ){
  stop("Offline 'document_similarity' queries are not yet implemented.")
}

#' drug resource
#'
#' @examples
#' \dontrun{
#' chembl_query_offline(query = "CHEMBL2", resource = "drug")
#' }
#' @noRd
chembl_offline_drug <- function(
  query,
  verbose = getOption("verbose"),
  version = "latest",
  con
  ){
  stop("Offline 'drug' queries are not yet implemented.")
}

#' drug_indication resource
#'
#' @examples
#' \dontrun{
#' chembl_query_offline(query = "22606", resource = "drug_indication")
#' }
#' @noRd
chembl_offline_drug_indication <- function(
  query,
  verbose = getOption("verbose"),
  version = "latest",
  con
  ){
  stop("Offline 'drug_indication' queries are not yet implemented.")
}

#' drug_warning resource using a local ChEMBL database
#'
#' @examples
#' \dontrun{
#' chembl_query_offline(query = "1", resource = "drug_warning")
#' }
#' @noRd
chembl_offline_drug_warning <- function(
  query,
  verbose = getOption("verbose"),
  version = "latest",
  con
  ){
  stop("Offline 'drug_warning' queries are not yet implemented.")
}

#' go_slim resource
#'
#' @examples
#' \dontrun{
#' chembl_query_offline(query = "GO:0000003", resource = "go_slim")
#' }
#' @noRd
chembl_offline_go_slim <- function(
  query,
  verbose = getOption("verbose"),
  version = "latest",
  con
  ){
  stop("Offline 'go_slim' queries are not yet implemented.")
}

#' mechanism resource
#'
#' @examples
#' \dontrun{
#' chembl_query_offline(query = "13", resource = "mechanism")
#' }
#' @noRd
chembl_offline_mechanism <- function(
  query,
  verbose = getOption("verbose"),
  version = "latest",
  con
  ){
  stop("Offline 'mechanism' queries are not yet implemented.")
}

#' metabolism resource
#'
#' @examples
#' \dontrun{
#' chembl_query_offline(query = "119", resource = "metabolism")
#' }
#' @noRd
chembl_offline_metabolism <- function(
  query,
  verbose = getOption("verbose"),
  version = "latest",
  con
  ){
  stop("Offline 'metabolism' queries are not yet implemented.")
}

#' molecule resource
#'
#' @importFrom rlang .data
#' @examples
#' \dontrun{
#' chembl_query_offline(query = "CHEMBL1082", resource = "molecule")
#' }
#' @noRd
chembl_offline_molecule <- function(
    query,
    verbose = getOption("verbose"),
    version = "latest", 
    con
  ){
  chembl_validate_id_offline(
      query = query,
      target = "COMPOUND",
      verbose = verbose,
      con = con
  )
  ids <- fetch_table(
    con = con,
    table = "molecule_dictionary",
    id_col = "chembl_id",
    ids = query,
    select_cols = c("chembl_id", "molregno")
  )
  molecule_dictionary <- fetch_table(
    con = con,
    table = "molecule_dictionary",
    id_col = "chembl_id",
    ids = query,
    select_cols = c(
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
    )
  )
  atc_classifications <- fetch_table(
    con = con,
    table = "molecule_atc_classification",
    id_col = "molregno",
    ids = ids$molregno,
    select_cols = c("molregno", "level5")
  )
  extract_variable <- function(df, var) {
    if (nrow(df) == 0) {
      return(NULL)
    } else {
      return(df[[var]])
    }
  }
  biotherapeutics <- fetch_table(
    con = con,
    table = "biotherapeutics",
    id_col = "molregno",
    ids = ids$molregno,
    select_cols = NULL
  )
  # description, helm notation!
  molecule_hierarchy_raw <- fetch_table(
    con = con,
    table = "molecule_hierarchy",
    id_col = "molregno",
    ids = ids$molregno,
    select_cols = NULL
  )
  molecule_properties <- fetch_table(
    con = con,
    table = "compound_properties",
    id_col = "molregno",
    ids = ids$molregno,
    select_cols = c(
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
    )
  )
  molecule_structures <- fetch_table(
    con = con,
    table = "compound_structures",
    id_col = "molregno",
    ids = ids$molregno,
    select_cols = c(
      "canonical_smiles",
      "molfile",
      "molregno",
      "standard_inchi",
      "standard_inchi_key"
    )
  )
  molecule_synonyms <- fetch_table(
    con = con,
    table = "molecule_synonyms",
    id_col = "molregno",
    ids = ids$molregno,
    select_cols = c("molregno", "synonyms","syn_type")
  )
  molecule_synonyms <- molecule_synonyms |>
    dplyr::rename("molecule_synonym" = "synonyms") |>
    dplyr::arrange(.data$molecule_synonym)

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
      dplyr::pull("level5")
    biotherapeutics2 <- biotherapeutics |>
      dplyr::filter(.data$molregno == query_molregno) |>
      dplyr::select(-"molregno")


    molecule_hierarchy_raw2 <- molecule_hierarchy_raw |>
      dplyr::filter(.data$molregno == query_molregno) |>
      dplyr::select(-"molregno")
    molecule_hierarchy <- tibble::tibble(
      "active_chembl_id" = query[i],
      "molecule_chembl_id" = query[i],
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

#'molecule_form resource
#'
#' @examples
#' \dontrun{
#' chembl_query_offline(query = "CHEMBL6329", resource = "molecule_form")
#' }
#' @noRd
chembl_offline_molecule_form <- function(
  query,
  verbose = getOption("verbose"),
  version = "latest",
  con
  ){
  stop("Offline 'molecule_form' queries are not yet implemented.")
}

#' organism resource
#'
#' @examples
#' \dontrun{
#' chembl_query_offline(query = "1", resource = "organism")
#' }
#' @noRd
chembl_offline_organism <- function(
  query,
  verbose = getOption("verbose"),
  version = "latest",
  con
  ){
  stop("Offline 'organism' queries are not yet implemented.")
}

#' protein_classification resource
#'
#' @examples
#' \dontrun{
#' chembl_query_offline(query = "1", resource = "protein_classification")
#' }
#' @noRd
chembl_offline_protein_classification <- function(
  query,
  verbose = getOption("verbose"),
  version = "latest",
  con
  ){
  stop("Offline 'protein_classification' queries are not yet implemented.")
}

#' source resource
#'
#' @examples
#' \dontrun{
#' chembl_query_offline(query = "1", resource = "source")
#' }
#' @noRd
chembl_offline_source <- function(
  query,
  verbose = getOption("verbose"),
  version = "latest",
  con
  ){
  stop("Offline 'source' queries are not yet implemented.")
}

#' similarity resource
#'
#' @examples
#' \dontrun{
#' chembl_query_offline(query = "CC(=O)Oc1ccccc1C(=O)O", resource = "similarity")
#' }
#' @noRd
chembl_offline_similarity <- function(
  query,
  verbose = getOption("verbose"),
  version = "latest",
  con
  ){
  stop("Offline 'similarity' queries are not yet implemented.")
}

#' substructure resource
#'
#' @examples
#' \dontrun{
#' chembl_query_offline(query = "CN(CCCN)c1cccc2ccccc12", resource = "substructure")
#' }
#' @noRd
chembl_offline_substructure <- function(
  query,
  verbose = getOption("verbose"),
  version = "latest",
  con
  ){
  stop("Offline 'substructure' queries are not yet implemented.")
}

#' target resource
#'
#' @examples
#' \dontrun{
#' chembl_query_offline(query = "CHEMBL2074", resource = "target")
#' }
#' @noRd
chembl_offline_target <- function(
  query,
  verbose = getOption("verbose"),
  version = "latest",
  con
  ){
  stop("Offline 'target' queries are not yet implemented.")
}

#' target_component resource
#'
#' @examples
#' \dontrun{
#' chembl_query_offline(query = "1", resource = "target_component")
#' }
#' @noRd
chembl_offline_target_component <- function(
  query,
  verbose = getOption("verbose"),
  version = "latest",
  con
  ){
  stop("Offline 'target_component' queries are not yet implemented.")
}

#' target_relation resource
#'
#' @examples
#' \dontrun{
#' chembl_query_offline(query = "CHEMBL2251", resource = "target_relation")
#' }
#' @noRd
chembl_offline_target_relation <- function(
  query,
  verbose = getOption("verbose"),
  version = "latest",
  con
  ){
  stop("Offline 'target_relation' queries are not yet implemented.")
}

#' tissue resource
#'
#' @examples
#' \dontrun{
#' chembl_query_offline(query = "CHEMBL3988026", resource = "tissue")
#' }
#' @noRd
chembl_offline_tissue <- function(
  query,
  verbose = getOption("verbose"),
  version = "latest",
  con
  ){
  stop("Offline 'tissue' queries are not yet implemented.")
}

#' xref_source resource
#'
#' @examples
#' \dontrun{
#' chembl_query_offline(query = "AlphaFoldDB", resource = "xref_source")
#' }
#' @noRd
chembl_offline_xref_source <- function(
  query,
  verbose = getOption("verbose"),
  version = "latest",
  con
  ){
  stop("Offline 'xref_source' queries are not yet implemented.")
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

  out <- compare_service_lists(ws = ws_result, offline = offline_result)

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
#' @noRd
#' @examples
#' \dontrun{
#' ws <- chembl_query("CHEMBL1082", resource = "molecule")
#' off <- chembl_query("CHEMBL1082", resource = "molecule", mode = "offline")
#' compare_service_lists(ws$CHEMBL1082, off$CHEMBL1082)
#' }
compare_service_lists <- function(ws, offline) {
  ws_na <- length(ws) == 1 && is.na(ws)
  offline_na <- length(offline) == 1 && is.na(offline)
  if (ws_na & offline_na) {
    return(list(
      status = "both failed",
      ws_unique_element = character(),
      offline_unique_element = character()
    ))
  }
  if (ws_na) stop("Webservice query failed.")
  if (offline_na) stop("Offline query failed.")
  if (!inherits(ws, "list")) stop("Webservice result is not a list.")
  if (!inherits(offline, "list")) stop("Offline result is not a list.")
  ws_unique_element <- setdiff(names(ws), names(offline))
  offline_unique_element <- setdiff(names(offline), names(ws))
  common_names <- intersect(names(ws), names(offline))
  ws_common_order <- names(ws)[names(ws) %in% common_names]
  offline_common_order <- names(offline)[names(offline) %in% common_names]
  if (!identical(ws_common_order, offline_common_order)) {
    stop(sprintf(
      "Names of common elements are not in the same order:\n  ws: %s\n  offline: %s",
      paste(ws_common_order, collapse = ", "),
      paste(offline_common_order, collapse = ", ")
    ))
  }
  for (n in common_names) {
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
          paste0(
            "Atomic element '%s' differs between webservice and offline.\n",
            "Webservice value: %s\nOffline value: %s"
          ),
          n,
          paste0(utils::capture.output(print(ws_elem)), collapse = "\n"),
          paste0(utils::capture.output(print(off_elem)), collapse = "\n")
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
  }
  return(list(
    status = "OK",
    ws_extra = ws_unique_element,
    offline_extra = offline_unique_element
  ))
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
        paste0(utils::capture.output(print(elem_a)), collapse = "\n"),
        paste0(utils::capture.output(print(elem_b)), collapse = "\n")
      ))
    }
  }
  return(list(
    unique_to_x = unique_a,
    unique_to_y = unique_b
  ))
}

fetch_table <- function(
    con,
    table,
    id_col = "molregno",
    ids,
    select_cols = NULL
  ) {
  out <- tbl(con, table) |> dplyr::filter(.data[[id_col]] %in% ids)
  if (!is.null(select_cols)) {
    out <- out |> dplyr::select(dplyr::all_of(select_cols))
  }
  out |> dplyr::collect()
}

chembl_validate_id_offline <- function(
   query,
   target,
   verbose = getOption("verbose"),
   con
) {
  entity_type <- fetch_table(
    con = con,
    table = "chembl_id_lookup",
    id_col = "chembl_id",
    ids = query,
    select_cols = c("chembl_id", "entity_type")
  )
  index <- which(!query %in% entity_type$chembl_id)
  if (length(index) > 0) {
    missing_ids <- paste(query[index], collapse = ", ")
    msg <- paste0("The following ChEMBL IDs were not found: ", missing_ids)
    stop(msg)
  }
  for (i in 1:nrow(entity_type)) {
    if (entity_type$entity_type[i] != target) {
      msg <- paste0(
        entity_type$chembl_id[i], " is not a ", target, ". It is a ",
        entity_type$entity_type[i], ".")
      stop(msg)
    }
  }
}
