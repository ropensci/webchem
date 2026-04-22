#' Replicate ChEMBL resource using a local ChEMBL database
#'
#' @param query character; activity ID to retrieve
#' @param resource character; ChEMBL resource to query
#' @param verbose logical; print verbose messages to the console?
#' @param version character; version of the ChEMBL database
#' @param output character; either "raw" or "tidy"
#' @examples
#' \dontrun{
#' chembl_query_offline(query = "CHEMBL1082", resource = "molecule")
#' }
#' @noRd
chembl_query_offline <- function(
    query,
    resource = "molecule",
    output = "raw",
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
      output = output,
      con = con
    ))
  } else {
    do.call(FUN, args = list(
      query = query,
      verbose = verbose,
      version = version,
      output = output,
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
  output = "raw",
  con
  ){
  stop("Offline 'activity' queries are not yet implemented.")
  # fetch relevant tables from database

  # loop through the queries and assemble raw output
  out <- unname(lapply(query, function(x) {
    # implementation here
  }))
  names(out) <- query
  if (output == "tidy") {
    stop("Tidy output for 'activity' is not yet implemented.")
  }
  return(out)
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
  output = "raw",
  con
  ){
  chembl_validate_id_offline(
    query = query,
    target = "ASSAY",
    verbose = verbose,
    con = con
  )
  assays <- fetch_table(
    con = con,
    table = "assays",
    id_col = "chembl_id",
    ids = query,
    select_cols = c(
      "assay_id",                     # link column (assay_parameters, docs, cell_dictionary, tissue_dictionary, target_dictionary)
      "aidx",                         # output column
      "assay_category",               # output column
      "assay_cell_type",              # output column
      "assay_group",                  # output column
      "assay_organism",               # output column
      "assay_strain",                 # output column
      "assay_subcellular_fraction",   # output column
      "assay_tax_id",                 # output column
      "assay_test_type",              # output column
      "assay_tissue",                 # output column
      "assay_type",                   # output column
      "bao_format",                   # output column
      "cell_id",                      # link column (cell_dictionary)
      "chembl_id",                    # query column, output column (rename to assay_chembl_id)
      "confidence_score",             # output column
      "description",                  # output column
      "doc_id",                       # link column (docs)
      "relationship_type",            # output column
      "src_assay_id",                 # output column
      "src_id",                       # output column
      "tid",                          # link column (target_dictionary)
      "tissue_id",                    # link column (tissue_dictionary)
      "variant_id"                    # link column (variant_sequences)
    )
  )

  # Fetch assay class map to link assays to classifications
  assay_class_map <- fetch_table(
    con = con,
    table = "assay_class_map",
    id_col = "assay_id",
    ids = unique(assays$assay_id),
    select_cols = c(
      "assay_id",        # link column (assays)
      "assay_class_id"   # link column (assay_classification)
    )
  )

  # Fetch assay classifications
  assay_classifications <- fetch_table(
    con = con,
    table = "assay_classification",
    id_col = "assay_class_id",
    ids = unique(assay_class_map$assay_class_id),
    select_cols = c(
      "assay_class_id",  # link column (assay_class_map)
      "l1",              # output column
      "l2",              # output column
      "l3",              # output column
      "class_type",      # output column
      "source"           # output column
    )
  )

  # Fetch assay parameters
  assay_parameters <- fetch_table(
    con = con,
    table = "assay_parameters",
    id_col = "assay_id",
    ids = unique(assays$assay_id),
    select_cols = c(
      "assay_id",           # link column (assays)
      "comments",           # output column
      "relation",           # output column
      "standard_type",      # output column
      "standard_units",     # output column
      "standard_value",     # output column
      "text_value",         # output column
      "type"                # output column
      # NOTE: 'active' field does not exist in database
    )
  )

  # Fetch assay type descriptions
  assay_type <- fetch_table(
    con = con,
    table = "assay_type",
    id_col = "assay_type",
    ids = unique(assays$assay_type),
    select_cols = c(
      "assay_type",         # link column (assays)
      "assay_desc"          # output column (rename to assay_type_description)
    )
  )

  # Fetch BAO format labels
  bioassay_ontology <- fetch_table(
    con = con,
    table = "bioassay_ontology",
    id_col = "bao_id",
    ids = unique(assays$bao_format),
    select_cols = c(
      "bao_id",             # link column (assays)
      "label"               # output column (rename to bao_label)
    )
  )

  # Fetch cell ChEMBL IDs
  cell_dictionary <- fetch_table(
    con = con,
    table = "cell_dictionary",
    id_col = "cell_id",
    ids = unique(assays$cell_id[!is.na(assays$cell_id)]),
    select_cols = c(
      "cell_id",            # link column (assays)
      "chembl_id"           # output column (rename to cell_chembl_id)
    )
  )

  # Fetch confidence descriptions
  confidence_score_lookup <- fetch_table(
    con = con,
    table = "confidence_score_lookup",
    id_col = "confidence_score",
    ids = unique(assays$confidence_score),
    select_cols = c(
      "confidence_score",   # link column (assays)
      "description"         # output column (rename to confidence_description)
    )
  )

  # Fetch document ChEMBL IDs
  docs <- fetch_table(
    con = con,
    table = "docs",
    id_col = "doc_id",
    ids = unique(assays$doc_id),
    select_cols = c(
      "doc_id",             # link column (assays)
      "chembl_id"           # output column (rename to document_chembl_id)
    )
  )

  # Fetch relationship descriptions
  relationship_type <- fetch_table(
    con = con,
    table = "relationship_type",
    id_col = "relationship_type",
    ids = unique(assays$relationship_type),
    select_cols = c(
      "relationship_type",  # link column (assays)
      "relationship_desc"   # output column (rename to relationship_description)
    )
  )

  # Fetch target ChEMBL IDs
  target_dictionary <- fetch_table(
    con = con,
    table = "target_dictionary",
    id_col = "tid",
    ids = unique(assays$tid),
    select_cols = c(
      "tid",                # link column (assays)
      "chembl_id"           # output column (rename to target_chembl_id)
    )
  )

  # Fetch tissue ChEMBL IDs
  tissue_dictionary <- fetch_table(
    con = con,
    table = "tissue_dictionary",
    id_col = "tissue_id",
    ids = unique(assays$tissue_id[!is.na(assays$tissue_id)]),
    select_cols = c(
      "tissue_id",          # link column (assays)
      "chembl_id"           # output column (rename to tissue_chembl_id)
    )
  )

  # Fetch variant sequences
  variant_sequences <- fetch_table(
    con = con,
    table = "variant_sequences",
    id_col = "variant_id",
    ids = unique(assays$variant_id[!is.na(assays$variant_id)]),
    select_cols = c(
      "variant_id",         # link column (assays)
      "mutation",           # output column
      "organism",           # output column
      "sequence",           # output column (rename to variant_sequence)
      "tax_id",             # output column
      "version"             # output column
    )
  )

  # Build output for each query
  out <- lapply(query, function(q) {
    # Get assay data
    assay <- assays |> dplyr::filter(.data$chembl_id == q)
    if (nrow(assay) == 0) return(NA)

    # Get assay classifications for this assay
    class_map <- assay_class_map |> dplyr::filter(.data$assay_id == assay$assay_id)
    assay_class_list <- if (nrow(class_map) > 0) {
      lapply(seq_len(nrow(class_map)), function(i) {
        class_data <- assay_classifications |>
          dplyr::filter(.data$assay_class_id == class_map$assay_class_id[i])
        if (nrow(class_data) > 0) {
          list(
            assay_class_id = class_data$assay_class_id,
            class_type = class_data$class_type,
            l1 = class_data$l1,
            l2 = class_data$l2,
            l3 = class_data$l3,
            source = class_data$source
          )
        } else {
          NA_character_
        }
      })
    } else {
      NA_character_
    }

    # Get assay parameters
    assay_params <- assay_parameters |>
      dplyr::filter(.data$assay_id == assay$assay_id)
    assay_params_list <- if (nrow(assay_params) > 0) {
      lapply(seq_len(nrow(assay_params)), function(i) {
        list(
          comments = assay_params$comments[i],
          relation = assay_params$relation[i],
          standard_type = assay_params$standard_type[i],
          standard_units = assay_params$standard_units[i],
          standard_value = ifelse(
            is.na(assay_params$standard_value[i]),
            NA_character_,
            sprintf("%.1f", assay_params$standard_value[i])
          ),
          text_value = assay_params$text_value[i],
          type = assay_params$type[i]
        )
      })
    } else {
      NA_character_
    }

    # Get variant sequence
    variant_seq <- if (!is.na(assay$variant_id)) {
      vs <- variant_sequences |>
        dplyr::filter(.data$variant_id == assay$variant_id)
      if (nrow(vs) > 0) vs$sequence else NA_character_
    } else {
      NA_character_
    }

    assay_type_df <- assay_type |>
      dplyr::filter(.data$assay_type == assay$assay_type)

    bao_label_df <- bioassay_ontology |>
      dplyr::filter(.data$bao_id == assay$bao_format)

    cell_df <- cell_dictionary |> dplyr::filter(.data$cell_id == assay$cell_id)

    confidence_score_df <- confidence_score_lookup |>
      dplyr::filter(.data$confidence_score == assay$confidence_score)

    docs_df <- docs |> dplyr::filter(.data$doc_id == assay$doc_id)

    relationship_type_df <- relationship_type |>
      dplyr::filter(.data$relationship_type == assay$relationship_type)

    target_dictionary_df <- target_dictionary |>
      dplyr::filter(.data$tid == assay$tid)

    tissue_dictionary_df <- tissue_dictionary |>
      dplyr::filter(.data$tissue_id == assay$tissue_id)

    # Build result
    result <- list(
      aidx = assay$aidx,
      assay_category = assay$assay_category,
      assay_cell_type = assay$assay_cell_type,
      assay_chembl_id = assay$chembl_id,
      assay_classifications = assay_class_list,
      assay_group = assay$assay_group,
      assay_organism = assay$assay_organism,
      assay_parameters = assay_params_list,
      assay_strain = assay$assay_strain,
      assay_subcellular_fraction = assay$assay_subcellular_fraction,
      assay_tax_id = assay$assay_tax_id,
      assay_test_type = assay$assay_test_type,
      assay_tissue = assay$assay_tissue,
      assay_type = assay$assay_type,
      assay_type_description = if (nrow(assay_type_df) > 0) {
        assay_type_df$assay_desc
      } else {
        NA_character_
      },
      bao_format = assay$bao_format,
      bao_label = if (nrow(bao_label_df) > 0) {
        bao_label_df$label
      } else {
        NA_character_
      },
      cell_chembl_id = if (!is.na(assay$cell_id) && nrow(cell_df) > 0) {
        cell_df$chembl_id
      } else {
        NA_character_
      },
      confidence_description = if (nrow(confidence_score_df) > 0) {
        confidence_score_df$description
      } else {
        NA_character_
      },
      confidence_score = assay$confidence_score,
      description = assay$description,
      document_chembl_id = docs_df$chembl_id,
      relationship_description = if (nrow(relationship_type_df) > 0) {
        relationship_type_df$relationship_desc
      } else {
        NA_character_
      },
      relationship_type = assay$relationship_type,
      src_assay_id = assay$src_assay_id,
      src_id = assay$src_id,
      target_chembl_id = target_dictionary_df$chembl_id,
      tissue_chembl_id = if (!is.na(assay$tissue_id) && nrow(tissue_dictionary_df) > 0) {
        tissue_dictionary_df$chembl_id
      } else {
        NA_character_
      },
      variant_sequence = variant_seq
    )
    return(result)
  })

  names(out) <- query
  class(out) <- c("chembl_assay_raw", class(out))

  if (output == "tidy") {
    stop("Tidy output for 'assay' is not yet implemented.")
  }
  return(out)
}

#' atc_class resource
#'
#' @examples
#' \dontrun{
#' chembl_query_offline(query = "A01AA01", resource = "atc_class")
#' chembl_query_offline(query = "A01AA", resource = "atc_class")
#' chembl_query_offline(query = "A01A", resource = "atc_class")
#' chembl_query_offline(query = "A01", resource = "atc_class")
#' chembl_query_offline(query = "A", resource = "atc_class")
#' }
#' @noRd
chembl_offline_atc_class <- function(
  query,
  verbose = getOption("verbose"),
  version = "latest",
  output = "raw",
  con
  ){
  # determine ATC level field for a given code length
  len2level <- function(x) {
    switch(as.character(x),
      "1" = "level1",
      "3" = "level2",
      "4" = "level3",
      "5" = "level4",
      "7" = "level5",
      NA
    )
  }
  # validate query lengths
  q_lengths <- nchar(query)
  levels <- sapply(q_lengths, len2level)
  invalid_idx <- which(is.na(levels))
  if (length(invalid_idx) > 0) {
    msg <- sprintf(
      "Invalid ATC code(s): %s. ATC codes must have lengths 1,3,4,5 or 7.",
      paste(unique(query[invalid_idx]), collapse = ", ")
    )
    stop(msg)
  }
  # fetch by groups of same-length queries to use the correct id_col
  fetched <- lapply(seq_along(q_lengths), function(i) {
    fetch_table(
      con = con,
      table = "atc_classification",
      id_col = len2level(q_lengths[i]),
      ids = query[i],
      select_cols = c(
        "level1",
        "level1_description",
        "level2",
        "level2_description",
        "level3",
        "level3_description",
        "level4",
        "level4_description",
        "level5",
        "who_name"
      )
    )
  })
  names(fetched) <- query
  if (output == "raw") {
    out <- lapply(query, function(q) {
      df <- fetched[[q]]
      if (nrow(df) == 0) return(NA)
      lapply(seq_len(nrow(df)), function(i) {
        row_list <- as.list(df[i, , drop = FALSE])
        row_list[sort(names(row_list))]
      })
    })
    names(out) <- query
    class(out) <- c("chembl_atc_class_raw", class(out))
  } else {
    # Ensure all queries have at least one row (with NAs if no results)
    for (i in seq_along(fetched)) {
      if (nrow(fetched[[i]]) == 0) {
        # Create a single row with all NAs
        fetched[[i]] <- fetched[[i]][1, , drop = FALSE]
        fetched[[i]][1, ] <- NA
      }
    }
    # Bind rows and add query column
    out <- dplyr::bind_rows(fetched, .id = "query")
    # Reorder columns to put query first
    out <- out |> dplyr::relocate("query")
    class(out) <- c("chembl_atc_class_tidy", class(out))
  }
  return(out)
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
  output = "raw",
  con
  ){
  # Fetch binding site data
  binding_sites <- fetch_table(
    con = con,
    table = "binding_sites",
    id_col = "site_id",
    ids = as.integer(query),
    select_cols = c("site_id", "site_name")
  )
  # Fetch site components
  site_components <- fetch_table(
    con = con,
    table = "site_components",
    id_col = "site_id",
    ids = as.integer(query),
    select_cols = c("sitecomp_id", "site_id", "component_id", "domain_id")
  )
  # Fetch domain info
  domains <- fetch_table(
    con = con,
    table = "domains",
    id_col = "domain_id",
    ids = unique(site_components$domain_id),
    select_cols = c(
      "domain_description",
      "domain_id",
      "domain_name",
      "domain_type",
      "source_domain_id"
    )
  )
  # Build results for each query
  out <- lapply(query, function(q) {
    q_int <- as.integer(q)
    # Check if binding site exists
    bs <- binding_sites |> dplyr::filter(.data$site_id == q_int)
    if (nrow(bs) == 0) return(NA)
    # Get site components for this query
    sc <- site_components |> dplyr::filter(.data$site_id == q_int)
    site_components_list <- list()
    for (i in seq_len(nrow(sc))) {
      sc_i <- sc[i, ]
      domain_info <- domains |> dplyr::filter(.data$domain_id == sc_i$domain_id)
      component_entry <- list(
        "component_id" = sc_i$component_id,
        "domain" = if (nrow(domain_info) > 0) {
          as.list(domain_info[1, , drop = FALSE])
        } else {
          NULL
        },
        "sitecomp_id" = sc_i$sitecomp_id
      )
      site_components_list[[i]] <- component_entry
    }
    out <- list(
      site_components = site_components_list,
      site_id = bs$site_id,
      site_name = bs$site_name
    )
    return(out)
  })
  names(out) <- query
  class(out) <- c("chembl_binding_site_raw", class(out))
  if (output == "tidy") {
    stop("Tidy output for 'binding_site' is not yet implemented.")
  }
  return(out)
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
    output = "raw",
    con
  ){
  chembl_validate_id_offline(
    query = query,
    target = "COMPOUND",
    verbose = verbose,
    con = con
  )
  # Fetch molregno for queries
  ids <- fetch_table(
    con = con,
    table = "molecule_dictionary",
    id_col = "chembl_id",
    ids = query,
    select_cols = c(
      "chembl_id",  # output column
      "molregno"    # link column (to biotherapeutics, biotherapeutic_components)
    )
  )

  # Fetch biotherapeutic data
  biotherapeutics <- fetch_table(
    con = con,
    table = "biotherapeutics",
    id_col = "molregno",
    ids = ids$molregno,
    select_cols = c(
      "molregno",        # link column (molecule_dictionary)
      "description",     # output column
      "helm_notation"    # output column
    )
  )

  # Fetch biotherapeutic components
  biotherapeutic_components <- fetch_table(
    con = con,
    table = "biotherapeutic_components",
    id_col = "molregno",
    ids = ids$molregno,
    select_cols = c(
      "molregno",        # link column (biotherapeutics)
      "component_id"     # link column (bio_component_sequences)
    )
  )

  # Fetch biocomponent sequences
  bio_component_sequences <- fetch_table(
    con = con,
    table = "bio_component_sequences",
    id_col = "component_id",
    ids = biotherapeutic_components$component_id,
    select_cols = c(
      "component_id",       # output column
      "component_type",     # output column
      "description",        # output column
      "organism",           # output column
      "sequence",           # output column
      "tax_id"              # output column
    )
  )
  # Build output for each query
  out <- lapply(query, function(q) {
    # Get molregno for this query
    q_molregno <- ids$molregno[ids$chembl_id == q]

    # Check if biotherapeutic data exists
    bio_data <- biotherapeutics |> dplyr::filter(.data$molregno == q_molregno)
    if (nrow(bio_data) == 0) return(NA)

    # Get biotherapeutic components for this molecule
    components <- biotherapeutic_components |>
      dplyr::filter(.data$molregno == q_molregno)

    biocomponents_list <- list()
    if (nrow(components) > 0) {
      for (i in seq_len(nrow(components))) {
        # Filter bio_component_sequences for this component
        comp <- bio_component_sequences |>
          dplyr::filter(.data$component_id == components$component_id[i])
        biocomponents_list[[i]] <- list(
          component_id = comp$component_id,
          component_type = comp$component_type,
          description = comp$description,
          organism = comp$organism,
          sequence = comp$sequence,
          tax_id = comp$tax_id
        )
      }
    }

    # Build final output structure
    result <- list(
      biocomponents = biocomponents_list,
      description = bio_data$description,
      helm_notation = bio_data$helm_notation,
      molecule_chembl_id = q
    )

    # Sort by names to match web service
    result[sort(names(result))]
  })

  names(out) <- query
  class(out) <- c("chembl_biotherapeutic_raw", class(out))

  if (output == "tidy") {
    stop("Tidy output for 'biotherapeutic' is not yet implemented.")
  }

  return(out)
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
  output = "raw",
  con
  ){
  chembl_validate_id_offline(
    query = query,
    target = "CELL",
    verbose = verbose,
    con = con
  )
  cell_line_data <- fetch_table(
    con = con,
    table = "cell_dictionary",
    id_col = "chembl_id",
    ids = query,
    select_cols = c(
      "cell_id",
      "cell_description",
      "cell_name",
      "cell_source_organism",
      "cell_source_tax_id",
      "cell_source_tissue",
      "cellosaurus_id",
      "chembl_id",
      "cl_lincs_id",
      "clo_id",
      "efo_id"
    )
  )
  out <- cell_line_data |>
    dplyr::rename("cell_chembl_id" = "chembl_id") |>
    dplyr::arrange(.data$cell_chembl_id)
  if (output == "raw") {
    out <- chembl_tidy2raw(query = query, df = out, resource = "cell_line")
  } else {
    class(out) <- c("chembl_cell_line_tidy", class(out))
  }
  return(out)
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
  verbose = getOption("verbose"),
  version = "latest",
  output = "raw",
  con
  ){
  out <- fetch_table(
    con = con,
    table = "chembl_id_lookup",
    id_col = "chembl_id",
    ids = query,
  )
  if (output == "raw") {
    out <- chembl_tidy2raw(query = query, df = out, resource = "chembl_id_lookup")
  } else {
    class(out) <- c("chembl_chembl_id_lookup_tidy", class(out))
  }
  return(out)
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
  output = "raw",
  con
  ){
  # Fetch compound records
  compound_records <- fetch_table(
    con = con,
    table = "compound_records",
    id_col = "record_id",
    ids = as.integer(query),
    select_cols = c(
      "record_id",      # query column
      "compound_key",   # output column
      "compound_name",  # output column
      "molregno",       # link column (molecule_dictionary)
      "doc_id",         # link column (docs)
      "src_id"          # link column (source)
    )
  )

  # Fetch molecule dictionary to get molecule_chembl_id
  molecule_ids <- fetch_table(
    con = con,
    table = "molecule_dictionary",
    id_col = "molregno",
    ids = unique(compound_records$molregno),
    select_cols = c(
      "molregno",   # link column (compound_records)
      "chembl_id"   # output column
    )
  )

  # Fetch document info to get document_chembl_id
  document_ids <- fetch_table(
    con = con,
    table = "docs",
    id_col = "doc_id",
    ids = unique(compound_records$doc_id),
    select_cols = c(
      "doc_id",     # link column (compound_records)
      "chembl_id"   # output column
    )
  )

  # Build output for each query
  out <- lapply(query, function(q) {
    q_int <- as.integer(q)

    # Get compound record
    cr <- compound_records |> dplyr::filter(.data$record_id == q_int)
    if (nrow(cr) == 0) return(NA)

    # Get molecule_chembl_id
    mol_chembl <- molecule_ids |>
      dplyr::filter(.data$molregno == cr$molregno) |>
      dplyr::pull(.data$chembl_id)

    # Get document_chembl_id
    doc_chembl <- document_ids |>
      dplyr::filter(.data$doc_id == cr$doc_id) |>
      dplyr::pull(.data$chembl_id)

    # Construct output matching webservice format
    result <- list(
      compound_key = cr$compound_key,
      compound_name = cr$compound_name,
      document_chembl_id = doc_chembl,
      molecule_chembl_id = mol_chembl,
      record_id = cr$record_id,
      src_id = cr$src_id
    )

    return(result)
  })

  names(out) <- query
  class(out) <- c("chembl_compound_record_raw", class(out))

  if (output == "tidy") {
    stop("Tidy output for 'compound_record' is not yet implemented.")
  }

  return(out)
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
  output = "raw",
  con
  ){
  stop("Offline 'compound_structural_alert' queries are not yet implemented.")
  chembl_validate_id_offline(
    query = query,
    target = "COMPOUND",
    verbose = verbose,
    con = con
  )
  # fetch relevant tables from database

  # loop through the queries and assemble raw output
  out <- unname(lapply(query, function(x) {
    # implementation here
  }))
  names(out) <- query
  if (output == "tidy") {
    stop("Tidy output for 'compound_structural_alert' is not yet implemented.")
  }
  return(out)
}

#' document resource
#'
#' @note This function does not return exactly the same output as the ChEMBL
#' webservice, because I couldn't find all the necessary data in the offline
#' database schema. The missing fields are: doi_chembl, journal_full_title.
#' @examples
#' \dontrun{
#' chembl_query_offline(query = "CHEMBL1158643", resource = "document")
#' }
#' @noRd
chembl_offline_document <- function(
  query,
  verbose = getOption("verbose"),
  version = "latest",
  output = "raw",
  con
  ){
  chembl_validate_id_offline(
    query = query,
    target = "DOCUMENT",
    verbose = verbose,
    con = con
  )

  # Fetch document data
  docs <- fetch_table(
    con = con,
    table = "docs",
    id_col = "chembl_id",
    ids = query,
    select_cols = c(
      "abstract",            # output column
      "authors",             # output column
      "chembl_release_id",   # output column (rename to chembl_release)
      "contact",             # output column
      "doc_type",            # output column
      "chembl_id",           # query, output column (rename to document_chembl_id)
      "doi",                 # output column
      "first_page",          # output column
      "issue",               # output column
      "journal",             # output column
      "last_page",           # output column
      "patent_id",           # output column
      "pubmed_id",           # output column
      "src_id",              # output column
      "title",               # output column
      "volume",              # output column
      "year"                 # output column
    )
  ) |>
    dplyr::rename(
      chembl_release = "chembl_release_id",
      document_chembl_id = "chembl_id"
    )

  # Fetch ChEMBL release info
  chembl_release_info <- fetch_table(
    con = con,
    table = "chembl_release",
    id_col = "chembl_release_id",
    ids = unique(docs$chembl_release)
  ) |>
    dplyr::mutate(
      creation_date = as.character(as.Date(.data$creation_date))
    )

  # Build output for each query
  out <- lapply(query, function(q) {
    # Get document data for this query
    doc <- docs |> dplyr::filter(.data$document_chembl_id == q)
    if (nrow(doc) == 0) return(NA)

    # Get ChEMBL release info
    chembl_release_info <- chembl_release_info |>
      dplyr::filter(.data$chembl_release_id == doc$chembl_release) |>
      dplyr::select(-"chembl_release_id")  |>
      as.list()

    # Construct output matching webservice format
    result <- list(
      abstract = if (is.na(doc$abstract)) "" else doc$abstract,
      authors = doc$authors,
      chembl_release = chembl_release_info,
      contact = doc$contact,
      doc_type = doc$doc_type,
      document_chembl_id = doc$document_chembl_id,
      doi = doc$doi,
      first_page = doc$first_page,
      issue = doc$issue,
      journal = doc$journal,
      last_page = doc$last_page,
      patent_id = doc$patent_id,
      pubmed_id = doc$pubmed_id,
      src_id = doc$src_id,
      title = doc$title,
      volume = doc$volume,
      year = doc$year
    )

    return(result)
  })

  names(out) <- query
  class(out) <- c("chembl_document_raw", class(out))

  if (output == "tidy") {
    stop("Tidy output for 'document' is not yet implemented.")
  }
  return(out)
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
  output = "raw",
  con
  ){
  stop("Offline 'document_similarity' queries are not yet implemented.")
  chembl_validate_id_offline(
    query = query,
    target = "DOCUMENT",
    verbose = verbose,
    con = con
  )
  # fetch relevant tables from database

  # loop through the queries and assemble raw output
  out <- unname(lapply(query, function(x) {
    # implementation here
  }))
  names(out) <- query
  if (output == "tidy") {
    stop("Tidy output for 'document_similarity' is not yet implemented.")
  }
  return(out)
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
  output = "raw",
  con
  ){
  stop("Offline 'drug' queries are not yet implemented.")
  chembl_validate_id_offline(
    query = query,
    target = "COMPOUND",
    verbose = verbose,
    con = con
  )
  # fetch relevant tables from database

  # loop through the queries and assemble raw output
  out <- unname(lapply(query, function(x) {
    # implementation here
  }))
  names(out) <- query
  if (output == "tidy") {
    stop("Tidy output for 'drug' is not yet implemented.")
  }
  return(out)
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
  output = "raw",
  con
  ){
  # Fetch drug indication data
  drug_indications <- fetch_table(
    con = con,
    table = "drug_indication",
    id_col = "drugind_id",
    ids = as.integer(query),
    select_cols = c(
      "drugind_id",           # query column
      "molregno",             # link column (molecule_dictionary)
      "efo_id",               # output column
      "efo_term",             # output column
      "max_phase_for_ind",    # output column
      "mesh_heading",         # output column
      "mesh_id"              # output column
    )
  )
  # Fetch indication references
  indication_refs <- fetch_table(
    con = con,
    table = "indication_refs",
    id_col = "drugind_id",
    ids = as.integer(query),
    select_cols = c(
      "drugind_id",         # link column (drug_indication)
      "ref_id",             # output column
      "ref_type",           # output column
      "ref_url"             # output column
    )
  )
  # Fetch molecule hierarchy to get parent_molecule_chembl_id
  molecule_hierarchy <- fetch_table(
    con = con,
    table = "molecule_hierarchy",
    id_col = "molregno",
    ids = unique(drug_indications$molregno),
    select_cols = c(
      "molregno",         # link column (drug_indication)
      "parent_molregno"   # link column (molecule_dictionary for parent)
    )
  )
  # Fetch molecule dictionary to get molecule_chembl_id and parent_molecule_chembl_id
  molecule_ids <- fetch_table(
    con = con,
    table = "molecule_dictionary",
    id_col = "molregno",
    ids = unique(c(
      drug_indications$molregno,
      molecule_hierarchy$parent_molregno
    )),
    select_cols = c(
      "molregno",   # link column (drug_indication)
      "chembl_id"   # output column
    )
  )
  # Build output for each query
  out <- lapply(query, function(q) {
    q_int <- as.integer(q)
    # Get drug indication data
    di <- drug_indications |> dplyr::filter(.data$drugind_id == q_int)
    if (nrow(di) == 0) return(NA)
    # Get molecule_chembl_id
    mol_chembl <- molecule_ids |>
      dplyr::filter(.data$molregno == di$molregno) |>
      dplyr::pull(.data$chembl_id)
    # Get CHEMBL ID for parent molecule
    parent_molregno <- molecule_hierarchy |>
      dplyr::filter(.data$molregno == di$molregno) |>
      dplyr::pull(.data$parent_molregno)
    parent_chembl <- molecule_ids |>
      dplyr::filter(.data$molregno == parent_molregno) |>
      dplyr::pull(.data$chembl_id)
    # Get indication refs for this drugind_id
    indication_refs_data <- indication_refs |>
      dplyr::filter(.data$drugind_id == q_int)
    # Build indication_refs list
    indication_refs_list <- list()
    if (nrow(indication_refs_data) > 0) {
      for (j in 1:nrow(indication_refs_data)) {
        indication_refs_list[[j]] <- list(
          ref_id = indication_refs_data$ref_id[j],
          ref_type = indication_refs_data$ref_type[j],
          ref_url = indication_refs_data$ref_url[j]
        )
      }
    }
    # Construct output matching webservice format
    out <- list(
      drugind_id = di$drugind_id,
      efo_id = di$efo_id,
      efo_term = di$efo_term,
      indication_refs = indication_refs_list,
      # note, this seems like a custom hack but the table has integer values
      # for max_phase_for_ind, so we convert to character with ".0" suffix,
      # for consistency with webservice output
      max_phase_for_ind = paste0(as.character(di$max_phase_for_ind), ".0"),
      mesh_heading = di$mesh_heading,
      mesh_id = di$mesh_id,
      molecule_chembl_id = mol_chembl,
      parent_molecule_chembl_id = parent_chembl
    )
    return(out)
  })
  names(out) <- query
  class(out) <- c("chembl_drug_indication_raw", class(out))

  if (output == "tidy") {
    stop("Tidy output for 'drug_indication' is not yet implemented.")
  }

  return(out)
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
  output = "raw",
  con
  ){
  # Fetch drug warning data
  drug_warnings <- fetch_table(
    con = con,
    table = "drug_warning",
    id_col = "warning_id",
    ids = as.integer(query),
    select_cols = c(
      "warning_id",                  # query column
      "molregno",                    # link column (molecule_dictionary)
      "efo_id",                      # output column
      "efo_id_for_warning_class",    # output column
      "efo_term",                    # output column
      "warning_class",               # output column
      "warning_country",             # output column
      "warning_description",         # output column
      "warning_type",                # output column
      "warning_year"                 # output column
    )
  )
  # Fetch molecule hierarchy to get parent_molecule_chembl_id
  molecule_hierarchy <- fetch_table(
    con = con,
    table = "molecule_hierarchy",
    id_col = "molregno",
    ids = unique(drug_warnings$molregno),
    select_cols = c(
      "molregno",         # link column (drug_warning)
      "parent_molregno"   # link column (molecule_dictionary for parent)
    )
  )
  # Fetch molecule dictionary to get molecule_chembl_id and parent_molecule_chembl_id
  molecule_ids <- fetch_table(
    con = con,
    table = "molecule_dictionary",
    id_col = "molregno",
    ids = unique(c(
      drug_warnings$molregno,
      molecule_hierarchy$parent_molregno
    )),
    select_cols = c(
      "molregno",   # link column (drug_warning)
      "chembl_id"   # output column
    )
  )
  # Fetch warning refs
  warning_refs <- fetch_table(
    con = con,
    table = "warning_refs",
    id_col = "warning_id",
    ids = as.integer(query),
    select_cols = c(
      "warning_id",         # link column (drug_warning)
      "ref_id",             # output column
      "ref_type",           # output column
      "ref_url"             # output column
    )
  )
  # Build output for each query
  out <- lapply(query, function(q) {
    q_int <- as.integer(q)
    # Get drug warning data
    dw <- drug_warnings |> dplyr::filter(.data$warning_id == q_int)
    if (nrow(dw) == 0) return(NA)
    # Get ChEMBL ID for molecule
    mol_chembl <- molecule_ids |>
      dplyr::filter(.data$molregno == dw$molregno) |>
      dplyr::pull(.data$chembl_id)
    # Get CHEMBL ID for parent molecule
    parent_molregno <- molecule_hierarchy |>
      dplyr::filter(.data$molregno == dw$molregno) |>
      dplyr::pull(.data$parent_molregno)
    parent_chembl <- molecule_ids |>
      dplyr::filter(.data$molregno == parent_molregno) |>
      dplyr::pull(.data$chembl_id)
    # Get warning refs for this warning_id
    warning_refs_data <- warning_refs |>
      dplyr::filter(.data$warning_id == q_int)
    # Build warning_refs list
    warning_refs_list <- list()
    if (nrow(warning_refs_data) > 0) {
      for (j in 1:nrow(warning_refs_data)) {
        warning_refs_list[[j]] <- list(
          ref_id = warning_refs_data$ref_id[j],
          ref_type = warning_refs_data$ref_type[j],
          ref_url = warning_refs_data$ref_url[j]
        )
      }
    }
    # Construct output matching webservice format
    out <- list(
      efo_id = dw$efo_id,
      efo_id_for_warning_class = dw$efo_id_for_warning_class,
      efo_term = dw$efo_term,
      molecule_chembl_id = mol_chembl,
      parent_molecule_chembl_id = parent_chembl,
      warning_class = dw$warning_class,
      warning_country = dw$warning_country,
      warning_description = dw$warning_description,
      warning_id = dw$warning_id,
      warning_refs = warning_refs_list,
      warning_type = dw$warning_type,
      warning_year = dw$warning_year
    )
    return(out)
  })
  names(out) <- query
  class(out) <- c("chembl_drug_warning_raw", class(out))

  if (output == "tidy") {
    stop("Tidy output for 'drug_warning' is not yet implemented.")
  }
  return(out)
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
  output = "raw",
  con
  ){
  # Fetch GO Slim classification data
  go_classification <- fetch_table(
    con = con,
    table = "go_classification",
    id_col = "go_id",
    ids = query,
    select_cols = c(
      "go_id",              # query column, output column
      "pref_name",          # output column
      "class_level",        # output column
      "aspect",             # output column
      "path",               # output column
      "parent_go_id"        # output column
    )
  )

  # Build output for each query
  out <- lapply(query, function(q) {
    # Get GO classification for this query
    go_data <- go_classification |> dplyr::filter(.data$go_id == q)

    if (nrow(go_data) == 0) return(NA)

    # Construct output matching webservice format
    result <- list(
      aspect = go_data$aspect,
      class_level = go_data$class_level,
      go_id = go_data$go_id,
      parent_go_id = go_data$parent_go_id,
      path = go_data$path,
      pref_name = go_data$pref_name
    )

    return(result)
  })

  names(out) <- query
  class(out) <- c("chembl_go_slim_raw", class(out))

  if (output == "tidy") {
    stop("Tidy output for 'go_slim' is not yet implemented.")
  }

  return(out)
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
  output = "raw",
  con
  ){
  stop("Offline 'mechanism' queries are not yet implemented.")
  # fetch relevant tables from database

  # loop through the queries and assemble raw output
  out <- unname(lapply(query, function(x) {
    # implementation here
  }))
  names(out) <- query
  if (output == "tidy") {
    stop("Tidy output for 'mechanism' is not yet implemented.")
  }
  return(out)
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
  output = "raw",
  con
  ){
  stop("Offline 'metabolism' queries are not yet implemented.")
  # fetch relevant tables from database

  # loop through the queries and assemble raw output
  out <- unname(lapply(query, function(x) {
    # implementation here
  }))
  names(out) <- query
  if (output == "tidy") {
    stop("Tidy output for 'metabolism' is not yet implemented.")
  }
  return(out)
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
    output = "raw",
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

  # TODO: add class and tidy output method
  # TODO: fix error with example query
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
  output = "raw",
  con
  ){
  stop("Offline 'molecule_form' queries are not yet implemented.")
  chembl_validate_id_offline(
    query = query,
    target = "COMPOUND",
    verbose = verbose,
    con = con
  )
  # fetch relevant tables from database

  # loop through the queries and assemble raw output
  out <- unname(lapply(query, function(x) {
    # implementation here
  }))
  names(out) <- query
  if (output == "tidy") {
    stop("Tidy output for 'molecule_form' is not yet implemented.")
  }
  return(out)
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
  output = "raw",
  con
  ){
  stop("Offline 'organism' queries are not yet implemented.")
  # fetch relevant tables from database

  # loop through the queries and assemble raw output
  out <- unname(lapply(query, function(x) {
    # implementation here
  }))
  names(out) <- query
  if (output == "tidy") {
    stop("Tidy output for 'organism' is not yet implemented.")
  }
  return(out)
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
  output = "raw",
  con
  ){
  stop("Offline 'protein_classification' queries are not yet implemented.")
  # fetch relevant tables from database

  # loop through the queries and assemble raw output
  out <- unname(lapply(query, function(x) {
    # implementation here
  }))
  names(out) <- query
  if (output == "tidy") {
    stop("Tidy output for 'protein_classification' is not yet implemented.")
  }
  return(out)
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
  output = "raw",
  con
  ){
  stop("Offline 'source' queries are not yet implemented.")
  # fetch relevant tables from database

  # loop through the queries and assemble raw output
  out <- unname(lapply(query, function(x) {
    # implementation here
  }))
  names(out) <- query
  if (output == "tidy") {
    stop("Tidy output for 'source' is not yet implemented.")
  }
  return(out)
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
  output = "raw",
  similarity = 70,
  con
  ){
  stop("Offline 'similarity' queries are not yet implemented.")
  # fetch relevant tables from database

  # loop through the queries and assemble raw output
  out <- unname(lapply(query, function(x) {
    # implementation here
  }))
  names(out) <- query
  if (output == "tidy") {
    stop("Tidy output for 'similarity' is not yet implemented.")
  }
  return(out)
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
  output = "raw",
  con
  ){
  stop("Offline 'substructure' queries are not yet implemented.")
  # fetch relevant tables from database

  # loop through the queries and assemble raw output
  out <- unname(lapply(query, function(x) {
    # implementation here
  }))
  names(out) <- query
  if (output == "tidy") {
    stop("Tidy output for 'substructure' is not yet implemented.")
  }
  return(out)
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
  output = "raw",
  con
  ){
  stop("Offline 'target' queries are not yet implemented.")
  chembl_validate_id_offline(
    query = query,
    target = "TARGET",
    verbose = verbose,
    con = con
  )
  # fetch relevant tables from database

  # loop through the queries and assemble raw output
  out <- unname(lapply(query, function(x) {
    # implementation here
  }))
  names(out) <- query
  if (output == "tidy") {
    stop("Tidy output for 'target' is not yet implemented.")
  }
  return(out)
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
  output = "raw",
  con
  ){
  stop("Offline 'target_component' queries are not yet implemented.")
  # fetch relevant tables from database

  # loop through the queries and assemble raw output
  out <- unname(lapply(query, function(x) {
    # implementation here
  }))
  names(out) <- query
  if (output == "tidy") {
    stop("Tidy output for 'target_component' is not yet implemented.")
  }
  return(out)
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
  output = "raw",
  con
  ){
  stop("Offline 'target_relation' queries are not yet implemented.")
  # fetch relevant tables from database

  # loop through the queries and assemble raw output
  out <- unname(lapply(query, function(x) {
    # implementation here
  }))
  names(out) <- query
  if (output == "tidy") {
    stop("Tidy output for 'target_relation' is not yet implemented.")
  }
  return(out)
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
  output = "raw",
  con
  ){
  stop("Offline 'tissue' queries are not yet implemented.")
  chembl_validate_id_offline(
    query = query,
    target = "TISSUE",
    verbose = verbose,
    con = con
  )
  # fetch relevant tables from database

  # loop through the queries and assemble raw output
  out <- unname(lapply(query, function(x) {
    # implementation here
  }))
  names(out) <- query
  if (output == "tidy") {
    stop("Tidy output for 'tissue' is not yet implemented.")
  }
  return(out)
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
  output = "raw",
  con
  ){
  stop("Offline 'xref_source' queries are not yet implemented.")
  # fetch relevant tables from database

  # loop through the queries and assemble raw output
  out <- unname(lapply(query, function(x) {
    # implementation here
  }))
  names(out) <- query
  if (output == "tidy") {
    stop("Tidy output for 'xref_source' is not yet implemented.")
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
  query,
  resource,
  version = "latest",
  verbose = getOption("verbose")
  ) {
  ws_result <- chembl_query(
    query = query,
    resource = resource,
    mode = "ws",
    verbose = verbose
  )
  offline_result <- chembl_query(
    query = query,
    resource = resource,
    mode = "offline",
    verbose = verbose
  )
  all.equal(ws_result, offline_result)
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
  for (i in seq_len(nrow(entity_type))) {
    if (entity_type$entity_type[i] != target) {
      msg <- paste0(
        entity_type$chembl_id[i], " is not a ", target, ". It is a ",
        entity_type$entity_type[i], ".")
      stop(msg)
    }
  }
}

#' Convert tidy data frame to raw list format
#'
#' @param query character; vector of query IDs.
#' @param df data.frame; tidy data frame to convert.
#' @return A named list where each element corresponds to a row in the data
#' frame, named by the query IDs.
#' @noRd
chembl_tidy2raw <- function(query, df, resource) {
  if (!is.data.frame(df) || nrow(df) == 0) {
    return(list())
  }
  # TODO handle pontential mismatch between query length and df rows
  res <- lapply(seq_len(nrow(df)), function(i) {
    raw_element <- df[i, , drop = FALSE]
    raw_element <- as.list(raw_element)
    raw_element[sort(names(raw_element))]
  })
  names(res) <- query
  class(res) <- c(paste0("chembl_", resource, "_raw"), class(res))
  res
}
