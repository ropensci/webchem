#' Download FooDB database
#'
#' Download the FooDB database for offline access.
#' @param verbose logical; should verbose messages be printed to the console?
#' @return Downloads the FooDB database file.
#' @note FooDB is a static database with a single release. To save storage space,
#' webchem only retrieves the JSON file. If you need other files as well,
#' please download them manually.
#' @references You can find more information about FooDB at
#' \url{https://foodb.ca/}
#' @examples
#' \dontrun{
#' db_download_foodb(verbose = TRUE)
#' }
#' @export
db_download_foodb <- function(verbose = getOption("verbose")) {
  stopifnot(is.logical(verbose), length(verbose) == 1)
  # Download
  url <- "https://foodb.ca/public/system/downloads/foodb_2020_04_07_json.zip"
  file_name <- basename(url)
  dir_path <- file.path(
    wc_cache$cache_path_get(),
    "foodb"
  ) |> path.expand()
  download_path <- file.path(dir_path, file_name)
  if (file.exists(download_path)) {
    if (verbose) message("Already downloaded.")
  } else {
    if (!dir.exists(dir_path)) dir.create(dir_path, recursive = TRUE)
    if (verbose) message("Downloading FooDB. ", appendLF = FALSE)
    curl::curl_download(url, download_path, quiet = TRUE)
    if (verbose) message("Done.")
  }
  # Convert to SQLite
  if (verbose) message("Converting to SQLite.")
  utils::unzip(download_path, exdir = dir_path)
  json_dir <- file.path(dir_path, tools::file_path_sans_ext(file_name))
  json_files <- list.files(json_dir, pattern = "\\.json$", full.names = TRUE)
  sqlite_path <- file.path(dir_path, "foodb_v1.sqlite")
  con <- DBI::dbConnect(RSQLite::SQLite(), sqlite_path)
  on.exit(DBI::dbDisconnect(con))
  for (f in json_files) {
    table_name <- tools::file_path_sans_ext(basename(f))
    if (DBI::dbExistsTable(con, table_name)) {
      if (verbose) {
        message("  Skipping table '", table_name, "'. Already converted.")
      }
      next()
    }
    if (verbose) {
      message("  Converting table '", table_name, "'. ", appendLF = FALSE)
    }
    chunk_size <- 10000L
    rows_processed <- 0L
    table_exists <- FALSE
    con_file <- file(f, open = "rb")
    tryCatch({
      jsonlite::stream_in(con_file, handler = function(df_chunk) {
        if (!inherits(df_chunk, "data.frame")) {
          stop("Expected a data frame from JSON file: ", f)
        }
        if (nrow(df_chunk) == 0) return(NULL)
        if (!table_exists) {
          DBI::dbWriteTable(con, table_name, df_chunk, overwrite = TRUE)
          table_exists <<- TRUE
        } else {
          DBI::dbWriteTable(con, table_name, df_chunk, append = TRUE)
        }
        rows_processed <<- rows_processed + nrow(df_chunk)
        NULL
      }, pagesize = chunk_size, verbose = FALSE)
    },
    finally = {
      if (isOpen(con_file)) {
        close(con_file)
      }
    })
    if (rows_processed == 0) {
      if (verbose) message("Empty file. Moving on.")
    } else {
      if (verbose) message("Done.")
    }
  }
  if (verbose) message("SQLite database written to: ", sqlite_path)
  invisible(sqlite_path)
}

#' Connect local FooDB database
#'
#' @importFrom rlang .data
#' @param ... Further args passed on to [DBI::dbConnect()]
#' @return an object of class "SQLiteConnection".
#' @examples
#' \dontrun{
#'   con <- connect_foodb(
#' }
#' @noRd
connect_foodb <- function(...) {
  db_path <- file.path(
    wc_cache$cache_path_get(),
    "foodb/foodb_v1.sqlite"
  ) |> path.expand()
  if (!file.exists(db_path)) {
    stop("Database not found. Use db_download_foodb() to download the database.")
  }
  con <- DBI::dbConnect(RSQLite::SQLite(), dbname = db_path, ...)
  return(con)
}

foodb_compound_idtypes <- function() {
  idtypes <- c(
    "id",
    "public_id",
    "name",
    "cas_number",
    "moldb_smiles",
    "moldb_inchi",
    "moldb_inchikey",
    "moldb_iupac"
  )
  return(idtypes)
}

#' Convert compound identifiers in the local FooDB database
#'
#' @param query A character vector of compound identifiers to convert.
#' @param from The type of identifier provided in \code{query}. See Details for
#' allowed values.
#' @param to The type of identifier to convert to. See Details for allowed
#' values.
#' @param verbose logical; should verbose messages be printed to the console?
#' @return A character vector of converted identifiers, in the same order as
#' the input \code{query}. If an identifier could not be converted, the
#' corresponding output will be \code{NA}.
#' @details Allowed values for \code{from} and \code{to} are:
#' \itemize{
#'   \item \code{id}: internal numeric ID in the FooDB database
#'   \item \code{public_id}: public numeric ID in the FooDB database
#'   \item \code{name}: compound name
#'   \item \code{cas_number}: CAS number
#'   \item \code{moldb_smiles}: SMILES string
#'   \item \code{moldb_inchi}: InChI string
#'   \item \code{moldb_inchikey}: InChIKey string
#'   \item \code{moldb_iupac}: IUPAC name
#' }
#' @examples
#' \dontrun{
#' foodb_convert("4", from = "id", to = "name")
#' foodb_convert("FDB000004", from = "public_id", to = "cas_number")
#' }
#' @export
foodb_convert <- function(query, from, to, verbose = getOption("verbose")) {
  con <- connect_foodb()
  on.exit(DBI::dbDisconnect(con))
  from <- match.arg(from, foodb_compound_idtypes(), several.ok = FALSE)
  to <- match.arg(to, foodb_compound_idtypes(), several.ok = FALSE)
  if (from == "name") {
    if (verbose) message("Harmonising compound names to match FooDB entries.")
    query <- foodb_harmonise_name(query, verbose = verbose)$foodb_name
  }
  compound <- fetch_table(
    con = con,
    table = "Compound",
    id_col = from,
    ids = query,
    select_cols = c(from, to)
  )
  foo <- function(x) {
    if (from == "moldb_inchikey") {
      if (!is.inchikey(x, type = "format")) {
        if (verbose) message("Invalid InChIKey format: ", x)
        return(NA_character_)
      }
    } else if (from == "cas_number") {
      if (!is.cas(x)) {
        if (verbose) message("Invalid CAS number format: ", x)
        return(NA_character_)
      }
    } else if (from == "moldb_smiles") {
      if (!is.smiles(x)) {
        if (verbose) message("Invalid SMILES format: ", x)
        return(NA_character_)
      }
    }
    out <- compound[[to]][which(compound[[from]] == x)]
    if (length(out) == 0) {
      if (verbose) message("No match found for identifier: ", x)
      return(NA_character_)
    }
    if (length(out) == 1) {
      return(out)
    }
    if (length(out) > 1) {
      stop("Multiple matches found for identifier: ", x)
    }
  }
  out <- unname(sapply(query, foo))
  # TODO - add class
  return(out)
}

foodb_expand_ontology_terms <- function(df, seed) {
  if (inherits(df, "tbl_SQLiteConnection")) {
    con <- connect_foodb()
    on.exit(DBI::dbDisconnect(con))
  }
  out <- seed
  parent_ids <- seed
  max_iterations <- 100L
  for (i in seq_len(max_iterations)) {
    parent_ids <- df |>
      dplyr::filter(!!rlang::sym("id") %in% parent_ids) |>
      dplyr::pull(!!rlang::sym("parent_id")) |>
      unique() |>
      (\(x) x[!is.na(x)])()
    
    if (length(parent_ids) == 0) break
    out <- unique(c(out, parent_ids))
  }
  if (i == max_iterations) {
    warning("Maximum iterations (", max_iterations, ") reached.")
  }
  return(out)
}

#' Fetch external IDs from FooDB
#'
#' @param con A database connection
#' @param ids Character vector of compound IDs
#'
#' @return A data frame with external IDs
#' @noRd
foodb_fetch_CompoundExternalDescriptor <- function(con, ids) {
  dplyr::tbl(con, "CompoundExternalDescriptor") |>
    dplyr::filter(!!rlang::sym("compound_id") %in% !!ids) |>
    dplyr::select("id", "external_id") |>
    dplyr::collect()
}

#' Fetch ontology IDs from FooDB
#'
#' @param con A database connection
#' @param ids Character vector of compound IDs
#'
#' @return A data frame with ontology IDs
#' @noRd
foodb_fetch_CompoundOntologyTerm <- function(con, ids) {
  dplyr::tbl(con, "CompoundOntologyTerm") |>
    dplyr::filter(!!rlang::sym("compound_id") %in% !!ids) |>
    dplyr::select("compound_id", "ontology_term_id") |>
    dplyr::collect()
}

#' Fetch ontology terms from FooDB
#'
#' @param con A database connection
#' @param ids Character vector of ontology term IDs
#'
#' @return A data frame with ontology term details
#' @noRd
foodb_fetch_OntologyTerm <- function(con, ids) {
  dplyr::tbl(con, "OntologyTerm") |>
    dplyr::filter(!!rlang::sym("id") %in% !!ontology_term_ids) |>
    dplyr::select(
      "id",
      "term",
      "definition",
      "external_id",
      "external_source",
      "comment",
      "parent_id",
      "level"
    ) |>
    dplyr::collect()
}

#' Fetch enzyme IDs from FooDB
#'
#' @param con A database connection
#' @param ids Character vector of compound IDs
#'
#' @return A data frame with enzyme IDs
#' @noRd
foodb_fetch_CompoundsEnzyme <- function(con, ids) {
  dplyr::tbl(con, "CompoundsEnzyme") |>
    dplyr::filter(!!rlang::sym("compound_id") %in% !!ids) |>
    dplyr::select(
      "compound_id",
      "enzyme_id",
      "citations"
    ) |>
    dplyr::collect()
}

#' Fetch enzyme data from FooDB
#'
#' @param con A database connection
#' @param enzyme_ids Character vector of enzyme IDs
#'
#' @return A data frame with enzyme details
#' @noRd
foodb_fetch_Enzyme <- function(con, enzyme_ids) {
  dplyr::tbl(con, "Enzyme") |>
    dplyr::filter(!!rlang::sym("id") %in% !!enzyme_ids) |>
    dplyr::select(
      "id",
      "name",
      "gene_name",
      "uniprot_id"
    ) |>
    dplyr::collect()
}

#' Fetch flavor IDs associations from FooDB
#'
#' @param con A database connection
#' @param ids Character vector of compound IDs
#'
#' @return A data frame with flavor IDs
#' @noRd
foodb_fetch_CompoundsFlavor <- function(con, ids) {
  dplyr::tbl(con, "CompoundsFlavor") |>
    dplyr::filter(!!rlang::sym("compound_id") %in% !!ids) |>
    dplyr::select(
      "compound_id",
      "flavor_id",
      "citations"
    ) |>
    dplyr::collect()
}

#' Fetch flavor data from FooDB
#'
#' @param con A database connection
#' @param flavor_ids Character vector of flavor IDs
#'
#' @return A data frame with flavor details
#' @noRd
foodb_fetch_Flavor <- function(con, flavor_ids) {
  dplyr::tbl(con, "Flavor") |>
    dplyr::filter(!!rlang::sym("id") %in% !!flavor_ids) |>
    dplyr::select(
      "id",
      "name",
      "flavor_group",
      "category"
    ) |>
    dplyr::collect()
}

#' Fetch health effect IDs from FooDB
#'
#' @param con A database connection
#' @param ids Character vector of compound IDs
#'
#' @return A data frame with health effect IDs
#' @noRd
foodb_fetch_CompoundsHealthEffect <- function(con, ids) {
  dplyr::tbl(con, "CompoundsHealthEffect") |>
    dplyr::filter(!!rlang::sym("compound_id") %in% !!ids) |>
    dplyr::select(
      "compound_id",
      "health_effect_id",
      "citation",
      "citation_type"
    ) |>
    dplyr::collect()
}

#' Fetch health effect data from FooDB
#'
#' @param con A database connection
#' @param health_effect_ids Character vector of health effect IDs
#'
#' @return A data frame with health effect details
#' @noRd
foodb_fetch_HealthEffect <- function(con, health_effect_ids) {
  dplyr::tbl(con, "HealthEffect") |>
    dplyr::filter(!!rlang::sym("id") %in% !!health_effect_ids) |>
    dplyr::select(
      "id",
      "name",
      "description",
      "chebi_name",
      "chebi_id",
      "chebi_definition"
    ) |>
    dplyr::collect()
}

#' Fetch pathway IDs from FooDB
#'
#' @param con A database connection
#' @param ids Character vector of compound IDs
#'
#' @return A data frame with pathway IDs
#' @noRd
foodb_fetch_CompoundsPathway <- function(con, ids) {
  dplyr::tbl(con, "CompoundsPathway") |>
    dplyr::filter(!!rlang::sym("compound_id") %in% !!ids) |>
    dplyr::select(
      "compound_id",
      "pathway_id"
    ) |>
    dplyr::collect()
}

#' Fetch pathway data from FooDB
#'
#' @param con A database connection
#' @param pathway_ids Character vector of pathway IDs
#'
#' @return A data frame with pathway details
#' @noRd
foodb_fetch_Pathway <- function(con, pathway_ids) {
  dplyr::tbl(con, "Pathway") |>
    dplyr::filter(!!rlang::sym("id") %in% !!pathway_ids) |>
    dplyr::select(
      "id",
      "smpdb_id",
      "kegg_map_id",
      "name"
    ) |>
    dplyr::collect()
}

#' Fetch content data from FooDB
#'
#' @param con A database connection
#' @param ids Character vector of compound IDs
#'
#' @return A data frame with content data
#' @noRd
foodb_fetch_Content <- function(con, ids) {
  dplyr::tbl(con, "Content") |>
    dplyr::filter(!!rlang::sym("source_type") == "Compound") |>
    dplyr::filter(!!rlang::sym("source_id") %in% !!ids) |>
    dplyr::select(
      "source_id",
      "food_id",
      "orig_food_part",
      "preparation_type",
      "standard_content",
      "orig_content",
      "orig_min",
      "orig_max",
      "orig_unit",
      "orig_unit_expression",
      "orig_method"
    ) |>
    dplyr::mutate(
      standard_content = as.numeric(!!rlang::sym("standard_content")),
      orig_content = as.numeric(!!rlang::sym("orig_content")),
      orig_min = as.numeric(!!rlang::sym("orig_min")),
      orig_max = as.numeric(!!rlang::sym("orig_max"))
    ) |>
    dplyr::collect()
}

#' Fetch food data from FooDB
#'
#' @param con A database connection
#' @param food_ids Character or numeric vector of food IDs
#'
#' @return A data frame with food details
#' @noRd
foodb_fetch_Food <- function(con, food_ids) {
  dplyr::tbl(con, "Food") |>
    dplyr::filter(!!rlang::sym("id") %in% !!food_ids) |>
    dplyr::select(
      "id",
      "name",
      "name_scientific",
      "description",
      "food_group",
      "food_subgroup",
      "food_type"
    ) |>
    dplyr::collect()
}

#' Build content output for a single compound query
#'
#' Combines content and food data for a single compound ID, handling joins
#' and NA cases.
#'
#' @param id The compound ID for which to build the content output
#' @param content A data frame with content data
#' @param food A data frame with food details for all compounds
#'
#' @return A tibble with content and food information, or a schema-conforming
#'   tibble of NAs if no content data exists
#' @noRd
foodb_build_content_output <- function(id, content, food) {
  content |> dplyr::filter(!!rlang::sym("source_id") == id)
  if (nrow(content_q) > 0) {
    food_q <- food |> dplyr::filter(!!rlang::sym("id") %in% content_q$food_id)
    if (nrow(food_q) == 0) {
      food_q <- data.frame(
        id = content_q$food_id,
        name = NA_character_,
        name_scientific = NA_character_,
        description = NA_character_,
        food_group = NA_character_,
        food_subgroup = NA_character_,
        food_type = NA_character_
      )
    }
    content_out <- content_q |>
      dplyr::left_join(food_q, by = c("food_id" = "id")) |>
      dplyr::relocate(
        dplyr::all_of(c(
          "name",
          "name_scientific",
          "description",
          "food_group",
          "food_subgroup",
          "food_type"
        )),
        .after = !!rlang::sym("food_id")
      ) |>
      dplyr::select(-!!rlang::sym("food_id"))
  } else {
    content_out <- tibble::tibble(
      source_id = NA_integer_,
      orig_food_part = NA_character_,
      preparation_type = NA_character_,
      standard_content = NA_real_,
      orig_content = NA_real_,
      orig_min = NA_real_,
      orig_max = NA_real_,
      orig_unit = NA_character_,
      orig_unit_expression = NA_character_,
      orig_method = NA_character_,
      name = NA_character_,
      name_scientific = NA_character_,
      description = NA_character_,
      food_group = NA_character_,
      food_subgroup = NA_character_,
      food_type = NA_character_
    )
  }
  return(content_out)
}

#' Harmonise compound names to match FooDB entries
#'
#' @param query A character vector of compound names to harmonise.
#' @param verbose logical; should verbose messages be printed to the console?
#' @return A data frame with two columns: `query` and `foodb_name`. The
#' `foodb_name` column contains the harmonised compound names. If a compound
#' name could not be harmonised, the corresponding `foodb_name` will be `NA`.
#' @details This function attempts to harmonise compound names in the input
#' `query` to match the names used in the FooDB database. It does this by
#' checking for exact matches first, and if none are found, it looks up
#' synonyms in the database. If multiple synonyms are found, the function will
#' stop with an error.
#' @examples
#' \dontrun{
#' foodb_harmonise_name("glucose")
#' foodb_harmonise_name(c("glucose", "fructose"))
#' }
#' @export
foodb_harmonise_name <- function(query, verbose = getOption("verbose")) {
  con <- connect_foodb()
  on.exit(DBI::dbDisconnect(con))
  if (all(is.na(query))) {
    if (verbose) message("No valid identifiers provided.")
    return(NA_character_)
  }
  harmonised_names <- data.frame(
    query = query,
    foodb_name = NA_character_
  )
  foodb_names <- foodb_list_compounds("name", verbose = verbose)
  index_harmonised <- which(query %in% foodb_names)
  if (length(index_harmonised) > 0) {
    harmonised_names$foodb_name[index_harmonised] <- query[index_harmonised]
  }
  query_na <- harmonised_names$query[which(is.na(harmonised_names$foodb_name))]
  synonyms <- dplyr::tbl(con, "CompoundSynonym") |>
    dplyr::filter(!!rlang::sym("synonym") %in% query_na) |>
    dplyr::select("synonym", "source_id") |>
    dplyr::collect() |>
    dplyr::mutate(
      foodb_name = foodb_convert(
        !!rlang::sym("source_id"),
        from = "id",
        to = "name",
        verbose = verbose
        )
    )
  foo <- function(i) {
    if (!is.na(harmonised_names$foodb_name[i])) {
      return(harmonised_names[i,])
    }
    hit <- synonyms[which(synonyms$synonym == harmonised_names$query[i]),]
    if (nrow(hit) == 0) {
      if (verbose) message("No match found for query: ", harmonised_names$query[i])
      return(harmonised_names[i,])
    }
    if (nrow(hit) == 1) {
      out <- harmonised_names[i,]
      out$foodb_name <- hit$foodb_name
      return(out)
    }
    if (nrow(hit) > 1) {
      stop("Multiple matches found for query: ", harmonised_names$query[i])
    }
  }
  out <- lapply(seq_len(nrow(harmonised_names)), foo) |> dplyr::bind_rows()
  return(out)
}

foodb_list_compounds <- function(idtype, verbose = getOption("verbose")) {
  idtype <- match.arg(idtype, foodb_compound_idtypes(), several.ok = FALSE)
  con <- connect_foodb()
  on.exit(DBI::dbDisconnect(con))
  compounds <- dplyr::tbl(con, "Compound") |>
    dplyr::select(idtype) |>
    dplyr::distinct() |>
    dplyr::pull()
  return(compounds)
}

foodb_query <- function(query, from, resource, verbose = getOption("verbose")) {
  con <- connect_foodb()
  on.exit(DBI::dbDisconnect(con))
  id <- foodb_convert(query, from = from, to = "id", verbose = verbose)
  compound <- dplyr::tbl(con, "Compound") |>
    dplyr::filter(!!rlang::sym("id") %in% !!id) |>
    dplyr::collect()
  if (nrow(compound) == 0) {
    if (verbose) message("No compound data found.")
    return(NA_character_)
  }

  # Fetch all data using helper functions
  compound_external <- foodb_fetch_CompoundExternalDescriptor(con, id)

  compound_ontology_term <- foodb_fetch_CompoundOntologyTerm(con, id)
  expanded_ontology_term_ids <- foodb_expand_ontology_terms(con, id)
  ontology_term <- foodb_fetch_OntologyTerm(con, expanded_ontology_term_ids)
  
  compound_enzyme <- foodb_fetch_CompoundsEnzyme(con, id)
  enzyme <- foodb_fetch_Enzyme(con, compound_enzyme$enzyme_id)
  
  compound_flavor <- foodb_fetch_CompoundsFlavor(con, id)
  flavor <- foodb_fetch_Flavor(con, compound_flavor$flavor_id)
  
  compound_he <- foodb_fetch_CompoundsHealthEffect(con, id)
  health_effect <- foodb_fetch_HealthEffect(con, compound_he$health_effect_id)
  
  compound_pathway <- foodb_fetch_CompoundsPathway(con, id)
  pathway <- foodb_fetch_Pathway(con, compound_pathway$pathway_id)

  # Fetch content and food data
  content <- foodb_fetch_Content(con, id)
  food <- if (nrow(content) > 0) {
    foodb_fetch_Food(con, content$food_id)
  } else {
    data.frame()
  }
  
  # Combine data into a single output for each query
  foo <- function(i) {
    q <- query[i]
    id_q <- id[i]
    compound_q <- compound |> 
      dplyr::filter(!!rlang::sym("id") == id_q)
    if (nrow(compound_q) == 0) {
      if (verbose) message("No compound data found for query: ", q)
      return(NA_character_)
    }
    compound_external_q <- compound_external |> 
      dplyr::filter(!!rlang::sym("id") == id_q)
    ontology_terms_q <- ontology_term |>
      dplyr::filter(!!rlang::sym("id") %in% foodb_expand_ontology_terms(
        df = ontology_term,
        seed = compound_ontology_term$ontology_term_id[which(compound_ontology_term$compound_id == id_q)]
      ))
    enzymes_q <- compound_enzyme |>
      dplyr::filter(!!rlang::sym("compound_id") == id_q) |>
      dplyr::left_join(
        enzyme,
        by = c("enzyme_id" = "id")
      ) |>
      dplyr::select(-"compound_id") |>
      dplyr::relocate(
        "citations",
        .after = dplyr::last_col()
      )
    flavor_q <- compound_flavor |>
      dplyr::filter(!!rlang::sym("compound_id") == id_q) |>
      dplyr::left_join(
        flavor,
        by = c("flavor_id" = "id")
      ) |>
      dplyr::select(-"compound_id") |>
      dplyr::relocate(
        "citations",
        .after = dplyr::last_col()
      )
    health_effect_q <- compound_he |>
      dplyr::filter(!!rlang::sym("compound_id") == id_q) |>
      dplyr::left_join(
        health_effect,
        by = c("health_effect_id" = "id")
      ) |>
      dplyr::select(-"compound_id") |>
      dplyr::relocate(
        "citation",
        "citation_type",
        .after = dplyr::last_col()
      )
    pathway_q <- compound_pathway |>
      dplyr::filter(!!rlang::sym("compound_id") == id_q) |>
      dplyr::pull(!!rlang::sym("pathway_id")) |>
      (\(x) pathway |> dplyr::filter(!!rlang::sym("id") %in% x))()
    
    # Build content output for this query  
    out <- list(
      public_id = compound_q$public_id,
      name = compound_q$name,
      state = compound_q$state,
      annotation_quality = compound_q$annotation_quality,
      description = compound_q$description,
      cas_number = compound_q$cas_number,
      moldb_smiles = compound_q$moldb_smiles,
      moldb_mono_mass = compound_q$moldb_mono_mass,
      moldb_inchikey = compound_q$moldb_inchikey,
      moldb_iupac = compound_q$moldb_iupac,
      kingdom = compound_q$kingdom,
      superklass = compound_q$superklass,
      klass = compound_q$klass,
      subklass = compound_q$subklass,
      external_descriptors = if (nrow(compound_external_q) > 0) {
        compound_external_q$external_id
      } else {
        NA_character_
      },
      ontology_terms = if (nrow(ontology_terms_q) > 0) {
        ontology_terms_q
      } else {
        tibble::tibble(
          id = NA_integer_,
          term = NA_character_,
          definition = NA_character_,
          external_id = NA_character_,
          external_source = NA_character_,
          comment = NA_character_,
          parent_id = NA_integer_,
          level = NA_integer_
        )
      },
      enzymes = if (nrow(enzymes_q) > 0) {
        enzymes_q
      } else {
        tibble::tibble(
          id = NA_integer_,
          name = NA_character_,
          gene_name = NA_character_,
          uniprot_id = NA_character_,
          citations = NA_character_
        )
      },
      flavor = if (nrow(flavor_q) > 0) {
        flavor_q
      } else {
        tibble::tibble(
          flavor_id = NA_integer_,
          name = NA_character_,
          flavor_group = NA_character_,
          category = NA_character_,
          citations = NA_character_
        )
      },
      health_effect = if (nrow(health_effect_q) > 0) {
        health_effect_q
      } else {
        tibble::tibble(
          health_effect_id = NA_integer_,
          name = NA_character_,
          description = NA_character_,
          chebi_name = NA_character_,
          chebi_id = NA_character_,
          chebi_definition = NA_character_,
          citation = NA_character_,
          citation_type = NA_character_
        )
      },
      pathway = if (nrow(pathway_q) > 0) {
        pathway_q
      } else {
        tibble::tibble(
          id = NA_integer_,
          smpdb_id = NA_character_,
          kegg_map_id = NA_character_,
          name = NA_character_
        )
      },
      content = foodb_build_content_output(id_q, content, food),
      synonyms = foodb_query_synonyms(
        query = q,
        from = from,
        verbose = verbose
      )
    )
    return(out)
  }
  out <- lapply(seq_along(query), foo)
  names(out) <- query
  return(out)
}

foodb_query_synonyms <- function(
  query,
  from = "name",
  verbose = getOption("verbose")) {
  con <- connect_foodb()
  on.exit(DBI::dbDisconnect(con))
  from <- match.arg(from, foodb_compound_idtypes(), several.ok = FALSE)
  if (from != "id") {
    query_id <- foodb_convert(query, from = from, to = "id", verbose = verbose)
  } else {
    query_id <- query
  }
  query_nona <- query_id[!is.na(query_id)]
  if (length(query_nona) == 0) {
    if (verbose) message("No valid identifiers provided.")
    return(NA_character_)
  }
  synonyms <- dplyr::tbl(con, "CompoundSynonym") |>
    dplyr::filter(!!rlang::sym("source_id") %in% query_nona) |>
    dplyr::select("source_id", "synonym") |>
    dplyr::collect()

  foo <- function(i) {
    q <- query[i]
    id <- query_id[i]
    if (is.na(id)) {
      if (verbose) message("Query not found in database: ", q)
      data.frame(
        query = q,
        synonym = NA_character_,
        foodb_name = NA_character_
      )
    } else {
      query_synonyms <- synonyms[synonyms$source_id == id, "synonym", drop = TRUE]
      if (length(query_synonyms) == 0) {
        if (verbose) message("No synonyms found for query: ", q)
        data.frame(
          query = q,
          synonym = NA_character_,
          foodb_name = q
        )
      } else {
        data.frame(
          query = q,
          synonym = query_synonyms,
          foodb_name = foodb_convert(
            id, 
            from = "id", 
            to = "name", 
            verbose = FALSE
          )
        )
      }
    }
  }
  out <- lapply(seq_along(query), foo) |> dplyr::bind_rows()
  return(out)
}

