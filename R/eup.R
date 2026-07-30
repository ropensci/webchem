#' Connect local EU Pesticides database
#'
#' @param ... Further args passed on to [DBI::dbConnect()]
#' @return an object of class "SQLiteConnection".
#' @examples
#' \dontrun{
#'   con <- connect_eup()
#' }
#' @noRd
connect_eup <- function(...) {
  db_path <- file.path(
    wc_cache$cache_path_get(),
    "eup/eup.sqlite"
  ) |> path.expand()
  if (!file.exists(db_path)) {
    stop("Database not found. Use db_download_eup() to download the database.")
  }
  con <- DBI::dbConnect(RSQLite::SQLite(), dbname = db_path, ...)
  return(con)
}

#' Convert identifiers in the local EU Pesticides database
#'
#' @param query character; a character of compound identifiers to convert.
#' @param from character; the type of identifier to conver from. Can be one of
#' \code{"substance_id"}, \code{"substance_name"}, or \code{"as_cas_number"}.
#' @param to character; the type of identifier to convert to. Can be one of
#' \code{"substance_id"}, \code{"substance_name"}, or \code{"as_cas_number"}.
#' @param resource character; the EU Pesticides resource to query. Can be one of
#' \code{"active_substances"} or \code{"residues"}.
#' @param mode character; the mode of operation. Can be one of \code{"offline"} 
#' (offline access) or \code{"ws"} (web service access). Currently only offline 
#' mode is implemented.
#' ... Further args passed on to [DBI::dbConnect()]
#' @return A data frame of converted identifiers, in the same order as
#' the input \code{query}. If an identifier could not be converted, the
#' corresponding output will be \code{NA}. If multiple matches are found for a
#' query, all matches will be returned in separate rows.
#' @details \code{"substance_id"} is the unique identifier for each active
#' substance in the EU Pesticides database. \code{"substance_name"} is the name
#' of the active substance, and \code{"as_cas_number"} is the CAS number of the
#' active substance.
#' @references You can find more information about the EU Pesticides database at
#' \url{https://food.ec.europa.eu/plants/pesticides/eu-pesticides-database_en}.
#' @examples
#' \dontrun{
#' # Download database
#' db_download_eup(verbose = TRUE)
#'
#' # Query downloaded database
#' eup_convert(1313, from = "substance_id", to = "substance_name")
#' eup_convert("Monolinuron", from = "substance_name", to = "as_cas_number")
#'
#' eup_convert()
#' }
#' @export
eup_convert <- function(
  query,
  from,
  to,
  resource = "active_substances",
  mode = "offline",
  ...
) {
  resource <- match.arg(resource, choices = c(
    "active_substances",
    "residues"
  ))
  if (resource == "active_substances") {
    idtypes <- c(
      "substance_id",
      "substance_name",
      "as_cas_number"
    )
  } else {
    idtypes <- c(
      "pesticide_residue_id",
      "pesticide_residue_name"
    )
  }
  from <- match.arg(from, choices = idtypes)
  to <- match.arg(to, choices = idtypes)
  if (from == to) {
    stop("From and to identifier types must be different.")
  }
  mode <- match.arg(mode, choices = c("ws", "offline"))
  if (mode == "ws") {
    stop("Web service mode is not implemented. Please use mode = 'offline'.")
  } else {
    eup_convert_offline(
      query = query,
      from = from,
      to = to,
      resource = resource,
      ...
    )
  }
}

eup_convert_offline <- function(
  query,
  from,
  to,
  resource,
  ...
) {
  if (from %in% c("substance_id", "pesticide_residue_id") && !is.numeric(query)) {
    stop("query must be a vector of numbers.")
  }
  if (from != "substance_id" && !is.character(query)) {
    stop("query must be a vector of strings.")
  }
  con <- connect_eup(...)
  on.exit(DBI::dbDisconnect(con))
  if (resource == "active_substances") {
    out <- fetch_table(
      con = con,
      table = "active_substances",
      id_col = from,
      ids = query,
      select_cols = c(from, to)
    )
  } else {
    out <- fetch_table(
      con = con,
      table = "residues",
      id_col = from,
      ids = query,
      select_cols = c(from, to)
    )
  }
  return(out)
}

#' List available entries in the local EU Pesticides database
#' 
#' @param idtype character; the type of identifier to list. Allowed values are:
#' "substance_id", "substance_name", "as_cas_number", "pesticide_residue_id", 
#' "pesticide_residue_name".
#' @param verbose logical; should verbose messages be printed to the console?
#' @return A character vector of unique identifiers of the specified type that 
#' are present in the EU Pesticides database.
#' @references You can find more information about the EU Pesticides database at
#' \url{https://food.ec.europa.eu/plants/pesticides/eu-pesticides-database_en}.
#' @examples
#' \dontrun{
#' eup_list_entries("substance_name")
#' eup_list_entries("pesticide_residue_name")
#' }
#' @export
eup_list_entries <- function(
  idtype,
  verbose = getOption("verbose")
) {
  idtypes <- c(
    "substance_id",
    "substance_name",
    "as_cas_number",
    "pesticide_residue_id",
    "pesticide_residue_name"
  )
  idtype <- match.arg(idtype, choices = idtypes)
  con <- connect_eup()
  on.exit(DBI::dbDisconnect(con))
  if (idtype %in% c("substance_id", "substance_name", "as_cas_number")) {
    table <- "active_substances"
  } else {
    table <- "residues"
  }
  if (verbose) message("Retrieving entries from table '", table, "'...")
  ids <- dplyr::tbl(con, table) |>
    dplyr::select(idtype) |>
    dplyr::distinct() |>
    dplyr::pull() |>
    sort()
  return(ids)
}

#' Query EU Pesticides
#'
#' @param query numeric; a vector of IDs. The type of ID depends on the
#' resource. See examples for more information.
#' @param resource character; the EU Pesticides resource to query. Can be one of
#' \code{"active_substances"} or \code{"residues"}.
#' @param mode character; the mode of operation. Can be one of \code{"offline"} 
#' (offline access) or \code{"ws"} (web service access). Currently only offline 
#' mode is implemented.
#' @param ... Further args passed on to [DBI::dbConnect()]
#' @return A data frame containing information about the specified entry.
#' @references You can find more information about the EU Pesticides database at
#' \url{https://food.ec.europa.eu/plants/pesticides/eu-pesticides-database_en}.
#' @examples
#' \dontrun{
#' # Download database
#' db_download_eup(verbose = TRUE)
#'
#' # Retrieve information about active substances
#' eup_query(query = c(1313, 1314), resource = "active_substances")
#'
#' # Retrieve information about residues
#' eup_query(query = c(1, 2), resource = "residues")
#' }
#' @export
eup_query <- function(
  query, 
  resource, 
  mode = "offline",
  ...) {
  if (!is.numeric(query)) {
    stop("query must be a vector of numbers.")
  }
  resource <- match.arg(resource, choices = c(
    "active_substances",
    "residues"
  ))
  mode <- match.arg(mode, choices = c("ws", "offline"))
  if (mode == "ws") {
    stop("Web service mode is not implemented. Please use mode = 'offline'.")
  } else {
    eup_query_offline(
      query = query,
      resource = resource,
      ...
    )
  }
}

eup_query_offline <- function(
  query,
  resource,
  ...
) {
  con <- connect_eup(...)
  on.exit(DBI::dbDisconnect(con))
  if (resource == "active_substances") {
    out <- fetch_table(
      con = con,
      table = "active_substances",
      id_col = "substance_id",
      ids = query
    )
  } else {
    out <- fetch_table(
      con = con,
      table = "residues",
      id_col = "pesticide_residue_id",
      ids = query
    )
  }
  return(out)
}

#' Download the EU Pesticides database and convert to SQLite
#'
#' This function downloads the EU Pesticides database in JSON format and
#' converts it to a local SQLite database for offline use. Note, the current
#' implementation of the database only contains information about active
#' substances (no residues and maximum residue limits).
#' @param verbose logical; print verbose messages to the console?
#' @return The path to the created SQLite database file.
#' @details
#' By default the SQLite database is stored in the cache directory of the
#' webchem package. You can view the current cache directory by using the
#' `wc_cache$cache_path_get()` function, and you can change the cache directory
#' by using the `wc_cache$cache_path_set()` function.
#' @references You can find more information about the EU Pesticides database at
#' \url{https://food.ec.europa.eu/plants/pesticides/eu-pesticides-database_en}.
#' @examples
#' \dontrun{
#' # Set cache path to a temporary directory
#' wc_cache$cache_path_set(full_path = tempdir())
#' # Download and convert the EU Pesticides database
#' db_download_eup(verbose = TRUE)
#' }
#' @export
db_download_eup <- function(verbose = getOption("verbose")) {
  db_download_eup_url(
    dirname = "eup",
    filename = "active_substances.json",
    resource = "EU Pesticides",
    table = "Active Substances",
    url = "https://api.datalake.sante.service.ec.europa.eu/sante/pesticides/active-substances-download?format=json&api-version=v3.0",
    verbose = verbose
  )
  db_download_eup_url(
    dirname = "eup",
    filename = "residues.json",
    resource = "EU Pesticides",
    table = "Residues and Maximum Residue Levels",
    url = "https://api.datalake.sante.service.ec.europa.eu/sante/pesticides/pesticide-residues-mrls-download?language_code=EN&format=json&api-version=v3.0",
    verbose = verbose
  )
  dir_path <- file.path(
    wc_cache$cache_path_get(),
    "eup"
  ) |> path.expand()
  json_dir <- dir_path
  json_files <- list.files(json_dir, pattern = "\\.json$", full.names = TRUE)
  sqlite_path <- file.path(dir_path, "eup.sqlite")
  if (verbose) message("Converting to SQLite.")
  con <- DBI::dbConnect(RSQLite::SQLite(), sqlite_path)
  on.exit(DBI::dbDisconnect(con))
  # Cleanup orphaned temp tables from interrupted runs
  existing_tables <- DBI::dbListTables(con)
  tmp_tables <- grep("__tmp$", existing_tables, value = TRUE)
  for (tmp in tmp_tables) {
    if (verbose) {
      message("Removing incomplete temporary table '", tmp, "'.")
    }
    DBI::dbRemoveTable(con, tmp)
  }
  for (f in json_files) {
    table_name <- tools::file_path_sans_ext(basename(f))
    tmp_table <- paste0(table_name, "__tmp")
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
          DBI::dbWriteTable(con, tmp_table, df_chunk, overwrite = TRUE)
          table_exists <<- TRUE
        } else {
          DBI::dbWriteTable(con, tmp_table, df_chunk, append = TRUE)
        }
        rows_processed <<- rows_processed + nrow(df_chunk)
        NULL
      }, pagesize = chunk_size, verbose = FALSE)
      if (table_exists) {
        DBI::dbExecute(
          con,
          paste(
            "ALTER TABLE",
            DBI::dbQuoteIdentifier(con, tmp_table),
            "RENAME TO", DBI::dbQuoteIdentifier(con, table_name)
          )
        )
      }
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

#' Download a file from the EU Pesticides database
#'
#' @param dirname character; the name of the directory to store the downloaded
#' file in.
#' @param filename character; the name of the file to download.
#' @param resource character; the name of the resource being downloaded (used
#' for verbose messages).
#' @param table character; the name of the table being downloaded (used for
#' verbose messages).
#' @param url character; the URL to download the file from.
#' @param verbose logical; should verbose messages be printed to the console?
#' @return NULL. The function downloads the json file.
#' @noRd
db_download_eup_url <- function(
  dirname,
  filename,
  resource,
  table,
  url,
  verbose = getOption("verbose")
) {
  stopifnot(is.logical(verbose), length(verbose) == 1)
  file_name <- filename
  dir_path <- file.path(
    wc_cache$cache_path_get(),
    dirname
  ) |> path.expand()
  download_path <- file.path(dir_path, file_name)
  if (verbose) message(
    paste0("Downloading ", resource, " - ", table, ". "), appendLF = FALSE)
  if (file.exists(download_path)) {
    if (verbose) message("Already downloaded.")
  } else {
    if (!dir.exists(dir_path)) dir.create(dir_path, recursive = TRUE)
    curl::curl_download(url, download_path, quiet = TRUE)
    if (verbose) message("Done.")
  }
}
