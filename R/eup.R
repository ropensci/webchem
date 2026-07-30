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
