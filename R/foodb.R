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