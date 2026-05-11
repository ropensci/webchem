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
  on.exit(DBI::dbDisconnect(con), add = TRUE)
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
    con_file <- file(f, open = "rb")
    df <- jsonlite::stream_in(con_file, verbose = FALSE)
    close(con_file)
    if (!inherits(df, "data.frame")) {
      stop("Expected a data frame from JSON file: ", f)
    }
    if (nrow(df) == 0) {
      if (verbose) message("Empty file. Moving on.")
      next
    }
    DBI::dbWriteTable(con, table_name, df, overwrite = TRUE)
    if (verbose) message("Done.")
  }
  if (verbose) message("SQLite database written to: ", sqlite_path)
  invisible(sqlite_path)
}
  }
  invisible(NULL)
}
