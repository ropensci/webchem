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
