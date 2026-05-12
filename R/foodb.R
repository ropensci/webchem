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
foodb_convert <- function(query, from, to, verbose = getOption("verbose")) {
  con <- connect_foodb()
  on.exit(DBI::dbDisconnect(con))
  compound_ids <- c(
    "id", 
    "public_id", 
    "name", 
    "cas_number", 
    "moldb_smiles", 
    "moldb_inchi", 
    "moldb_inchikey", 
    "moldb_iupac"
  )
  from <- match.arg(from, compound_ids)
  to <- match.arg(to, compound_ids)
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
    dplyr::select(!!rlang::sym("synonym"), !!rlang::sym("source_id")) |>
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
