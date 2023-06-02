
#' Auto-translate identifiers and search databases
#'
#' Supply a query of any type (e.g. SMILES, CAS, name, InChI, etc.) along with
#' any webchem function that has \code{query} and \code{from} arguments.  If the
#' function doesn't accept the type of query you've supplied, this will try to
#' automatically translate it using CTS and run the query.
#'
#' @param query character; the search term
#' @param from character; the format or type of query.  Commonly accepted values
#'   are "name", "cas", "inchi", and "inchikey"
#' @param .f character; the (quoted) name of a webchem function
#' @param .verbose logical; print a message when translating query?
#' @param ... other arguments passed to the function specified with \code{.f}
#' @note During the translation step, only the first hit from CTS is used.
#'   Therefore, using this function to translate on the fly is not foolproof and
#'   care should be taken to verify the results.
#' @return returns results from \code{.f}
#' @importFrom rlang as_function fn_fmls
#'
#' @examples
#' \dontrun{
#' with_cts("XDDAORKBJWWYJS-UHFFFAOYSA-N", from = "inchikey", .f = "get_etoxid")
#' }
with_cts <- function(query, from, .f, .verbose = getOption("verbose"), ...) {
  f <- rlang::as_function(.f)
  pos_froms <- eval(rlang::fn_fmls(f)$from)

  if (!from %in% pos_froms) {
    pos_froms <- pos_froms[pos_froms != "name"] #cts name conversion broken
    new_from <- pos_froms[which(pos_froms %in% cts_to())[1]]
    if (.verbose) {
      message(
        paste0(.f,
               " doesn't accept ",
               from,
               ".\n",
               "Attempting to translate to ",
               new_from,
               " with CTS. ")
      )
    }
    new_query <- cts_convert(query, from = from, to = new_from, match = "first")
    #would like to try a again if cts fails the first time (as it often does).
    from <- new_from
    query <- new_query
  }
  f(query = query, from = from, ...)
}



#' Check data source coverage of compounds
#'
#' Checks if entries are found in (most) data sources included in webchem
#'
#' @param query character; the search term
#' @param from character; the format or type of query.  Commonly accepted values
#'   are "name", "cas", "inchi", and "inchikey"
#' @param sources character; which data sources to check.  Data sources are
#'   identified by the prefix associated with webchem functions that query those
#'   databases.  If not specified, all data sources listed will be checked.
#' @param plot logical; plot a graphical representation of results.
#'
#' @return a tibble of logical values where \code{TRUE} indicates that a data
#'   source contains a record for the query
#' @export
#' @import dplyr
#' @importFrom stringr str_trunc
#' @examples
#' \dontrun{
#' find_db("hexane", from = "name")
#' }
find_db <- function(query, from,
                           sources = c("etox", "pc", "chebi", "cs",
                                       "bcpc", "fn", "srs"),
                           plot = FALSE) {
  sources <- match.arg(sources, several.ok = TRUE)
  sources <- sapply(sources, switch,
                    "etox" = "get_etoxid",
                    "pc" = "get_cid",
                    "chebi" = "get_chebiid",
                    "cs" = "get_csid",
                    "bcpc" = "bcpc_query",
                    "fn" = "fn_percept",
                    "srs" = "srs_query")

  foo <- function(.f, query, from) {
    # if a function errors (e.g. API is down) then return NA
    x <-
      try(with_cts(
        query = query,
        from = from,
        .f = .f,
        match = "first"
      ))
    if (inherits(x, "try-error")) {
      return(NA)
    }
    if (inherits(x, "data.frame")) {
      x <- x[[ncol(x)]]
    }
    return(!is.na(x))
  }

  out <- lapply(sources, foo, query = query, from = from)
  names(out) <- names(sources)
  out <- dplyr::bind_cols(query = query, out)

  if (plot) {
    if (!requireNamespace("plot.matrix", quietly = TRUE)) {
      warning("The plot.matrix package is required for plotting results")
    } else {
      out <- filter(out, !is.na(query))
      colorder <- select(out, -query) %>%
        colSums(., na.rm = TRUE) %>%
        sort(decreasing = TRUE) %>%
        names()
      pmat <- out %>%
        select(all_of(colorder)) %>%
        as.matrix()
      opar <- graphics::par(no.readonly = TRUE)

      query_trunc <- stringr::str_trunc(query, 25)
      leftmargin <- 0.5 * max(nchar(query_trunc))
      graphics::par(mar = c(5.1, leftmargin, 4.1, 4.1))

      plot(
        pmat,
        col = c("#C7010B", "#3BC03B"),
        breaks = c(FALSE, TRUE),
        na.col = "grey70",
        axis.col = list(side = 3),
        axis.row = list(las = 2, labels = query_trunc),
        xlab = NA,
        ylab = NA,
        main = NA
      )
      graphics::par(opar)
    }
  }
  return(out)
}
