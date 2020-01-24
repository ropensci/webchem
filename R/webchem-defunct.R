#' Defunct function(s) in the webchem package
#'
#' These functions are defunct and no longer available.
#' @rdname webchem-defunct
#' @name webchem-defunct
#' @export
#' @aliases ppdb_query
#' @aliases ppdb_parse
#' @aliases ppdb
#' @aliases cir
ppdb_query <- function() {
  .Defunct(
    "ppdb_parse",
    package = "webchem",
    msg = "ppdb_query() has been removed from the package due to copyright issues.")
}

#' @rdname webchem-defunct
#' @export
ppdb_parse <- function() {
  .Defunct(
    "ppdb",
    package = "webchem",
    msg = "ppdb_parse() has been removed from the package due to copyright issues.")
}

#' @rdname webchem-defunct
#' @export
ppdb <- function() {
  .Defunct(
    "ppdb",
    package = "webchem",
    msg = "ppdb() has been removed from the package due to copyright issues.")
}

#' @rdname webchem-defunct
#' @export
cir <- function() {
  .Defunct("cir_query", package = "webchem")
}