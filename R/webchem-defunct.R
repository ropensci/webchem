#' Defunct function(s) in the webchem package
#'
#' These functions are defunct and no longer available.
#' @rdname webchem-defunct
#' @name webchem-defunct
#' @export
#' @aliases ppdb_query
#' @aliases ppdb
#' @aliases cir
#' @details Defunct functions are:
#' \tabular{rl}{
#'   \code{ppdb_query} \cr
#'   \code{ppdb} \cr
#'   \code{cir} \cr
#' }
ppdb_query <- function() {
  .Defunct("ppdb_parse", package = "webchem",
msg = "ppdb_query() has been removed from the package due to copyright issues. \n
           You can use ppdb_parse() to parse html source code.")
}

#' @rdname webchem-defunct
#' @export
ppdb <- function() {
  .Defunct("ppdb", package = "webchem",
           msg = "ppdb() has been removed from the package due to copyright issues. \n
           You can use ppdb_parse() to parse html source code.")
}

#' @rdname webchem-defunct
#' @export
cir <- function() {
  .Defunct("cir_query", package = "webchem")
}