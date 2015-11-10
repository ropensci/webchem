#' Deprecated function(s) in the webchem package
#'
#' These functions are provided for compatibility with older version of
#' the webchem package.  They may eventually be completely
#' removed.
#' @rdname webchem-deprecated
#' @name webchem-deprecated
#' @param ... Parameters to be passed to the modern version of the function
#' @export
#' @aliases ppdb_query
#' @details Deprecated functions are:
#' \tabular{rl}{
#'   \code{ppdb_query} \tab now a synonym for \code{\link{ppdb}}\cr
#' }
ppdb_query <- function(...) {
  .Deprecated("ppdb_query", package = "webchem")
  ppdb(...)
}