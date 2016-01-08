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
#' @aliases cir_query
#' @aliases csid_extcompinfo
#' @aliases csid_compinfo
#' @details Deprecated functions are:
#' \tabular{rl}{
#'   \code{ppdb_query} \tab is now a synonym for \code{\link{ppdb}}\cr
#'   \code{cir_query} \tab is now a synonym for \code{\link{cir}}\cr
#'   \code{pan_query} \tab is now a synonym for \code{\link{pan}}\cr
#' }
ppdb <- function(...) {
  .Deprecated("ppdb_query", package = "webchem")
  ppdb(...)
}

#' @rdname webchem-deprecated
#' @export
cir <- function(...) {
  .Deprecated("cir_query", package = "webchem")
  cir(...)
}

#' @rdname webchem-deprecated
#' @export
pan <- function(...) {
  .Deprecated("pan_query", package = "webchem")
  cir(...)
}
