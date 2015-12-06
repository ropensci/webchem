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
#'   \code{csid_extcompinfo} \tab is now a synonym for \code{\link{cs_extcompinfo}}\cr
#'   \code{csid_compinfo} \tab is now a synonym for \code{\link{cs_compinfo}}\cr
#' }
ppdb_query <- function(...) {
  .Deprecated("ppdb", package = "webchem")
  ppdb(...)
}


#' @rdname webchem-deprecated
#' @export
cir_query <- function(...) {
  .Deprecated("cir", package = "webchem")
  cir(...)
}

#' @rdname webchem-deprecated
#' @export
csid_extcompinfo <- function(...) {
  .Deprecated("cs_extcompinfo", package = "webchem")
  cs_extcompinfo(...)
}


#' @rdname webchem-deprecated
#' @export
csid_compinfo <- function(...) {
  .Deprecated("cs_compinfo", package = "webchem")
  cs_compinfo(...)
}