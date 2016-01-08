#' Deprecated function(s) in the webchem package
#'
#' These functions are provided for compatibility with older version of
#' the webchem package.  They may eventually be completely
#' removed.
#' @rdname webchem-deprecated
#' @name webchem-deprecated
#' @param ... Parameters to be passed to the modern version of the function
#' @export
#' @aliases ppdb
#' @aliases cir
#' @aliases pan
#' @aliases alanwood
#' @aliases physprop
#' @aliases get_cid
#' @aliases cid_compinfo
#' @details Deprecated functions are:
#' \tabular{rl}{
#'   \code{ppdb_query} \tab is now a synonym for \code{\link{ppdb}}\cr
#'   \code{cir_query} \tab is now a synonym for \code{\link{cir}}\cr
#'   \code{pan_query} \tab is now a synonym for \code{\link{pan}}\cr
#'   \code{aw_query} \tab is now a synonym for \code{\link{alanwood}}\cr
#'   \code{pp_query} \tab is now a synonym for \code{\link{physprop}}\cr
#'   \code{get_pcid} \tab is now a synonym for \code{\link{get_cid}}\cr
#'   \code{pc_compinfo} \tab is now a synonym for \code{\link{cid_compinfo}}\cr
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

#' @rdname webchem-deprecated
#' @export
alanwood <- function(...) {
  .Deprecated("aw_query", package = "webchem")
  cir(...)
}

#' @rdname webchem-deprecated
#' @export
physprop <- function(...) {
  .Deprecated("pp_query", package = "webchem")
  cir(...)
}

#' @rdname webchem-deprecated
#' @export
get_cid <- function(...) {
  .Deprecated("get_pcid", package = "webchem")
  cir(...)
}

#' @rdname webchem-deprecated
#' @export
cid_compinfo <- function(...) {
  .Deprecated("pc_compinfo", package = "webchem")
  cir(...)
}

#' @rdname webchem-deprecated
#' @export
chemid <- function(...) {
  .Deprecated("ci_query", package = "webchem")
  cir(...)
}

