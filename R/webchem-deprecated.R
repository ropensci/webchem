#' Deprecated function(s) in the webchem package
#'
#' These functions are provided for compatibility with older version of
#' the webchem package.  They may eventually be completely
#' removed.
#' @rdname webchem-deprecated
#' @name webchem-deprecated
#' @param ... Parameters to be passed to the modern version of the function
#' @export
#' @aliases cid_compinfo
#' @details Deprecated functions are:
#' \tabular{rl}{
#'   \code{pc_prop} \tab was formerly \code{\link{cid_compinfo}}\cr
#'   \code{bcpc_query} \tab was formerly \code{\link{aw_query}}\cr
#' }
cid_compinfo <- function(...) {
  .Deprecated("pc_prop", package = "webchem")
  cid_compinfo(...)
}

#' @rdname webchem-deprecated
#' @aliases aw_query
#' @export
aw_query <- function(...) {
  .Deprecated("bcpc_query", package = "webchem")
  bcpc_query(...)
}
