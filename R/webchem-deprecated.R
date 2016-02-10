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
#'   \code{pc_compinfo} \tab is now a synonym for \code{\link{cid_compinfo}}\cr
#' }
cid_compinfo <- function(...) {
  .Deprecated("pc_compinfo", package = "webchem")
  cid_compinfo(...)
}
