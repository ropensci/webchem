#' Deprecated function(s) in the webchem package
#'
#' These functions are provided for compatibility with older version of
#' the webchem package.  They may eventually be completely
#' removed.
#' @rdname webchem-deprecated
#' @name webchem-deprecated
#' @param ... Parameters to be passed to the modern version of the function
#' @docType package
#' @export  allanwood
#' @aliases allanwood
#' @section Details:
#' \tabular{rl}{
#'   \code{allanwood} \tab now a synonym for \code{\link{alanwood}}\cr
#' }
allanwood <- function(...) {
  .Deprecated("alanwood", package = "webchem")
  alanwood(...)
}