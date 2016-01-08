#' Defunct function(s) in the webchem package
#'
#' These functions are defunct and no longer available.
#' @rdname webchem-defunct
#' @name webchem-defunct
#' @export
#' @aliases csid_extcompinfo
#' @aliases csid_compinfo
#' @details Defunct functions are:
#' \tabular{rl}{
#'   \code{csid_extcompinfo} \cr
#'   \code{csid_extcompinfo} \cr
#' }
csid_extcompinfo <- function() {
  .Defunct("cs_extcompinfo", package = "webchem")
}


#' @rdname webchem-defunct
#' @export
csid_compinfo <- function() {
  .Defunct("cs_compinfo", package = "webchem")
}