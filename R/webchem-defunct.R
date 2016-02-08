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


#' @rdname webchem-defunct
#' @export
ppdb_query <- function() {
  .Defunct("ppdb_parse", package = "webchem",
msg = "ppdb_query has been removed from the package due to copyright issues. \n
           You can use ppdb_parse() to parse html source code.")
}