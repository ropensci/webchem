#' Defunct function(s) in the webchem package
#'
#' These functions are defunct and no longer available.
#' @rdname webchem-defunct
#' @name webchem-defunct
#' @export
#' @aliases ppdb_query
#' @aliases ppdb_parse
#' @aliases ppdb
#' @aliases cir
ppdb_query <- function() {
  .Defunct(
    "ppdb_parse",
    package = "webchem",
    msg = "ppdb_query() has been removed from the package due to copyright
    issues.")
}

#' @rdname webchem-defunct
#' @export
ppdb_parse <- function() {
  .Defunct(
    "ppdb",
    package = "webchem",
    msg = "ppdb_parse() has been removed from the package due to copyright
    issues.")
}

#' @rdname webchem-defunct
#' @export
ppdb <- function() {
  .Defunct(
    "ppdb",
    package = "webchem",
    msg = "ppdb() has been removed from the package due to copyright issues.")
}

#' @rdname webchem-defunct
#' @export
cir <- function() {
  .Defunct("cir_query", package = "webchem")
}

#' @rdname webchem-defunct
#' @export
pp_query <- function() {
  .Defunct(
    "pp_query",
    package = "webchem",
    msg = "pp_query() has been removed from the package since the Physprop API
    is no longer active."
  )
}

#' @rdname webchem-defunct
#' @export
cs_prop <- function() {
  .Defunct(
    "cs_prop",
    package = "webchem",
    msg = "cs_prop() has been removed from the package since RSC does not allow
    the scraping of ChemSpider pages."
  )
}

#' @rdname webchem-defunct
#' @export
ci_query <- function() {
  .Defunct(
    "ci_query",
    package = "webchem",
    msg = paste0(
      "ci_query() has been removed from the package because NLM had retired ",
      "ChemIDplus. According to NLM all data found in ChemIDplus is available ",
      "in PubChem. 'webchem' provides a number of functions for ",
      "programmatically accessing PubChem."
    )
  )
}

#' @rdname webchem-defunct
#' @export
pan_query <- function() {
  .Defunct(
    "pan_query",
    package = "webchem",
    msg = paste0(
      "pan_query() has been removed from the package because programmatic ",
      "access to the Pesticide Action Network database is no longer supported."
    )
  )
}
