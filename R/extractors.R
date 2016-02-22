#' Extract parts from webchem objects
#' @name extractors
#' @rdname extractors
#' @param x object
#' @param ... currently not used.
#' @return a vector.
#' @export
cas <- function(x, ...){
  UseMethod("cas")
}


#' @export
cas.default <- function(x, ...) {
  sapply(x, function(y) {
    if (length(y) == 1 && is.na(y))
      return(NA)
    y$cas
    })
}

#' @export
cas.pan_query <- function(x, ...) {
  sapply(x, function(y) y$`CAS Number`)
}

#' @export
cas.wd_ident <- function(x, ...) {
  x$cas
}



# InChIKey ----------------------------------------------------------------
#' @rdname extractors
#' @export
inchikey <- function(x, ...){
  UseMethod("inchikey")
}
#' @export
inchikey.default <- function(x, ...) {
  sapply(x, function(y) y$inchikey)
}
#' @export
inchikey.cs_compinfo <- function(x, ...) {
  x$inchikey
}

#' @export
inchikey.cs_extcompinfo <- function(x, ...) {
  x$inchikey
}

#' @export
inchikey.etox_basic <- function(x, ...) {
  stop("InChIkey is not returned by this datasource!")
}
#' @export
inchikey.pan_query <- function(x, ...) {
  stop("InChIkey is not returned by this datasource!")
}


#' @export
inchikey.opsin_query <- function(x, ...) {
  x$stdinchikey
}

#' @export
inchikey.pc_prop <- function(x, ...) {
  if (!"InChIKey" %in% names(x)) {
    stop("InChIKey not queried!")
  }
  x$InChIKey
}

#' @export
inchikey.wd_ident <- function(x, ...) {
  x$inchikey
}


# SMILES ------------------------------------------------------------------
#' @rdname extractors
#' @export
smiles <- function(x, ...){
  UseMethod("smiles")
}
#' @export
smiles.default <- function(x, ...) {
  sapply(x, function(y) y$smiles)
}

#' @export
smiles.cs_compinfo <- function(x, ...) {
  x$smiles
}

#' @export
smiles.cs_extcompinfo <- function(x, ...) {
  x$smiles
}


#' @export
smiles.cts_compinfo <- function(x, ...) {
  stop("SMILES is not returned by this datasource!")
}

#' @export
smiles.etox_basic <- function(x, ...) {
  stop("InChIkey is not returned by this datasource!")
}
#' @export
smiles.pan_query <- function(x, ...) {
  stop("SMILES is not returned by this datasource!")
}

#' @export
smiles.opsin_query <- function(x, ...) {
  x$smiles
}
#' @export
smiles.aw_query <- function(x, ...) {
  stop("SMILES is not returned by this datasource!")
}

#' @export
smiles.pc_prop <- function(x, ...) {
  if (!"CanonicalSMILES" %in% names(x)) {
    stop("CanonicalSMILES not queried!")
  }
  x$CanonicalSMILES
}

#' @export
smiles.wd_ident <- function(x, ...) {
  x$smiles
}

