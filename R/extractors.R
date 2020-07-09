#' Extract parts from webchem objects
#' @name extractors
#' @rdname extractors
#' @param x object
#' @param ... currently not used.
#' @return a vector.
#' @references Eduard Szöcs, Tamás Stirling, Eric R. Scott, Andreas Scharmüller,
#' Ralf B. Schäfer (2020). webchem: An R Package to Retrieve Chemical
#' Information from the Web. Journal of Statistical Software, 93(13).
#' <doi:10.18637/jss.v093.i13>.
#' @export
cas <- function(x, ...){
  UseMethod("cas")
}

# CAS ---------------------------------------------------------------------
#' @export
cas.default <- function(x, ...) {
  stop(paste("No cas method for class", class(x)))
}
#' @export
cas.chebi_comp_entity <- function(x, ...) {
  sapply(x, function(y) {
    if (length(y) == 1 && is.na(y)) return(NA)
    unique(y$regnumbers$data[y$regnumbers$type == "CAS Registry Number"])
  })
}

#' @export
cas.opsin_query <- function(x, ...) {
  stop("CAS is not returned by this datasource!")
}

#' @export
cas.pan_query <- function(x, ...) {
  sapply(x, function(y) y$`CAS Number`)
}

#' @export
cas.aw_query <- function(x, ...) {
  sapply(x, function(y) y$cas)
}

#' @export
cas.wd_ident <- function(x, ...) {
  x$cas
}


#' @export
cas.cts_compinfo <- function(x, ...) {
  stop("CAS is not returned by this data source")
}

#' @export
cas.etox_basic <- function(x, ...) {
  sapply(x, function(y) {
    if (length(y) == 1 && is.na(y))
      return(NA)
    unique(y$cas)
  })
}

#' @export
cas.ci_query <- function(x, ...) {
  sapply(x, function(y) {
    if (length(y) == 1 && is.na(y))
      return(NA)
    unique(y$cas)
  })
}

# InChIKey ----------------------------------------------------------------
#' @rdname extractors
#' @export
inchikey <- function(x, ...){
  UseMethod("inchikey")
}

#' @export
inchikey.default <- function(x, ...) {
  stop(paste(" No inchikey method for class", class(x)))
}

#' @export
inchikey.aw_query <- function(x, ...) {
  sapply(x, function(y) {
    if (length(y) == 1 && is.na(y)) return(NA)
    y$inchikey
  })
}

#' @export
inchikey.chebi_comp_entity <- function(x, ...) {
  sapply(x, function (y) {
    if (length(y) == 1 && is.na(y)) return(NA)
    y$properties$inchikey
  })
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

#' @export
inchikey.cts_compinfo <- function(x, ...) {
  sapply(x, function(x) x$inchikey)
}

#' @export
inchikey.ci_query <- function(x, ...) {
  sapply(x, function(y) {
    if (length(y) == 1 && is.na(y))
      return(NA)
    unique(y$inchikey)
  })
}

# SMILES ------------------------------------------------------------------
#' @rdname extractors
#' @export
smiles <- function(x, ...){
  UseMethod("smiles")
}

#' @export
smiles.default <- function(x, ...) {
  stop(paste("no smiles method for class", class(x)))
}
#' @export
smiles.chebi_comp_entity <- function(x, ...) {
  sapply(x, function(y) {
    if (length(y) == 1 && is.na(y)) return(NA)
    y$properties$smiles
  })
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

#' @export
smiles.ci_query <- function(x, ...) {
  sapply(x, function(y) {
    if (length(y) == 1 && is.na(y))
      return(NA)
    unique(y$smiles)
  })
}
