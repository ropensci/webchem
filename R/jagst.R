#' Organic plant protection products in the river Jagst / Germany in 2013
#'
#' This dataset comprises environmental monitoring data of organic plant protection products
#' in the year 2013 in the river Jagst, Germany.
#' The data is publicly available and can be retrieved from the
#' LUBW Landesanstalt für Umwelt, Messungen und Naturschutz Baden-Württemberg.
#' It has been preprocessed and comprises measurements of 34 substances.
#' Substances without detects have been removed.
#' on 13 sampling occasions.
#' Values are given in ug/L.
#'
#' @format A data frame with 442 rows and 4 variables:
#' \describe{
#'   \item{date}{sampling data}
#'   \item{substance}{substance names}
#'   \item{value}{concentration in ug/L}
#'   \item{qual}{qualifier, indicating values < LOQ}
#' }
#' @source \url{https://udo.lubw.baden-wuerttemberg.de/public/}
"jagst"
