#' Check if input has correct inchikey format
#'
#' @param x character; input string
#' @return a logical
#'
#' @export
#' @examples
#' is.inchikey('BQJCRHHNABKAKU-KBQPJGBKSA-N')
#' is.inchikey('BQJCRHHNABKAKU-KBQPJGBKS')
is.inchikey = function(x) {
  (x == toupper(x)) &
    (nchar(x) == 27) &
    (substr(x, 15, 15) == "-") &
    (substr(x, 26, 26) == "-")
}


#' Extract number from string
#' @param x character; input string
#' @return a numeric vector
#' @export
#' @examples
extr_num <- function(x) {
  as.numeric(gsub("[^0-9\\-]+", "", x))
}