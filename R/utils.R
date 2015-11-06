#' Check if input has correct inchikey format
#'
#' @param x input string
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
