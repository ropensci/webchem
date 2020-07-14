#' Retrieve data from the Web-services Toxicity Estimation Software Tool (WebTEST)
#'
#' Queries WebTEST, a Web-Services Program to Estimate Toxicity
#' from Molecular Structure. \url{https://www.epa.gov/sites/production/files/2018-08/documents/webtest_users_guide.pdf}.
#'
#' @import httr dplyr
#' @importFrom stats rgamma
#'
#' @param query character; A SMILES string.
#' @param from character; Query identifier. Can be one of: "smiles".
#' @param endpoint character; Which predicted endpoint should be returned?
#' Can be one of "LC50" (Pimephales promelas 96 hr), "LC50DM" (Daphnia magna 48h)
#' "IGC50" (Tetrahymena pyriformis 48 hr), "LD50" (Oral rat),
#' "BCF" (Bioaccumulation factor), "DevTox" (Developmental Toxicity),
#' "Mutagenicity", "BP" (Normal boiling point), "VP" (Vapor pressure at 25°C),
#' "MP" (Melting point), "Density" (Flash point), "FP" (Density),
#' "ST" (Surface tension at 25°C), "TC" (Thermal conductivity at 25°C),
#' "Viscosity" (Viscosity at 25°C) and "WS" (Water solubility at 25°C).
#' For detail see
#' \url{https://www.epa.gov/sites/production/files/2018-08/documents/webtest_users_guide.pdf}.
#' @param method character; Which prediction method should be used? Can be one
#' of "consensus" (default), "hc" (hierachical clustering), "sm" (single model),
#' "nn" (nearest neighbor), "gc" (Group contribution). For detail see
#' \url{https://www.epa.gov/sites/production/files/2018-08/documents/webtest_users_guide.pdf}.
#' @param verbose logical; should a verbose output be printed on the console?
#'
#' @return data.frame
#'
#' @author Andreas Scharmueller, \email{andschar@@protonmail.com}
#'
#' @export
#' @examples
#' \dontrun{
#' # might fail if web site is not available
#' webtest_query("Clc2cc(Cl)ccc2Oc1ccc(Cl)cc1O")
#' webtest_query("Clc2cc(Cl)ccc2Oc1ccc(Cl)cc1O",
#'               endpoint = "LC50")
#' webtest_query("Clc2cc(Cl)ccc2Oc1ccc(Cl)cc1O",
#'               endpoint = "LC50DM", method = "nn")
#'
#' # multiple inputs
#' query = c("CCO", "O=C(O)CNCP(=O)(O)O", "blub")
#' webtest_query(query)
#' }
#'
webtest_query <- function(query,
                          from = "smiles",
                          endpoint = c("LC50",
                                       "LC50DM",
                                       "IGC50",
                                       "LD50",
                                       "BCF",
                                       "DevTox",
                                       "Mutagenicity",
                                       "BP",
                                       "VP",
                                       "MP",
                                       "Density",
                                       "FP",
                                       "ST",
                                       "TC",
                                       "Viscosity",
                                       "WS"),
                          method = c("consensus",
                                     "hc",
                                     "sm",
                                     "nn",
                                     "gc"),
                          verbose = TRUE) {
  # checks
  from <- match.arg(from)
  endpoint <- match.arg(endpoint, several.ok = TRUE)
  method <- match.arg(method)
  # vecorize
  foo <- function(query, from, endpoint, method, verbose) {
    # debuging
    # query = "CCO"; endpoint = "LC50"; from  = "smiles"; method = "hc"
    # url
    baseurl <- "https://comptox.epa.gov/dashboard/web-test"
    queryfrom <- paste0("?", from, "=", query)
    method <- paste0("method=", method)
    prolog <- paste(queryfrom, method, sep = "&")
    qurl <- file.path(baseurl, paste0(endpoint, prolog))
    # query
    if (verbose)
      message("Querying: ", query)
    Sys.sleep(rgamma(1, shape = 5, scale = 1/10))
    res <- try(
      httr::RETRY("GET", qurl), silent = TRUE
    )
    if (inherits(res, "try-error")) {
      message("No result found. Returning empty entry.")
      return(data.frame(query = query, stringsAsFactors = FALSE))
    }
    if (httr::status_code(res) != 200) {
      message("No result found. Returning empty entry.")
      return(data.frame(query = query, stringsAsFactors = FALSE))
    }
    cont <- httr::content(res)
    if (!is.null(cont$predictions[[1]]$error)) { # NB for non-parseable results
      message("No result found. Returning empty entry.")
      return(data.frame(query = query, stringsAsFactors = FALSE))
    }
    # prepare
    dat_meta <- dplyr::bind_rows(cont[ names(cont) != "predictions" ])
    dat_pred <- dplyr::bind_rows(cont[ names(cont) == "predictions" ][[1]])
    # return
    out <- dplyr::bind_cols(dat_meta, dat_pred)
    out$query <- query
    out
  }
  l <- lapply(query,
              foo,
              from = from,
              endpoint = endpoint,
              method = method,
              verbose = verbose)
  # return
  dplyr::select(dplyr::bind_rows(l),
                query, dplyr::everything())
}

