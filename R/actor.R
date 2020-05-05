#' Retrieve data from AcTOR
#'
#' @description The function allows to search the the
#' Aggregated Computational Toxicology Online Resource (AcTOR), which is the
#' warehouse for different US EPA web applications: \url{https://actor.epa.gov}.
#' Only the top most parameters (e.g. dsstox id, inchi) are queried because
#' more advanced parameters (e.g. Hazard, Acute Toxicity, Use) only represent
#' a loose collection of un-structure data. It is recommended to look these up
#' manually. Entries can only be queried by a valid CASNR.
#'
#' @import httr xml2
#'
#' @param query character; search term.
#' @param from character; type of input. Only "cas".
#' @param verbose logical; should a verbose output be printed on the console?
#'
#' @references \url{https://actor.epa.gov}
#' @author Andreas Scharmueller, \email{andschar@@protonmail.com}
#' @return data.frame tibble
#' @export
#' @examples
#' \donttest{
#' # might fail if web site is not available
#' actor("1071-83-6")
#'
#' # multiple inputs
#' comp <- c("1071-83-6", "50-00-0")
#' actor(comp)
#'
#' }
#'
actor <- function(query,
                  from = "cas",
                  verbose = TRUE) {
  # checking
  from <- match.arg(from)
  foo <- function(query,
                  from,
                  verbose) {
    # url
    baseurl <- "https://actor.epa.gov/actor/chemical.xhtml"
    prolog <- "?casrn="
    qurl <- paste0(baseurl, prolog, query)
    # query
    if (verbose)
      message("Querying: ", query)
    site <- try(read_html(qurl))
    if (inherits(site, "try-error")) {
      message("Error. Returning NA.")
      out <- data.frame(query = query,
                        stringsAsFactors = FALSE)
    }
    # prepare
    chemical_name <- trimws(xml_text(xml_nodes(site, ".chemicalNameFont")))
    cas_dsstox <- xml_nodes(site, "#dsstoxSubstanceIdContainerId")
    cas <- trimws(xml_text(xml_node(cas_dsstox, "#casrnId")))
    dsstox <- trimws(xml_text(xml_child(cas_dsstox[[1]], 3))) # error prone
    inchi <- trimws(xml_text(xml_node(site, "#inchiContainerId")))
    inchi <- trimws(sub("InChi: InChI=", "", inchi, fixed = TRUE))
    inchikey <- trimws(xml_text(xml_node(site, "#inchiKeyContainerId")))
    inchikey <- trimws(sub("InChi Key:", "", inchikey))
    formula <- trimws(xml_text(xml_node(site, "#molFormulaContainerId")))
    formula <- trimws(sub("Molecular Formula:", "", formula))
    molecularweight <- trimws(xml_text(xml_node(site, "#molWeightContainerId")))
    molecularweight <- trimws(sub("Molecular Weight:", "", molecularweight))
    # out
    if (cas == "") {
      # result check
      message("No result found. Retuning NA.")
      out <- data.frame(query = query,
                        stringsAsFactors = FALSE)
    } else {
      out <- data.frame(query = query,
                        chemical_name = chemical_name,
                        cas = cas[1],
                        dsstox = dsstox[1],
                        inchi = inchi[1],
                        inchikey = inchikey[1],
                        formula = formula[1],
                        molecularweight = molecularweight[1],
                        stringsAsFactors = FALSE)
    }
  }
  l <- lapply(query, foo, from = from, verbose = verbose)
  dplyr::bind_rows(l)
}

#' Retrieve chemical structure images from AcTOR
#'
#' @description The function to retrieve images from the
#' Aggregated Computational Toxicology Online Resource (AcTOR), which is the
#' warehouse for different US EPA web applications: \url{https://actor.epa.gov}.
#'
#' @import curl
#'
#' @param query character; search term.
#' @param from character; type of input. Only "cas".
#' @param dir character; Directory to store the image.
#' @param format character; Image format. Can be on of ("svg", "png", "jpeg").
#' @param width integer; Image width in pixels.
#' @param height integer; Image height in pixels.
#' @param verbose logical; should a verbose output be printed on the console?
#'
#' @references \url{https://actor.epa.gov}
#' @author Andreas Scharmueller, \email{andschar@@protonmail.com}
#' @return disk file
#' @export
#' @examples
#' \donttest{
#' # might fail if web site is not available
#' actor_img("1071-83-6")
#'
#' # multiple inputs
#' comp <- c("1071-83-6", "50-00-0")
#' actor_img(comp)
#' }
#'
actor_img = function(query,
                     from = "cas",
                     dir = NULL,
                     format = c("svg", "png", "jpeg"),
                     width = 400,
                     height = 400,
                     verbose = TRUE) {
  # debuging
  # query = '1071-83-6'; from = "cas"; format = "svg";
  # width = 400; height = 400; dir = '/tmp'; verbose = TRUE
  # checking
  if (is.null(dir))
    stop('Please provide a dir.')
  format <- match.arg(format)
  foo <- function(query,
                  from,
                  dir,
                  format,
                  width,
                  height,
                  verbose) {
    # url
    baseurl <- "https://actorws.epa.gov/actorws/chemical/image"
    prolog <- "?casrn="
    qurl <- paste0(baseurl, prolog, query)
    fmt <- paste0("&fmt=", format)
    width <- paste0("&width=", width)
    height <- paste0("&height=", height)
    qurl <- paste0(qurl, width, height, fmt)
    # query
    if (verbose)
      message("Querying: ", query)
    fl <- paste0(query, ".", format)
    Sys.sleep(rgamma(1, shape = 5, scale = 1/10))
    tr <- try(
      suppressWarnings(
        curl::curl_download(qurl, destfile = file.path(dir, fl), quiet = TRUE)
      ),
      silent = TRUE)
    if (inherits(tr, 'try-error'))
      message("CAS not found. No file written.")
  }
  lapply(query, foo,
         from = from, dir = dir, format = format,
         width = width, height = height,
         verbose = verbose)
}

