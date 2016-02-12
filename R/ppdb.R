#' Parse a HTML source from PPDB.
#'
#' This function parses a (substance) html from the website into an R object.
#' Earlier versions allowed also to search and download the database.
#' However, this is explicitly against the terms and conditions of use [link removed on request].
#' On request we also removed all links to the website / database.
#'
#' @param source an object of class \code{xml_document} as returned by  \code{\link[xml2]{read_html}}.
#' @param verbose logical; print message during processing to console?
#' @return A list of 11 data.frames : ec_regulation, approved_in, general, parents, fate,
#' deg, soil, metab, etox, names and source_url.
#'
#' @note Please read the Terms and Conditions for use [link removed on request] and
#' the Copyright statement [link removed on request].
#'
#' This function only parses a html. Saving (or downloading) substantial parts
#' from the database is explicitly against the terms and conditions and copyright of use
#' [link removed on request].
#'
#' @author Eduard Szoecs, \email{eduardszoecs@@gmail.com}
#' @references [Reference removed on request.]
#' @export
ppdb_parse <- function(source, verbose = TRUE){
  ttt <- source

  chk <- function(x){
    if (inherits(x, 'try-error'))
      return(NA)
    return(x)
  }

  # ec regulation
  ec_regulation <- html_table(
    xml_find_all(ttt, "//p[contains(.,'EC Reg')]/following-sibling::table[1]"),
    header = TRUE)[[1]]

  # approved in contries
  status <- xml_attr(xml_find_all(ttt, "//*[contains(.,'Approved')]/following-sibling::table[1]/tr[2]/td/p/img"), 'src')
  approved_id <- data.frame(t(html_table(
    xml_find_all(ttt, "//p[contains(.,'Approved for use')]/following-sibling::table[1]"),
    header = TRUE)[[1]]), stringsAsFactors = FALSE)
  approved_id$status <- ifelse(status == 'tick.jpg', TRUE, FALSE)
  approved_id[ , 1] <- NULL
  approved_id$country <- rownames(approved_id)

  # general status
  general <- html_table(
    xml_find_all(ttt, "//p[contains(.,'General status')]/following-sibling::table[1]"),
    header = FALSE, fill = TRUE)[[1]]
  general <- general[ , 1:2]
  names(general) <- c('variable', 'value')

  # parents
  parents <- try(html_table(
    xml_find_all(ttt,  "//p[contains(.,'Can be a metabolite of')]/following-sibling::table[1]"),
    header = TRUE)[[1]],
    silent = TRUE)
  parents <- chk(parents)

#   # formulations
#   formulation <- tables[[8]]
#   names(formulation) <- c('variable', 'value')

  # fate
  fate <- try(html_table(
    xml_find_all(ttt,   "//p[contains(.,'ENVIRONMENTAL FATE')]/following-sibling::table[1]"),
    header = TRUE, fill = TRUE)[[1]],
    silent = TRUE)
  fate <- chk(fate)
  if (!length(fate) == 0) {
    take <- fate[!is.na(fate[ , 5]), ]
    take[ , 1] <- paste(take[ , 1], take[ , 2])
    take[ , 2] <- NULL
    fate <- fate[is.na(fate[ , 5]), 1:4]
    nam <- c('Property', 'Value', 'Source/Quality', 'Interpretation')
    colnames(fate) <- nam
    colnames(take) <- nam
    fate <- rbind(fate, take)
    fate <- fate[!grepl('Note', fate[ , 'Property']), ]
    if (any(is.na(fate[ , 'Interpretation']))) {
      take <- fate[is.na(fate[ , 'Interpretation']), ]
      corr <- which(is.na(fate[ , 'Interpretation']))[1] - 1
      take <- data.frame(Prop = NA, take[ , c(1,2,3)])
      take[ , 1] <- fate[corr, 'Property']
      colnames(take) <- nam
      fate <- fate[!is.na(fate[ , 'Interpretation']), ]
      fate <- rbind(fate, take)
    }
  }

  # degredation
  deg <- try(html_table(
    xml_find_all(ttt,   "//p[contains(.,'Degradation')]/following-sibling::table[1]"),
    header = TRUE, fill = TRUE)[[1]],
    silent = TRUE)
  deg <- chk(deg)
  if (!length(deg) == 0) {
    take <- deg[!is.na(deg[ , 5]), ]
    take[ , 1] <- paste(take[ , 1], take[ , 2])
    take[ , 2] <- NULL
    deg <- deg[is.na(deg[ , 5]), 1:4]
    nam <- c('Property', 'Value', 'Source/Quality', 'Interpretation')
    colnames(deg) <- nam
    colnames(take) <- nam
    deg <- rbind(deg, take)
    deg <- deg[!grepl('Note', deg[ , 'Property']), ]
    # if (any(deg[ , 'Value'] == 'Value')) {
    #   take <- deg[deg[ , 'Value'] == 'Value', ]
    #   take <- data.frame(take[ , -2], N = NA)
    #   colnames(take) <- colnames(deg)
    #   deg <- deg[!deg[ , 'Value'] == 'Value', ]
    #   deg <- rbind(deg, take)
    # }
  }

  # soil adsorption and mobility
  soil <- try(html_table(
    xml_find_all(ttt,   "//p[contains(.,'Soil adsorption')]/following-sibling::table[1]"),
    header = TRUE, fill = TRUE)[[1]],
    silent = TRUE)
  soil <- chk(soil)
  if (!length(soil) == 0) {
    nam <- c('Property', 'Value', 'Source/Quality', 'Interpretation')
    colnames(soil) <- nam
    soil <- soil[!grepl('Note', soil[ , 'Property']), ]
    corr <- soil[!is.na(soil[,5]), 1]
    soil[!is.na(soil[ , 5]), 1] <- paste(soil[!is.na(soil[ , 5]), 1], soil[!is.na(soil[ , 5]), 2])
    soil[!is.na(soil[ , 5]), ] <- c(soil[!is.na(soil[ , 5]), c(1, 3, 4, 5)], NA)
    soil[ , 5] <- NULL
  }

  # metabolites
  metab <- try(html_table(
    xml_find_all(ttt,   "//p[contains(.,'Key metabolites')]/following-sibling::table[1]"),
    header = TRUE, fill = TRUE)[[1]],
    silent = TRUE)
  metab <- chk(metab)

  # ecotoxicology
  etox <- try(html_table(
    xml_find_all(ttt,   "//p[contains(.,'ECOTOXICOLOGY')]/following-sibling::table[1]"),
    header = TRUE, fill = TRUE)[[1]],
    silent = TRUE)
  etox <- chk(etox)
  if (!length(etox) == 0) {
    nam <- c('Property', 'Value', 'Source/Quality', 'Interpretation')
    colnames(etox) <- nam
    etox <- etox[!grepl('Note', etox[ , 'Property']), ]
    corr <- etox[!is.na(etox[ , 5]), 1]
    etox[!is.na(etox[ , 5]), 1] <- paste(etox[!is.na(etox[ , 5]), 1], etox[!is.na(etox[ , 5]), 2])
    etox[!is.na(etox[ , 5]), ] <- c(etox[!is.na(etox[ , 5]), c(1, 3, 4, 5)], NA)
    etox[ , 5] <- NULL
  }

  # names
  names <- try(html_table(
    xml_find_all(ttt,   "//p[contains(.,'TRANSLATIONS ')]/following-sibling::table[1]"),
    header = TRUE, fill = TRUE)[[1]],
    silent = TRUE)
  names <- chk(names)

  out <- list(ec_regulation = ec_regulation,
       approved_in = approved_id,
       general = general,
       parents = parents,
       # formulation = formulation,
       fate = fate,
       deg = deg,
       soil = soil,
       metab = metab,
       etox = etox,
       names = names
       )
  return(out)
}