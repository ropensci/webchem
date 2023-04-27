#' Retrieve retention indices from NIST
#' @description This function scrapes NIST for literature retention indices
#'   given a query or vector of queries as input. The query can be a cas
#'   number, IUPAC name, or International Chemical Identifier (\code{inchikey}),
#'   according to the value of the \code{from} argument. Retention indices are
#'   stored in tables by \code{type}, \code{polarity} and temperature program
#'   (\code{temp_prog}). The function can take multiple arguments for these
#'   parameters and will return any retention times matching the specified
#'   criteria in a single table.
#'
#'   If a non-cas query is provided, the function will try to resolve the query
#'   by searching the NIST WebBook for a corresponding CAS number. If
#'   \code{from == "name"}, phonetic spellings of Greek stereo-descriptors
#'   (e.g. "alpha", "beta", "gamma") will be automatically
#'   converted to the corresponding letters to match the form used by NIST. If
#'   a CAS number is found, it will be returned in a \code{tibble} with the
#'   corresponding information from the NIST retention index database.
#'
#' @param query character; the search term
#' @param from character; type of search term. can be one of \code{"name"},
#'   \code{"inchi"}, \code{"inchikey"}, or \code{"cas"}. Using an identifier is
#'   preferred to \code{"name"} since \code{NA} is returned in the event of
#'   multiple matches to a query. Using an identifier other than a CAS number
#'   will cause this function to run slower as CAS numbers are used as internal
#'   identifiers by NIST.
#' @param type Retention index type: \code{"kovats"}, \code{"linear"},
#'   \code{"alkane"}, and/or \code{"lee"}. See details for more.
#' @param polarity Column polarity: \code{"polar"} and/or \code{"non-polar"}
#'   to get RIs calculated for polar or non-polar columns.
#' @param temp_prog Temperature program: \code{"isothermal"},
#'   \code{"ramp"}, and/or \code{"custom"}.
#' @param cas deprecated.  Use \code{query} instead.
#' @param verbose logical; should a verbose output be printed on the console?
#' @details The types of retention indices included in NIST include Kovats
#'   (\code{"kovats"}), Van den Dool and Kratz (\code{"linear"}), normal alkane
#'   (\code{"alkane"}), and Lee (\code{"lee"}). Details about how these are
#'   calculated are available on the NIST website:
#'   \url{https://webbook.nist.gov/chemistry/gc-ri/}
#' @importFrom purrr map
#' @importFrom purrr map_dfr
#' @import dplyr
#'
#' @return returns a tibble of literature RIs with the following columns:
#' \itemize{
#' \item{\code{query} is the query provided to the NIST server}
#' \item{\code{cas} is the CAS number or unique record identified used by NIST}
#' \item{\code{RI} is retention index}
#' \item{\code{type} is the type of RI (e.g. "kovats", "linear", "alkane", or "lee")}
#' \item{\code{polarity} is the polarity of the column (either "polar" or "non-polar")}
#' \item{\code{temp_prog} is the type of temperature program (e.g. "isothermal", "ramp", or "custom")}
#' \item{\code{column} is the column type, e.g. "capillary"}
#' \item{\code{phase} is the stationary phase (column phase)}
#' \item{\code{length} is column length in meters}
#' \item{\code{gas} is the carrier gas used}
#' \item{\code{substrate}}
#' \item{\code{diameter} is the column diameter in mm}
#' \item{\code{thickness} is the phase thickness in µm}
#' \item{\code{program}. various columns depending on the value of
#' \code{temp_prog}}
#' \item{\code{reference} is where this retention index was published}
#' \item{\code{comment}. I believe this denotes the database these data
#'       were aggregated from}
#'}
#'
#' @references NIST Mass Spectrometry Data Center, William E. Wallace, director,
#'   "Retention Indices" in NIST Chemistry WebBook, NIST Standard Reference
#'   Database Number 69, Eds. P.J. Linstrom and W.G. Mallard,
#'   National Institute of Standards and Technology, Gaithersburg MD, 20899,
#'   \doi{10.18434/T4D303}.
#'
#' @export
#' @note Copyright for NIST Standard Reference Data is governed by the Standard
#' Reference Data Act, \url{https://www.nist.gov/srd/public-law}.
#' @seealso \code{\link{is.cas}} \code{\link{as.cas}}
#'
#' @examples
#' \dontrun{
#' myRIs <-
#'   nist_ri(
#'     c("78-70-6", "13474-59-4"),
#'     from = "cas",
#'     type = c("linear", "kovats"),
#'     polarity = "non-polar",
#'     temp_prog = "ramp"
#'   )
#' myRIs}
nist_ri <- function(query,
                    from = c("cas", "inchi", "inchikey", "name"),
                    type = c("kovats", "linear", "alkane", "lee"),
                    polarity = c("polar", "non-polar"),
                    temp_prog = c("isothermal", "ramp", "custom"),
                    cas = NULL,
                    verbose = getOption("verbose")) {

  if (!is.null(cas)) {
    warning("`cas` is deprecated.  Using `query` instead with `from = 'cas'`.")
    query <- cas
    from <- "cas"
  }

  from <- match.arg(from)
  type <- match.arg(type, c("kovats", "linear", "alkane", "lee"), several.ok = TRUE)
  polarity <- match.arg(polarity, c("polar", "non-polar"), several.ok = TRUE)
  temp_prog <- match.arg(temp_prog, c("isothermal", "ramp", "custom"), several.ok = TRUE)

  if (from == "cas"){
    query <- sapply(query, function(x){
      cas <- as.cas(gsub("C","", x))
      ifelse(is.cas(cas) & !is.na(cas), cas, x)
    })
  } else if (from == "name"){
    query <- format_name_nist(query)
  }

  ri_tables <-
    purrr::map(
      query,
      ~ get_ri_xml(
        query = .x,
        from = from,
        type = type,
        polarity = polarity,
        temp_prog = temp_prog,
        verbose = verbose
      )
    )

  querynames <- query
  querynames [is.na(querynames)] <- ".NA"
  names(ri_tables) <- querynames

  ri_tables <- dplyr::bind_rows(ri_tables)
  return(ri_tables)
}

#' Scrape Retention Indices from NIST
#'
#' @param query query of type matching the options in `from`
#' @param from one of "name", "cas", "inchi", or "inchikey"
#' @param type what kind of RI
#' @param polarity polar or non-polar
#' @param temp_prog what kind of temperature program
#' @param verbose logical; should a verbose output be printed on the console?
#' @noRd
#' @import rvest
#' @import xml2
#' @importFrom utils URLencode
#' @return an xml nodeset
#'
get_ri_xml <-
  function(query,
           from,
           type,
           polarity,
           temp_prog,
           verbose) {

    if (!ping_service("nist")) stop(webchem_message("service_down"))
    cas_found <- FALSE
    from_str <- (switch(
      from,
      "name" = "Name",
      "inchi" = "InChI",
      "inchikey" = "InChI",
      "cas" = "ID"
    ))

    baseurl <- "https://webbook.nist.gov/cgi/cbook.cgi"

    #handle NAs
    if (is.na(query)) {
      if (verbose) webchem_message("na")
      return(dplyr::tibble(query=NA))
    }
    if (from == "cas"){
      ID <- paste0("C", gsub("-", "", query))
    }
    # check for existence of record
      qurl <- URLencode(paste0(baseurl, "?", from_str, "=", gsub(" ","+", query), "&Units=SI"))
      webchem_sleep(type = 'scrape')
      if (verbose) webchem_message("query", query, appendLF = FALSE)
      res <- try_url(qurl)
      if (inherits(res, "try-error")) {
        if (verbose) webchem_message("service_down")
        return(dplyr::tibble(query=NA))
      }
      if (verbose) message(httr::message_for_status(res))
      if (res$status_code == 200) {
        page <- xml2::read_html(res)
        #Warnings
        result <- page %>%
          rvest::html_node("main h1") %>%
          rvest::html_text()
        # if query not found
        if (stringr::str_detect(result, "Not Found")) {
          if (verbose) webchem_message("not_found", appendLF = FALSE)
          ri_xml <- construct_NA_table(query)
        } else if (result == "Search Results") {
          # if more than one compound found
          if (verbose){
          message(paste0(" More than one match for '", query,
                         "'. Returning NA."))
          }
          ri_xml <- construct_NA_table(query)
        } else{
        links <-
          page %>%
          rvest::html_nodes("li li a") %>%
          rvest::html_attr("href")

        gaschrom <- links[which(regexpr("Gas-Chrom", links) >= 1)]

        if (length(gaschrom) == 0) {
          if (verbose) webchem_message("not_available")
          cas <- page %>%
            rvest::html_nodes("li") %>%
            grep("CAS Registry", ., value=TRUE) %>%
            gsub("<li>\n<strong>CAS Registry Number:</strong> |</li>", "",.)
          ri_xml <- construct_NA_table(query, cas=ifelse(length(cas) > 0, cas, NA))
        } else {
          if (verbose) message(" CAS found. ", appendLF = FALSE)
          ID <- stringr::str_extract(gaschrom, "(?<=ID=).+?(?=&)")
          cas_found <- TRUE
        }
      }
    } else {
        return(dplyr::tibble(query=NA))
      }
    if (cas_found){
      # check which records exist
      check_records <- function(gaschrom, type, polarity, temp_prog, verbose){
        if (any(sapply(list(type, polarity, temp_prog), length) > 1)){
          res2 <- try_url(paste0("https://webbook.nist.gov", gaschrom))
          if (inherits(res2, "try-error")) {
            if (verbose) webchem_message("service_down")
            return(dplyr::tibble(query=NA))
          }
          page <- xml2::read_html(res2)
          tables <- page %>% html_nodes(".data") %>% html_attr("aria-label")
          tables <- format_gaschrom_tables(tables)
          tables.idx <- which(tables[,1] %in% type &
                                tables[,2] %in% polarity &
                                tables[,3] %in% temp_prog)
          tables <- tables[tables.idx,,drop=FALSE]
        } else {
          tables <- data.frame(type, polarity, temp_prog)
        }
        tables
      }

      tables <- check_records(gaschrom, type, polarity, temp_prog, verbose)
      if (nrow(tables) == 0){
        ri_xml <- construct_NA_table(query, cas = ID)
      } else{
        ri_xml <- lapply(seq_len(nrow(tables)), function(i){
          ri_xml <- scrape_RI_table(ID, type=tables[i,1], polarity = tables[i,2],
                          temp_prog = tables[i,3], from = from, verbose = verbose,
                          cas_found = cas_found)
          attr(ri_xml, "query") <- query
          ri_xml
        })
      }
    }
      purrr::map_dfr(ri_xml, tidy_ritable)
  }

#' @noRd
construct_NA_table <- function(query, cas=NA){
  x <- NA
  attr(x,"query") <- query
  attr(x,"cas") <- as.cas(gsub("C", "", cas))
  list(x)
}

#' @noRd
scrape_RI_table <- function(ID, type, polarity, temp_prog,
                            from, verbose, cas_found){
    #scrape RI table
    type_str <- toupper(paste(type, "RI", polarity, temp_prog, sep = "-"))
    baseurl <- "https://webbook.nist.gov/cgi/cbook.cgi"
    qurl <- paste0(baseurl, "?ID=", ID, "&Units-SI&Mask=2000&Type=", type_str)
    webchem_sleep(type = 'scrape')
    if (verbose) webchem_message("query", ID, appendLF = FALSE)
    res2 <- try_url(qurl)
    if (inherits(res2, "try-error")) {
      if (verbose) webchem_message("service_down")
      return(dplyr::tibble(query=NA))
    }
    if (verbose) message(httr::message_for_status(res2))
    if (res2$status_code == 200) {
      page <- xml2::read_html(res2)
      ri_xml.all <- html_nodes(page, ".data")

      #message if table doesn't exist at URL
      if (length(ri_xml.all) == 0) {
        if (verbose) webchem_message("not_available")
        ri_xml <- NA
      } else {
        ri_xml <- ri_xml.all
        cas_found <- TRUE
      }
    } else {
      return(dplyr::tibble(query=NA))
    }
  #set attributes to label what type of RI
  attr(ri_xml, "from") <- from
  attr(ri_xml, "type") <- type
  attr(ri_xml, "polarity") <- polarity
  attr(ri_xml, "temp_prog") <- temp_prog
  attr(ri_xml, "cas") <- ifelse(cas_found, as.cas(gsub("C","",ID)), NA)
  return(ri_xml)
}


#' Tidier for webscraped RI ri_xml
#'
#' @param ri_xml captured by \code{get_ri_xml}
#'
#' @import rvest
#' @importFrom purrr map
#' @importFrom purrr map_dfr
#' @importFrom tibble as_tibble
#' @import dplyr
#' @noRd
#' @return a single table
#'
tidy_ritable <- function(ri_xml) {
  #Skip all these steps if the table didn't exist at the URL and was set to NA
  cas <- attr(ri_xml, "cas")
  if (any(is.na(ri_xml))) {
    return(tibble(query = attr(ri_xml, "query"),
                  cas = ifelse(is.null(cas), NA, cas), RI = NA))
  } else {
    # Read in the tables from xml
    table.list <- purrr::map(ri_xml, html_table)
    # Transpose and tidy
    tidy1 <- purrr::map_dfr(
      table.list,
      ~{
        transposed <- t(.x)
        colnames(transposed) <- transposed[1, ]
        transposed[-1, , drop = FALSE] %>%
          as_tibble()
      }
    )

    tidy2 <- dplyr::select(tidy1,
                           "RI" = "I",
                           "column" = "Column type",
                           "phase" = "Active phase",
                           "length" = "Column length (m)",
                           "gas" = "Carrier gas",
                           "substrate" = "Substrate",
                           "diameter" = "Column diameter (mm)",
                           "thickness" = "Phase thickness (\u03bcm)",
                           "program" = contains("Program"),
                           "temp" = contains("Temperature (C)"),
                           "temp_start" = contains("Tstart (C)"),
                           "temp_end" = contains("Tend (C)"),
                           "temp_rate" = contains("Heat rate (K/min)"),
                           "hold_start" = contains("Initial hold (min)"),
                           "hold_end" = contains("Final hold (min)"),
                           "reference" = "Reference",
                           "comment" = "Comment") %>%
  dplyr::mutate_at(vars(any_of(c("length", "diameter", "thickness", "temp",
                            "temp_start","temp_end","temp_rate",
                            "hold_start","hold_end", "RI"))), as.numeric)

    # make NAs explicit and gas abbreviations consistent
    output <- tidy2 %>%
      mutate(across(where(is.character), ~na_if(., ""))) %>%
      dplyr::mutate(
        gas = case_when(
          stringr::str_detect(gas, "He") ~ "Helium",
          stringr::str_detect(gas, "H2") ~ "Hydrogen",
          stringr::str_detect(gas, "N2") ~ "Nitrogen",
          TRUE                  ~ as.character(NA)
        )
      ) %>%
      dplyr::mutate(query = attr(ri_xml, "query"),
                    cas = attr(ri_xml, "cas"),
                    type = attr(ri_xml, "type"),
                    polarity = attr(ri_xml, "polarity"),
                    temp_prog = attr(ri_xml, "temp_prog"),) %>%
      # reorder columns
      dplyr::select("query","cas", "type", "polarity", "temp_prog", "RI", "type",
                    "phase", everything())
  }
  return(output)
}

#' Format chemical names into NIST format
#'
#' NIST uses Greek letters to distinguish stereoisomers, but users may have
#' strings that spell out the letters in Latin script. This is utility
#' function to convert chemical names to NIST format.
#' @param x character; a IUPAC chemical name
#' @return a character
#' @examples
#' format_name_nist("alpha-bergatomene")
#' format_name_nist("beta-bisabolene")
#' @noRd
format_name_nist <- function(x) {
  format_name <- function(x){
    # format name
    dictionary <- c("[^a-zA-Z]alpha[^a-zA-Z]" = "-\\u03b1-",
                    "^alpha[^a-zA-Z]" = "\\u03b1-",
                    "[^a-zA-Z]alpha$" = "-\\u03b1",
                    "[^a-zA-Z]beta[^a-zA-Z]" = "-\\u03b2-",
                    "^beta[^a-zA-Z]" = "\\u03b2-",
                    "[^a-zA-Z]beta$" = "-\\u03b2",
                    "[^a-zA-Z]gamma[^a-zA-Z]" = "-\\u03b3-",
                    "^gamma[^a-zA-Z]" = "\\u03b3-",
                    "[^a-zA-Z]gamma$" = "-\\u03b3",
                    "[^a-zA-Z]delta[^a-zA-Z]" = "-\\U0001d6ff-",
                    "^delta[^a-zA-Z]" = "\\U0001d6ff-",
                    "[^a-zA-Z]delta$" = "-\\U0001d6ff")
    x <- stringr::str_replace_all(x, dictionary)
    x
  }
  sapply(x, format_name)
}

#' @noRd
try_url <- function(qurl){
  try(httr::RETRY("GET",
                qurl,
                httr::user_agent(webchem_url()),
                terminate_on = 404,
                quiet = TRUE), silent = TRUE)
}

#' @noRd
format_gaschrom_tables <- function(tables){
  tables <- stringr::str_split_fixed(tables,",",3)

  tables[,1] <- stringr::str_replace_all(tables[,1],
                                         pattern = c("Kovats' RI" = "kovats",
                                                     "Normal alkane RI" = "alkane",
                                                     "Van Den Dool and Kratz RI" = "linear",
                                                     "Lee's RI" = "lee")
  )
  tables[,2] <- stringr::str_split_fixed(tables[,2], " ", 3)[,2]
  tables[,3] <- stringr::str_replace_all(tables[,3],
                                         pattern = c(" isothermal" = "isothermal",
                                                     " custom temperature program" = "custom",
                                                     " temperature ramp" = "ramp")
  )
  tables
}
