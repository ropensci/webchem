#' Scrape Retention Indices from NIST
#'
#' @param query query of type matching the options in `from`
#' @param from one of "name", "cas", "inchi", or "inchikey"
#' @param type what kind of RI
#' @param polarity polar or non-polar
#' @param temp_prog what kind of temperature program
#' @noRd
#' @import rvest
#' @import xml2
#' @import polite
#' @importFrom rlang list2
#' @return an xml nodeset
#'
get_ri_xml <-
  function(query,
           from,
           type,
           polarity,
           temp_prog) {

    from_str <- (switch(
      from,
      "name" = "Name",
      "inchi" = "InChI",
      "inchikey" = "InChI",
      "cas" = "ID"
    ))

    # Open session
    #TODO: get rid of "no encoding supplied: defaulting to UTF-9" message correctly
    session <- suppressMessages(bow("https://webbook.nist.gov/cgi/cbook.cgi"))
    # Unclear if this actually randomizes or just sets a single value.  Should re-set everyt time this gets run though
    set_scrape_delay(rgamma(1, shape = 15, scale = 1/10))

    if (from == "cas") {
      ID <- paste0("C", gsub("-", "", query))
    } else {
      page <-
        scrape(session,
               query = list2(!!from_str := query,
                             Units = "SI"))
      #Warnings
      result <- page %>%
        html_node("main h1") %>%
        html_text()
      # if cquery not found
      if(str_detect(result, "Not Found")) {
        warning(paste0("'", query, "' not found. Returning NA."))
        return(as.data.frame(NA))
      }
      # if more than one compound found
      if(result == "Search Results") {
        warning(paste0("More than one match for '", query,
                "'. Returning NA."))
        return(as.data.frame(NA))
      }
      links <-
        page %>%
        html_nodes("li li a") %>%
        html_attr("href")

      gaschrom <- links[which(regexpr("Gas-Chrom", links) >= 1)]

      if (length(gaschrom) == 0) {
        warning(paste0(
          "There are no chromatography data for '",
          query,
          "'. Returning NA."
        ))
        # ri_xml <- as.data.frame(NA)
        return(as.data.frame(NA))
      } else {
        ID <- str_extract(gaschrom, "(?<=ID=).+?(?=&)")
      }
    }
        #scrape RI table
    type_str <-
      toupper(paste(type, "RI", polarity, temp_prog, sep = "-"))

    page <- scrape(session,
                   query = list(
                     ID = ID,
                     Units = "SI",
                     Mask = "2000",
                     Type = type_str
                   ))

    ri_xml.all <- html_nodes(page, ".data")

    #Warn if table doesn't exist at URL
    if (length(ri_xml.all) == 0) {
      warning(paste0(
        "There are no RIs for ",
        query,
        " of type ",
        type_str,
        ". Returning NA."
      ))
      ri_xml <- as.data.frame(NA) #TODO: is this the right thing?
    } else {
      ri_xml <- ri_xml.all
    }

    #set attributes to label what type of RI
    attr(ri_xml, "from") <- from
    attr(ri_xml, "type") <- type
    attr(ri_xml, "polarity") <- polarity
    attr(ri_xml, "temp_prog") <- temp_prog
    return(ri_xml)
  }


#' Tidier for webscraped RI ri_xml
#'
#' @param ri_xml captured by \code{get_ri_xml}
#'
#' @import rvest
#' @importFrom purrr map
#' @importFrom purrr map_dfr
#' @import dplyr
#' @noRd
#'
#' @return a single table
#'
tidy_ritable <- function(ri_xml) {
  #Skip all these steps if the table didn't exist at the URL and was set to NA
  if (any(is.na(ri_xml))) {
    output <- ri_xml

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

    # Extract the temperature program metadata
    temp_prog <- attr(ri_xml, "temp_prog")

    if (temp_prog == "custom") {
      tidy2 <- dplyr::rename(tidy1,
                      "type" = "Column type",
                      "phase" = "Active phase",
                      "length" = "Column length (m)",
                      "gas" = "Carrier gas",
                      "substrate" = "Substrate",
                      "diameter" = "Column diameter (mm)",
                      "thickness" = "Phase thickness (m)",
                      "program" = "Program",
                      "RI" = "I",
                      "reference" = "Reference",
                      "comment" = "Comment") %>%
        # fix column types and make uniform contents of some columns
        dplyr::mutate_at(vars("length", "diameter", "thickness", "RI"),
                  as.numeric)

    } else if (temp_prog == "ramp") {
      tidy2 <- dplyr::rename(tidy1,
                      "type" = "Column type",
                      "phase" = "Active phase",
                      "length" = "Column length (m)",
                      "gas" = "Carrier gas",
                      "substrate" = "Substrate",
                      "diameter" = "Column diameter (mm)",
                      "thickness" = "Phase thickness (m)",
                      "temp_start" = "Tstart (C)",
                      "temp_end" = "Tend (C)",
                      "temp_rate" = "Heat rate (K/min)",
                      "hold_start" = "Initial hold (min)",
                      "hold_end" = "Final hold (min)",
                      "RI" = "I",
                      "reference" = "Reference",
                      "comment" = "Comment") %>%
        # fix column types and make uniform contents of some columns
        dplyr::mutate_at(
          dplyr::vars(
            "length",
            "diameter",
            "thickness",
            "temp_start",
            "temp_end",
            "temp_rate",
            "hold_start",
            "hold_end",
            "RI"
          ),
          as.numeric
        )

    } else if (temp_prog == "isothermal") {
      tidy2 <- dplyr::rename(tidy1,
                      "type" = "Column type",
                      "phase" = "Active phase",
                      "length" = "Column length (m)",
                      "gas" = "Carrier gas",
                      "substrate" = "Substrate",
                      "diameter" = "Column diameter (mm)",
                      "thickness" = "Phase thickness (m)",
                      "temp" = "Temperature (C)",
                      "RI" = "I",
                      "reference" = "Reference",
                      "comment" = "Comment") %>%
        # fix column types and make uniform contents of some columns
        dplyr::mutate_at(vars("length", "diameter", "thickness", "temp",  "RI"),
                  as.numeric)
    }

    # make NAs explicit and gas abbreviations consistent
    output <- tidy2 %>%
      dplyr::mutate_all(~ na_if(., "")) %>%
      dplyr::mutate(
        gas = case_when(
          stringr::str_detect(gas, "He") ~ "Helium",
          stringr::str_detect(gas, "H2") ~ "Hydrogen",
          stringr::str_detect(gas, "N2") ~ "Nitrogen",
          TRUE                  ~ as.character(NA)
        )
      ) %>%
      # reorder columns
      dplyr::select("type", "phase", "RI", everything())
  }
  return(output)
}


#' Retrieve retention indices from NIST
#' @description This function scrapes NIST for literature retention indices
#'  given CAS numbers as an input.
#'
#' @param query query
#' @param from what kind of search
#' @param type Retention index type. One of \code{"kovats"}, \code{"linear"},
#'  \code{"alkane"}, or \code{"lee"}. See details for more.
#' @param polarity Column polarity. One of "polar" or "non-polar"
#'  to get RIs calculated for polar or non-polar columns.
#' @param temp_prog Temperature program. One of "isothermal", "ramp",
#'  or "custom".
#' @details The types of retention indices included in NIST include Kovats
#'  (\code{"kovats"}), Van den Dool and Kratz (\code{"linear"}), normal alkane
#'  (\code{"alkane"}), and Lee (\code{"lee"}). Details about how these are
#'  calculated are available on the NIST website:
#'  \url{https://webbook.nist.gov/chemistry/gc-ri/}
#' @importFrom purrr map
#' @importFrom purrr map_dfr
#' @import dplyr
#'
#' @return a table of literature RIs with the following columns:
#' \itemize{
#' \item{\code{CAS} is the CAS number}
#' \item{\code{type} is the column type, e.g. "capillary"}
#' \item{\code{phase} is the stationary phase (column phase)}
#' \item{\code{RI} is retention index}
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
#' @export
#' @note Copyright for NIST Standard Reference Data is governed by the Standard
#' Reference Data Act, \url{https://www.nist.gov/srd/public-law}.
#' @seealso \code{\link{is.cas}} \code{\link{as.cas}}
#'
#' @examples
#' \dontrun{
#' myRIs <- nist_ri(c("78-70-6", "13474-59-4"), "linear", "non-polar", "ramp")
#' }
nist_ri <- function(query,
                    from = c("cas", "inchi", "inchikey", "name"),
                    type = c("kovats", "linear", "alkane", "lee"),
                    polarity = c("polar", "non-polar"),
                    temp_prog = c("isothermal", "ramp", "custom")) {

  from <- match.arg(from)
  type <- match.arg(type)
  polarity <- match.arg(polarity)
  temp_prog <- match.arg(temp_prog)

  ri_xmls <-
    map(
      query,
      ~ get_ri_xml(
        query = .x,
        from = from,
        type = type,
        polarity = polarity,
        temp_prog = temp_prog
      )
    ) %>%
    setNames(query)

  ri_tables <- map_dfr(ri_xmls, tidy_ritable, .id = "query")
  return(ri_tables)
}
