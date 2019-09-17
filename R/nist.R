#' Read HTML slowly
#' @description Just adds a rest after `read_html()`.  Useful for web-scraping.
#' @import xml2
#' @param x a URL
#' @param ... currently unused
#'
#' @return html
#'
read_html_slow <- function(x, ...){
  output <- xml2::read_html(x)
  Sys.sleep(1)
  return(output)
}

#' Scrape Retention Indices from NIST
#'
#' @param cas cas number
#' @param type what kind of RI
#' @param polarity polar or non-polar
#' @param temp_prog what kind of temperature program
#'
#' @import rvest
#' @import xml2
#'
#' @return an xml nodeset
#'
get_ri_xml <- function(cas, type = c("kovats", "linear", "alkane", "lee"),
                       polarity = c("polar", "non-polar"),
                       temp_prog = c("isothermal", "ramp", "custom")){
  #Construct URL
  type_str <- toupper(paste(type, "RI", polarity, temp_prog, sep = "-"))
  URL_detail <- paste0("https://webbook.nist.gov/cgi/cbook.cgi?ID=C",
                       gsub("-", "", cas),
                       "&Units=SI&Mask=2000&Type=",
                       type_str)
  #Read URL and extract xml
  page <- read_html_slow(URL_detail)
  ri_xml.all <- html_nodes(page, ".data")

  #Warn if table doesn't exist at URL
  if (length(ri_xml.all) == 0) {
    warning(paste0("There are no RIs for CAS# ", cas, " of type ", type_str, ". Returning NA."))
    ri_xml <- as.data.frame(NA)
  } else {
    ri_xml <- ri_xml.all
  }

  #set attributes to label what type of RI
  attr(ri_xml, "type") <- type
  attr(ri_xml, "polarity") <- polarity
  attr(ri_xml, "temp_prog") <- temp_prog

  return(ri_xml)
}


#' Tidier for webscraped RI ri_xml
#'
#' @param ri_xml captured by `get_ri_xml`
#'
#' @import rvest
#' @importFrom purrr map
#' @importFrom purrr map_dfr
#' @import dplyr
#'
#' @return a single table
#'
tidy_ritable <- function(ri_xml){
  #Skip all these steps if the table didn't exist at the URL and was set to NA
  if (any(is.na(ri_xml))) {
    output <- ri_xml

  } else {
    # Read in the tables from xml
    table.list <- map(ri_xml, html_table)

    # Transpose and tidy
    tidy1 <- map_dfr(
      table.list,
      ~t(.) %>%
        as.data.frame(stringsAsFactors = FALSE) %>%
        # pull column names from first row
        setNames(.[1, ]) %>%
        .[-1, ]
    )

    # Extract the temperature program metadata
    temp_prog <- attr(ri_xml, "temp_prog")

    if (temp_prog == "custom") {
      tidy2 <- rename(tidy1,
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
        mutate_at(vars("length", "diameter", "thickness", "RI"),
                  as.numeric)

    } else if (temp_prog == "ramp") {
      tidy2 <- rename(tidy1,
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
        mutate_at(vars("length", "diameter", "thickness", "temp_start", "temp_end",
                       "temp_rate", "hold_start", "hold_end",  "RI"),
                  as.numeric)

    } else if (temp_prog == "isothermal") {
      tidy2 <- rename(tidy1,
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
        mutate_at(vars("length", "diameter", "thickness", "temp",  "RI"),
                  as.numeric)
    }

    # make NAs explicit and gas abbreviations consistent
    output <- tidy2 %>%
      mutate_all(~na_if(., "")) %>%
      mutate(gas = case_when(
        str_detect(gas, "He") ~ "Helium",
        str_detect(gas, "H2") ~ "Hydrogen",
        str_detect(gas, "N2") ~ "Nitrogen",
        TRUE                  ~ as.character(NA)
      )) %>%
      # reorder columns
      select("type", "phase", "RI", everything())
  }
  return(output)
}


#' Retrieve retention indices from NIST
#' @description This function scrapes NIST for literature retention indices given CAS numbers as an input.
#'
#' @param cas CAS numbers either as numeric or formatted correctly with hyphens.
#' @param type One of "kovats", "linear", "alkane", or "lee". Type of RI to retrieve. Details about how these are calculated here: [https://webbook.nist.gov/chemistry/gc-ri/]
#' @param polarity One of "polar" or "non-polar" to get RIs calculated for polar or non-polar columns.
#' @param temp_prog One of "isothermal", "ramp", or "custom".
#'
#' @importFrom purrr map
#' @importFrom purrr map_dfr
#' @import dplyr
#'
#' @return a table of literature RIs
#'
#' @export
#'
#' @seealso \code{\link{is.cas}} \code{\link{as.cas}}
#'
#' @examples
#' \dontrun{
#' myRIs <- nist_ri(c("78-70-6", "13474-59-4"), "linear", "non-polar", "ramp")
#' }
nist_ri <- function(cas,
                   type = c("kovats", "linear", "alkane", "lee"),
                   polarity = c("polar", "non-polar"),
                   temp_prog = c("isothermal", "ramp", "custom")){
  type <- match.arg(type)
  polarity <- match.arg(polarity)
  temp_prog <- match.arg(temp_prog)
  ri_xmls <- map(cas, ~get_ri_xml(., type, polarity, temp_prog)) %>%
    setNames(cas)

  ri_tables <- map_dfr(ri_xmls, tidy_ritable, .id = "CAS")
  return(ri_tables)
}
