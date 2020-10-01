#' Query the PAN Pesticide database
#'
#' Retrieve information from the PAN database (\url{http://www.pesticideinfo.org/})
#' @import xml2
#' @importFrom utils adist
#' @importFrom rvest html_table
#' @importFrom stats rgamma
#' @param query character; searchterm, e.g. chemical name or CAS.
#' @param from character; one of "name" or "cas".
#' @param match character; \code{match="all"} returns all matches,
#'   \code{match="first"} the first one and \code{match="best"} (recommended) the hit with the lowest
#'    Levenshtein distance between query and matching synonym.
#' @param verbose logical; should a verbose output be printed on the console?
#' @param ... currently not used.
#' @return a named list of 73 entries,
#'   see \url{http://www.pesticideinfo.org/Docs/ref_overview.html} for more information.
#'   If \code{match="best"} an additional entry \code{match_score} with the normalized
#'   Levenshtein distance (0 = perfect match, 1 = worst match).
#'
#
#' CAS Number; U.S. EPAPC Code; CA ChemCode;
#' Use Type; Chemical Class; Molecular Weight; U.S. EPARegistered ; CA Reg Status;
#' PIC; POPs; WHO Obsolete; EPA HAP; CA TAC; Ground Water Contaminant;
#' CA Grnd Water Contam.; Acute Aquatic Toxcity; Chronic Aquatic Toxicity;
#' PAN BadActor Chem; Dirty Dozen; Acute Toxicity Summary; Cholinesterase Inhibitor;
#' Acute rating from U.S. EPA product label; U.S. NTP Acute Toxicity Studies;
#' Material Safety Data Sheets; TRI Acute Hazard; WHO Acute Toxicity; Cancer Rating;
#' U.S. EPA Carcinogens; IARC Carcinogens; U.S. NTP Carcinogens;
#' California Prop 65 Known Carcinogens; TRI Carcinogen;
#' Developmental or Reproductive Toxicity; CA Prop 65 Developmental Toxin;
#' U.S. TRI Developmental Toxin; CA Prop 65 Female Reproductive Toxin;
#' CA Prop 65 Male Reproductive Toxin ; U.S. TRI Reproductive Toxin;
#' Endocrine Disruption; E.U. ED Rating; Benbrook list; Denmark Inert list;
#' Colborn list; Illinois EPA list; Keith list; Water Solubility (Avg, mg/L);
#' Adsorption Coefficient (Koc); Hydrolysis Half-life (Avg, Days);
#' Aerobic Soil Half-life (Avg, Days); Anaerobic Soil Half-life (Avg, Days);
#' Maximum Contaminant Level (MCL) (ug/L); Maximum Contaminant Level Goal (MCLG) (ug/L);
#' One Day Exposure Health Advisory Level (ug/L); Ten Day Exposure Health Advisory Level (ug/L);
#' Reference Dose (ug/kg/day); U.S. Drinking Water Equivalent Level (ug/L);
#' Lifetime Exposure Health Advisory Level (ug/L);
#' Lifetime Estimated Cancer Risk (cases per 1,000,000);
#' Maximum Acceptable Concentration (MAC) (ug/L);
#' Interim Maximum Acceptable Concentration (IMAC) (ug/L);
#' Aesthetic Objectives (ug/L); Fresh Water Quality Criteria Continuous Exposure (ug/L);
#' Fresh Water Quality Criteria Maximum Peak (ug/L); Salt Water Quality Criteria Continuous Exposure (ug/L);
#' Salt Water Quality Criteria Max (ug/L); Human Consumption of Organisms from Water Source (ug/L);
#' Human Consumption of Water and Organisms from Water Source (ug/L);
#' Taste and Odor Criteria (ug/L);
#' Fresh Water Guidelines (ug/L); Salt Water Guidelines (ug/L);
#' Irrigation Water Guidelines (ug/L); Livestock Water Guidelines (ug/L);
#' Chemical Name; matching synonym; source URL
#' @export
#' @examples
#' \dontrun{
#'  # might fail if API is not available
#'
#'  # return all hits
#'  pan_query('2,4-dichlorophenol')[[1]][c(1, 2, 5, 74)]
#'  # return only first hit
#'  pan_query('2,4-dichlorophenol', match = 'first')[[1]][c(1, 2, 5, 74)]
#'  # return only best hit
#'  pan_query('2,4-dichlorophenol', match = 'best')[[1]][c(1, 2, 5, 74)]
#'
#'  out <- pan_query(c('Glyphosate', 'Rotenone'), from = "name", match = 'best')
#'  out
#'
#'  # extract Acute Toxicity Summary
#'  sapply(out, function(y) y$`Acute Toxicity Summary`)
#' }
pan_query <- function(query, from = c("name", "cas"),
                      match = c('best', 'all', 'first', "na"), verbose = TRUE, ...){

  if (!ping_service("pan")) stop(webchem_message("service_down"))

  match <- match.arg(match)
  from <- match.arg(from)

  foo <- function(query, match, from, verbose) {
    if (is.na(query)) {
      if (verbose) webchem_message("na")
      return(NA)
    }
    baseurl <- 'http://www.pesticideinfo.org/List_Chemicals.jsp?'
    baseq <- paste0('ChooseSearchType=Begin&ResultCnt=50&dCAS_No=y&dEPA_PCCode=y&',
                    'dDPR_Chem_Code=y&dUseList=y&dClassList=y&dMol_weight=y&',
                    'dEPA_Reg=y&dCA_Reg=y&dPIC=y&dPOP=y&dWHOObsolete=y&dEPA_HAP=y&',
                    'dCA_TAC=y&dS_GrdWat=y&dDPR_GrdWatCont=y&dS_AquaAcute=y&',
                    'dS_AquaChronic=y&dS_BA=y&dDirtyDozen=y&dS_Acute=y&',
                    'dS_ChEInhib=y&',
                    'dEPAAcute=y&dNTPAcute=y&dPANAcute=y&dTRI_Acute=y&dWHOAcute=y&',
                    'dS_Cancer=y&dEPACancer=y&dIARCCancer=y&dNIHCancer=y&',
                    'dp65_Cancer=y&dTRI_Cancer=y&dS_DevRep=y&dp65_Dev=y&dTRI_Dev=y&',
                    'dp65_Female=y&dp65_Male=y&dTRI_Repr=y&dS_ED=y&dED_EU=y&',
                    'dED_Benbrook=y&dED_Denmark_Inert=y&dED_Colborn=y&',
                    'dED_Illinois_EPA=y&dED_Keith=y&dAvg_Sol=y&dAvg_Koc=y&',
                    'dAvg_Hydrolysis=y&dAvg_Aerobic=y&dAvg_Anaerobic=y&dMCL=y&',
                    'dMCLG=y&dOneDay=y&dTenDay=y&dLifetime=y&dRfD=y&dDWEL=y&',
                    'dCancerRisk=y&dMAC=y&dIMAC=y&dAO=y&dFWCont=y&dFWMax=y&',
                    'dSWCont=y&dSWMax=y&dHumConsOrgOnly=y&dHumConsWaterOrg=y&',
                    'dEPAOrganoleptic=y&dCanAquaFWConc=y&dCanAquaMarineConc=y&',
                    'dIrrigConc=y&dLivestockConc=y&')
    qurl <- paste0(baseurl, baseq, 'ChemName=', query)
    if (verbose) webchem_message("query", query, appendLF = FALSE)
    Sys.sleep(rgamma(1, shape = 15, scale = 1/10))
    res <- try(httr::RETRY("GET",
                           qurl,
                           user_agent(webchem_url()),
                           terminate_on = 404,
                           quiet = TRUE), silent = TRUE)
    if (inherits(res, "try-error")) {
      if (verbose) webchem_message("service_down")
      return(NA)
    }
    if (verbose) httr::message_for_status(res)
    if (res$status_code == 200) {
      h <- xml2::read_html(res)
      nd <- xml_find_all(h, "//table[contains(.,'Detailed Info')]")
      if (length(nd) == 0) {
        webchem_message("not_found")
        return(NA)
      }
      ttt <- html_table(nd)[[1]]
      out <- as.list(ttt) #TODO: why not just keep it as a table???
      # clean
      out$`Detailed Info` <- NULL
      names(out) <- gsub('\\n', ' ', names(out))
      out <- rapply(out, f = function(x){
        ifelse(x %in% c('null', '-', ''), NA, x)
      }, how = "replace" )
      out <- c(out[1:46],
               rapply(out[47:73], function(x) gsub(',', '', x), how = 'replace'))

      # split chemName and matching synonym
      out[['matching synonym']] <- sapply(strsplit(out[['Chemical Name and matching synonym']], '\\n'), '[', 2)
      out[['Chemical name']] <- sapply(strsplit(out[['Chemical Name and matching synonym']], '\\n'), '[', 1)
      out[['Chemical Name and matching synonym']] <- NULL
      # return also source url
      # xmlview::xml_view(nd, add_filter = TRUE)
      source_url <- xml_attr(xml_find_all(nd, ".//a[contains(., 'Details')]"), 'href')
      out[['source_url']] <- paste0('http://www.pesticideinfo.org/', source_url)

      ind <-
        matcher(
          x = 1:nrow(ttt),
          query = query,
          result = out[["Chemical name"]],
          match = match,
          from = from,
          verbose = verbose
        )
      out <- lapply(out, '[', ind)

      return(out)
    }
    else {
      if (verbose) httr::message_for_status(res)
      return(NA)
    }
  }
  out <- lapply(query, foo, match = match, from = from, verbose = verbose)
  out <- setNames(out, query)
  class(out) <- c('pan_query', 'list')
  return(out)
}
