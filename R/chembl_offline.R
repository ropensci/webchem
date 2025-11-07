#' examples
#' \dontrun{
#' chembl_query_offline(query = "CHEMBL1082", resource = "molecule")
#' }
chembl_query_offline <- function(
    query,
    resource = "molecule",
    version = "latest",
    verbose = getOption("verbose")
) {
  resource <- match.arg(resource, chembl_resources())
  if (!inherits(version, "chembl_version")) {
    version <- validate_chembl_version(version = version)
  }
  FUN <- paste0("chembl_offline_", resource)
  if (!exists(FUN)) {
    msg <- paste0(
      "Offline query for resource '", resource, "' is not implemented.")
    stop(msg)
  }
  do.call(FUN, args = list(
    query = query,
    version = version,
    verbose = verbose
  ))
}

#' Replicate ChEMBL API molecule resource using a local ChEMBL database
#'
#' @param query character; ChEMBL ID of the molecule to retrieve
#' @param version character; version of the ChEMBL database
#' @param verbose logical; print verbose messages to the console?
#' examples
#' \dontrun{
#' chembl_offline_molecule(query = "CHEMBL1082")
#' }
#' @noRd
chembl_offline_molecule <- function(
    query,
    version = "latest",
    verbose = getOption("verbose")
  ){
  if (!inherits(version, "chembl_version")) {
    version <- validate_chembl_version(version = version)
  }
  # connect to local database
  con <- connect_chembl(version = version)
  # confirm chembl_id
  entity_type <- tbl(con, "chembl_id_lookup") |>
    dplyr::filter(chembl_id == query) |>
    dplyr::collect() |>
    dplyr::pull(entity_type)
  if (entity_type != "COMPOUND") {
    msg <- paste0(query, " is not a COMPOUND. It is a ", entity_type, ".")
    stop(msg)
  }
  internal_id <- tbl(con, "molecule_dictionary") |>
    dplyr::filter(chembl_id == query) |>
    dplyr::pull(molregno)
  molecule_dictionary <- tbl(con, "molecule_dictionary") |>
    dplyr::filter(chembl_id == query) |>
    dplyr::select(
      "availability_type",
      "black_box_warning",
      "chebi_par_id",
      "chemical_probe",
      "chirality",
      "dosed_ingredient",
      "first_approval",
      "first_in_class",
      "indication_class",
      "inorganic_flag",
      "max_phase",
      "molecule_type",
      "natural_product",
      "oral",
      "orphan",
      "parenteral",
      "polymer_flag",
      "pref_name",
      "prodrug",
      "structure_type",
      "therapeutic_flag",
      "topical",
      "usan_stem",
      "usan_stem_definition",
      "usan_substem",
      "usan_year",
      "withdrawn_flag"
    ) |>
    dplyr::collect()
  if (nrow(molecule_dictionary) > 1) {
    stop("Multiple hits found.")
  }
  atc_classifications <- tbl(con, "molecule_atc_classification") |>
    dplyr::filter(molregno == internal_id) |>
    dplyr::pull(level5)
  extract_variable <- function(df, var) {
    if (nrow(df) == 0) {
      return(NULL)
    } else {
      return(df[[var]])
    }
  }
  biotherapeutics <- tbl(con, "biotherapeutics") |>
    dplyr::filter(molregno == internal_id) |>
    dplyr::collect()
  biotherapeutic <- extract_variable(biotherapeutics, "description")
  helm_notation <- extract_variable(biotherapeutics, "helm_notation")
  molecule_hierarchy_raw <- tbl(con, "molecule_hierarchy") |>
    dplyr::filter(molregno == internal_id) |>
    dplyr::collect()
  molecule_hierarchy <- tibble::tibble(
    "active_chembl_id" = query,
    "molecule_chembl_id" = query,
    "parent_chembl_id" = tbl(con, "molecule_dictionary") |>
      dplyr::filter(molregno == molecule_hierarchy_raw$parent_molregno) |>
      dplyr::pull(chembl_id)
  )
  molecule_properties <- tbl(con, "compound_properties") |>
    dplyr::filter(molregno == internal_id) |>
    dplyr::select(
      "alogp",
      "aromatic_rings",
      "cx_logd",
      "cx_logp",
      "cx_most_apka",
      "cx_most_bpka",
      "full_molformula",
      "full_mwt",
      "hba",
      "hba_lipinski",
      "hbd",
      "hbd_lipinski",
      "heavy_atoms",
      "molecular_species",
      "mw_freebase",
      "mw_monoisotopic",
      "np_likeness_score",
      "num_lipinski_ro5_violations",
      "num_ro5_violations",
      "psa",
      "qed_weighted",
      "ro3_pass",
      "rtb"
    ) |>
    dplyr::collect()
    molecule_structures <- tbl(con, "compound_structures") |>
      dplyr::filter(molregno == internal_id) |>
      dplyr::select(
        "canonical_smiles",
        "molfile",
        "standard_inchi",
        "standard_inchi_key"
      ) |>
      dplyr::collect()
    molecule_synonyms <- tbl(con, "molecule_synonyms") |>
      dplyr::filter(molregno == internal_id) |>
      dplyr::select(
        "synonyms",
        "syn_type"
      ) |>
      collect() |>
      dplyr::rename(molecule_synonym = synonyms) |>
      dplyr::arrange(molecule_synonym)
    out <- c(
      molecule_dictionary,
      list(
        "atc_classifications" = atc_classifications,
        "biotherapeutic" = biotherapeutic,
        "helm_notation" = helm_notation,
        "molecule_chembl_id" = query,
        "molecule_hierarchy" = molecule_hierarchy,
        "molecule_properties" = molecule_properties,
        "molecule_structures" = molecule_structures,
        "molecule_synonyms" = molecule_synonyms
      )
    )
    out <- out[sort(names(out))]
    return(out)
}

