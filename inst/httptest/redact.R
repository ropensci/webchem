#shortens file paths created by httptest so that R CMD check doesn't complain
httptest::set_redactor(
  function (resp) {
    resp |>
      httptest::gsub_response("property/.*/", "property/") |>
      httptest::gsub_response("www.ebi.ac.uk-80/webservices/chebi/2.0/", "chebi/") |>
      httptest::gsub_response("www.ebi.ac.uk/chembl/api/data/", "chembl/") |>
      httptest::gsub_response("api.rsc.org/compounds/v1/filter/", "cs/") |>
      httptest::gsub_response("webetox.uba.de/webETOX/public/search/", "etox/")
  }
)