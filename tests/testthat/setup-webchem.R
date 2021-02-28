library("vcr")
invisible(vcr::vcr_configure(
  filter_sensitive_data = list("<<my_api_key>>" = Sys.getenv("CHEMSPIDER_KEY")),
  dir = vcr::vcr_test_path("fixtures")
))

vcr::check_cassette_names()

if (!nzchar(Sys.getenv("CHEMSPIDER_KEY"))) {
  Sys.setenv("CHEMSPIDER_KEY" = "<<my_api_key>>")
}
