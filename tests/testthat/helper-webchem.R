library("vcr")
invisible(vcr::vcr_configure(
  dir = "../fixtures"
))
vcr::check_cassette_names()
