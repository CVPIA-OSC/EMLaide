library("vcr")
invisible(vcr::vcr_configure(
  filter_sensitive_data = list("<<<my_api_key>>>" = Sys.getenv('APIKEY')),  # add this
  dir = "../fixtures"
))
vcr::check_cassette_names()
