# WORK FLOW -----
#' @title temp
d <- function() {
  rmarkdown::pandoc_convert('data-raw/example_abstract.docx', to = 'markdown', 
                            wd = getwd(), output = 'data-raw/temp.md')
  
  a <- read_file('data-raw/temp.md')
  
  dataset <- list() %>%
    add_title(title = "OMG it is a title of sufficient length, WOW!", 
              short_name = "shorter name dur") %>% 
    add_abstract(a) %>%
    add_funding(funder_name = "National Science Foundation", 
                funder_identifier = "http://dx.doi.org/10.13039/100000001",
                award_number = "1656026",
                award_title = "LTER: Beaufort Sea Lagoons: An Arctic Coastal Ecosystem in Transition",
                award_url = "https://www.nsf.gov/awardsearch/showAward?AWD_ID=1656026",
                funding_description = "BLE LTER is supported by the National Science") %>%
    #The add_funding doesn't have the exact nesting structure as in the example 
    add_license(default_license = "CCO")
  list(dataset = dataset) %>% 
    as_xml() %>% 
    xml2::write_xml('dogee.xml')
  
  return(NULL)
}

# add_physical(file_path = "tests/testthat/test_data.csv",
#              number_of_headers = "1",
#              record_delimiter = "\\r\\n",
#              attribute_orientation = "column",
#              field_delimiter = ",",
#              data_url = "https://mydata.org/etc") %>%
#   EML::write_eml('dogge.xml')