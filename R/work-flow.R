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

# attribute_list <- add_attribute_list(attribute_name = "site_id", attribute_definition = "Site id as used in sites table",
#                                      storage_type = "typeSystem = 'http://www.w3.org/2001/XMLSchema-datatypes'>string<",
#                                      measurement_scale = "nominal", nominal_scale_definition = "Site id as used in sites table.")
# add_data_table(list(), entity_name = "692_EML_IncubationByDepth_SoilCO2Fluxes.csv",
#                               entity_description = "Soil CO2 Fluxes 2013-2014", object_name = "692_EML_IncubationByDepth_SoilCO2Fluxes.csv",
#                               number_of_headers = "1", record_delimiter = "n", physical_line_delimiter = "n",
#                               attribute_orientation = "column", field_delimiter = ",",
#                               online_url = "function='download'>https://pasta.lternet.edu/package/data/eml/knb-lter-bnz/692/2/b52b9d6ab39ff0b903bdb375d7debc69",
#                               attribute_list = attribute_list, case_sensitivity = "yes", number_of_records = "1",
#                               constraint = "TODO") %>%
#   as_xml() %>%
#   write_xml('dogggg.xml')
# 



  
  