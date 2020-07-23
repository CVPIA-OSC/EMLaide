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


 # taxonomic_coverage <- add_taxonomic_coverage(CVPIA_common_species = "chinook")
 # add_coverage(parent_element = list(), geographic_description = "Description",
 #              west_bounding_coordinate = "-160.594000", east_bounding_coordinate = "-134.104800",
 #             north_bounding_coordinate = "71.238300", south_bounding_coordinate = "67.865000",
 #            begin_date = "1980-01-01", end_date = "2010-12-31", taxonomic_coverage = taxonomic_coverage) %>%
 #   add_personnel(first_name = "Edith", last_name = "Windsor",
 #                 email = 'ewindsor@ibm.com', role = 'Data Manager', organization = 'IBM', orcid = "HI") %>%
 #   as_xml() %>%
 #   write_xml('dogggggggggg.xml')