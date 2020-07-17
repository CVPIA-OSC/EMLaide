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

# add_taxonomic_coverage(list(),parent_element, CVPIA_common_species = NULL,
#                        kingdom = "kingdom", kingdom_value,
#                        phylum = "phylum", phylum_value,
#                        class = "class", class_value,
#                        order = "order", order_value,
#                        family = "family", family_value,
#                        genus = "genus", genus_value,
#                        species = "species", species_value,
#                        common_name) %>%
#   as_xml() %>%
#   write_xml('dogggggggggg.xml')


# add_taxonomic_coverage(list(), CVPIA_common_species = "delta_smelt") %>%
#   as_xml() %>%
#   write_xml('dogggggggggg.xml')

#taxize::classification(sci_id = "161980", db = "itis")
#If you put in the ITIS ID number (listed as the taxonomic serial number on their website), the command will identify kingdom thru species for you. 

