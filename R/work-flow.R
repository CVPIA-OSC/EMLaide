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

# add_method(title = "Climate Data",
#            description = "Daily temperature (maximum/minimum) and
#            precipitation data were obtained for each stand from 1996 to 2011
#            from the online PRISM Gridded Climate database (PRISM Climate Group,
#            Oregon State University, http://prism.oregonstate.edu, created 26 Mar 2015)
#            by interpolating 4km2 resolution climate data at the centroid of each
#            eastern hemlock stand using values from surrounding grid cell centers
#            and inverse-distance squared weighting.",
#            instrumentation = "Thermometer") %>%
#   as_xml() %>%
#   write_xml('doggggg.xml')