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

#add_license(list()) %>% as_xml() %>% write_xml('doge.xml')
# <intellectualRights>
#   <para>This data package is released to the \"public domain\" under Creative Commons CC0 1.0
#     \"No Rights Reserved\" (see: https://creativecommons.org/publicdomain/zero/1.0/). It is
#     considered professional etiquette to provide attribution of the original work if this data
#     package is shared in whole or by individual components. A generic citation is provided for
#     this data package on the website https://portal.edirepository.org (herein \"website\") in
#     the summary metadata page. Communication (and collaboration) with the creators of this data
#     package is recommended to prevent duplicate research or publication. This data package (and
#     its components) is made available \"as is\" and with no warranty of accuracy or fitness for
#     use. The creators of this data package and the website shall not be liable for any damages
#     resulting from misinterpretation or misuse of the data package or its components. Periodic
#     updates of this data package may be available from the website. Thank you.&#13; 
#   </para>
# </intellectualRights>
# <licensed>
#   <licenseName>Creative Commons Zero v1.0 Universal</licenseName>
#   <url>https://spdx.org/licenses/CC0-1.0.html</url>
#   <identifier>CC0-1.0</identifier>
# </licensed>
