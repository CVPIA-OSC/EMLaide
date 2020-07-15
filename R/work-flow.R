# WORK FLOW -----
#' @title temp
d <- function() {
  rmarkdown::pandoc_convert('data-raw/example_abstract.docx', to = 'markdown', 
                            wd = getwd(), output = 'data-raw/temp.md')
  
  a <- read_file('data-raw/temp.md')
  
  dataset <- list() %>%
    add_title(title = "OMG it is a title of sufficient length, WOW!", 
              short_name = "shorter name dur") %>% 
    add_abstract(a) 
  
  list(dataset = dataset) %>% 
    as_xml() %>% 
    xml2::write_xml('doge.xml')
  
  return(NULL)
}

# add_coverage(list(), geographic_description = "North Slope drainage basin:Bounding box encompasses 42 drainage basins totaling the North Slope drainage basin, Alaska, USA.",
#              west_bounding_coordinate = "-160.594000", east_bounding_coordinate = "-134.104800",
#              north_bounding_coordinate = "71.238300", south_bounding_coordinate = "67.865000",
#              begin_date = "1980-01-01", end_date = "2010-12-31") %>%
#   as_xml() %>%
#   write_xml('dogeee.xml')
