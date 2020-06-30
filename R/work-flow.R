# WORK FLOW -----
rmarkdown::pandoc_convert('data-raw/example_abstract_methods3.docx', to = 'markdown', 
                          wd = getwd(), output = 'data-raw/temp.md')

a <- read_file('data-raw/temp.md')

dataset <- list() %>%
  add_title(title = "OMG it is a title of sufficient length, WOW!", 
            short_name = "shorter name dur") %>% 
  add_abstract(a) 

list(dataset = dataset) %>% 
  as_xml() %>% 
  xml2::write_xml('doge.xml')
