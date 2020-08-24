library(tidyverse)
library(readxl)
parent_element <- list() 

#Append Personnel Information 
personnel_table <- read_excel("data-raw/template/template.xlsx", sheet = "personnel")
for (i in 1:nrow(personnel_table)) {
  current <- personnel_table[i, ]
  parent_element <- add_personnel(parent_element = parent_element, 
                                  first_name = current$first_name, 
                                  last_name = current$last_name, 
                                  email = current$email, 
                                  role = current$role, 
                                  orcid = current$orcid)
}

#Append Title Information
parent_element <- add_title(parent_element = parent_element, title = "This is the title of the dataset and it fits the requirements.", 
                            short_name = "A shorter name than the title")

#Append Keyword Set 
parent_element <- add_keyword_set(parent_element = parent_element, 
                                 keyword_set = list(keyword = list("chinook", "salmon", "steelhead"), 
                                                    keywordThesauraus = "LTER controlled vocabulary"))
parent_element <- add_keyword_set(parent_element = parent_element,
                                 keyword_set = list(keyword = list("water", "stream", "river"), 
                                                    keywordThesauraus = "LTER controlled vocabulary"))

#Append Abstract 
rmarkdown::pandoc_convert('data-raw/template/abstract_template.docx', to = 'markdown', 
                          wd = getwd(), output = 'data-raw/template/abstract.md')

abstract <- read_file('data-raw/template/abstract.md')
parent_element <- add_abstract(parent_element = parent_element, abstract = abstract)

#Append License and Intellectual Rights Information 

parent_element <- add_license(parent_element = parent_element, default_license = "CCO")

#Append Funding Information

parent_element <- add_funding(parent_element = parent_element, funder_name = "National Science Foundation",
                              funder_identifier = "http://dx.doi.org/10.13039/100000001",
                              award_number = "123456",
                              award_title = "Example Data Title",
                              award_url = "examplelink.org",
                              funding_description = "The funding recieved was awarded in 2020.")

#Append Maintenance Information 

parent_element <- add_maintenance(parent_element = parent_element, status = "complete")

#Append Method Information 
method_table <- read_excel("data-raw/template/template.xlsx", sheet = "methods")
for (i in 1:nrow(method_table)) {
  current <- method_table[i, ]
  parent_element <- add_method(title = current$title,
                               description = current$description,
                               instrumentation = current$instrumentation)
  
}

#Append Coverage Information
chinook <- add_taxonomic_coverage(CVPIA_common_species =  cvpiaEDIutils::CVPIA_common_species$chinook)
lion <- add_taxonomic_coverage(kingdom_value = "Animalia",
                               phylum_value = "Chordata",
                               class_value = "Mammalia",
                               order_value = "Carnivora",
                               family_value = "Felidae",
                               genus_value = "Panthera",
                               species_value = "Panthera Leo",
                               common_name = "Lion",
                               taxon_id = "183803")
taxonomic_coverage <- list(chinook, lion)
coverage_table <- read_excel("data-raw/template/template.xlsx", sheet = "coverage")
for (i in 1:nrow(coverage_table)) {
  current <- coverage_table[i, ]
  tax <- taxonomic_coverage[i]
  parent_element <- add_coverage(parent_element = parent_element,
                                 geographic_description = current$geographic_description,
                                 west_bounding_coordinate = current$west_bounding_coordinate,
                                 east_bounding_coordinate = current$east_bounding_coordinate,
                                 north_bounding_coordinate = current$north_bounding_coordinate,
                                 south_bounding_coordinate = current$south_bounding_coordinate,
                                 begin_date = current$begin_date,
                                 end_date = current$end_date,
                                 taxonomic_coverage = tax
                                 )
}

# Append Physical Information 
physical_element_table_1 <- add_physical(file_path = "/Users/lizzyshaw/FlowWest/cvpiaEDIutils/tests/testthat/test_data.csv", 
                                         data_url = "my.data.com")

physical_element_table_2 <- add_physical(file_path = "/Users/lizzyshaw/FlowWest/cvpiaEDIutils/tests/testthat/test_data.csv", 
                                         number_of_headers = "1",
                                         record_delimiter = ",",
                                         attribute_orientation = "rows",
                                         field_delimiter = ",",
                                         data_url = "my.data.org")

physical_list <- list(physical_element_table_1, physical_element_table_2)

# Append Attribute Information  
nominal <- add_attribute(attribute_name = "site_id",
                             attribute_definition = "Site id as used in sites table",
                             storage_type = cvpiaEDIutils::storage_type$integer,
                             measurement_scale = cvpiaEDIutils::measurement_scale$nominal,
                             domain = "text",
                             definition = "Site id as used in sites table.")
ordinal <- add_attribute(attribute_name = "LatitudeDD",
                             attribute_definition = "Latitude",
                             storage_type = cvpiaEDIutils::storage_type$string,
                             measurement_scale = cvpiaEDIutils::measurement_scale$ordinal,
                             domain = "text",
                             definition = "Latitude")
attribute_1_list <- list(nominal, ordinal)

interval <- add_attribute(attribute_name = "Count",
                          attribute_definition = "Number of individuals observed",
                          measurement_scale = cvpiaEDIutils::measurement_scale$interval,
                          storage_type = cvpiaEDIutils::storage_type$integer,
                          type = "interval",
                          units = "number",
                          unit_precision = "1",
                          number_type = "whole",
                          minimum = "0")

ratio <- add_attribute(attribute_name = "pH",
                       attribute_definition = "pH of soil solution",
                       storage_type = cvpiaEDIutils::storage_type$float,
                       measurement_scale = cvpiaEDIutils::measurement_scale$ratio,
                       type = "ratio",
                       units = "dimensionless",
                       unit_precision = "0.01",
                       number_type = "real")

dateTime <- add_attribute(attribute_name = "Yrs",
                          attribute_definition = "Calendar year of the observation from years 1990 - 2010.",
                          storage_type = cvpiaEDIutils::storage_type$integer,
                          measurement_scale = cvpiaEDIutils::measurement_scale$dateTime,
                          attribute_label = "Years",
                          date_time_format = "YYYY",
                          date_time_precision = "1",
                          minimum = "1993",
                          maximum = "2003")
attribute_2_list <- list(interval, ratio, dateTime)

all_attributes <- list(attribute_1_list, attribute_2_list)

# Append Data Table 
data_table <- read_excel("data-raw/template/template.xlsx", sheet = "data_table")
for (i in 1:nrow(data_table)) {
  current <- data_table[i, ]
  phy <- physical_list[i]
  att <- all_attributes[i]
  parent_element <- add_data_table(parent_element = parent_element,
                                   entity_name = current$entity_name,
                                   entity_description = current$entity_description,
                                   physical = phy,
                                   attribute_list = att,
                                   number_of_records = current$number_of_records,
                                   alternate_identifier = current$alternate_identifier)
}
parent_element

parent_element %>%
  EML::write_eml('eml.xml')


# # Append Attribute Information 
# ## Nominal 
# attribute_list <- list()
# nominal_table <- read_excel("data-raw/template/template.xlsx", sheet = "nominal")
# for (i in 1:nrow(nominal_table)) {
#   current <- nominal_table[i,]
#   attribute_list <- add_attribute(attribute_list = list(),
#                                   attribute_name = current$attribute_name,
#                                   attribute_definition = current$attribute_definition,
#                                   storage_type = current$storage_type,
#                                   measurement_scale = current$measurement_scale,
#                                   domain = current$domain,
#                                   definition = current$definition,
#                                   text_pattern = current$text_pattern,
#                                   attribute_label = current$attribute_label)
# }
# attribute_list 
# 
# ## Ordinal 
# 
# ordinal_table <- read_excel("data-raw/template/template.xlsx", sheet = "ordinal")
# for (i in 1:nrow(ordinal_table)) {
#   current <- ordinal_table[i,]
#   attribute_list <- add_attribute(attribute_name = current$attribute_name,
#                                   attribute_definition = current$attribute_definition,
#                                   storage_type = current$storage_type,
#                                   measurement_scale = current$measurement_scale,
#                                   domain = current$domain,
#                                   definition = current$definition,
#                                   text_pattern = current$text_pattern,
#                                   attribute_label = current$attribute_label)
# }
# attribute_list
# 
# ## Interval 
# interval_table <- read_excel("data-raw/template/template.xlsx", sheet = "interval")
# for (i in 1:nrow(interval_table)) {
#   current <- interval_table[i,]
#   attribute_list <- add_attribute(attribute_name = current$attribute_name,
#                                   attribute_definition = current$attribute_definition,
#                                   storage_type = current$storage_type,
#                                   measurement_scale = current$measurement_scale,
#                                   type = current$type,
#                                   units = current$units,
#                                   unit_precision = current$unit_precision,
#                                   number_type = current$number_type,
#                                   minimum = current$minimum,
#                                   maximum = current$maximum,
#                                   attribute_label = current$attribute_label)
# }
# attribute_list
# 
# ## Ratio 
# 
# ratio_table <- read_excel("data-raw/template/template.xlsx", sheet = "ratio")
# for (i in 1:nrow(ratio_table)) {
#   current <- ratio_table[i,]
#   attribute_list <- add_attribute(attribute_name = current$attribute_name,
#                                   attribute_definition = current$attribute_definition,
#                                   storage_type = current$storage_type,
#                                   measurement_scale = current$measurement_scale,
#                                   type = current$type,
#                                   units = current$units,
#                                   unit_precision = current$unit_precision,
#                                   number_type = current$number_type,
#                                   minimum = current$minimum,
#                                   maximum = current$maximum,
#                                   attribute_label = current$attribute_label)
# }
# attribute_list
# 
# ## dateTime
# 
# dateTime_table <- read_excel("data-raw/template/template.xlsx", sheet = "dateTime")
# for (i in 1:nrow(dateTime_table)) {
#   current <- dateTime_table[i,]
#   attribute_list <- add_attribute(attribute_name = current$attribute_name,
#                                   attribute_definition = current$attribute_definition,
#                                   storage_type = current$storage_type,
#                                   measurement_scale = current$measurement_scale,
#                                   date_time_format = current$date_time_format,
#                                   date_time_precision = current$date_time_precision,
#                                   minimum = current$minimum,
#                                   maximum = current$maximum,
#                                   attribute_label = current$attribute_label)
# }
# attribute_list
# 
# attribute_list %>%
#   EML::write_eml('eml.xml')
# 
# parent_element %>%
#   EML::write_eml('eml.xml')
