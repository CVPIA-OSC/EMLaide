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
parent_element

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
parent_element

#Append Coverage Information
chinook <- add_taxonomic_coverage(CVPIA_common_species =  cvpiaEDIutils::CVPIA_common_species$chinook)
steelhead <- add_taxonomic_coverage(CVPIA_common_species = cvpiaEDIutils::CVPIA_common_species$steelhead)
taxonomic_coverage <- list(chinook, steelhead)
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
parent_element

parent_element %>%
  EML::write_eml('eml.xml')

# Append physical Information 
physical_list <- list()
physical_table <- read_excel("data-raw/template/template.xlsx", sheet = "physical")
for (i in 1:nrow(physical_table)) {
  current <- physical_table[i,]
  physical_list <- add_physical(physical_list = physical_list,
                                file_path = current$file_path,
                                number_of_headers = current$number_of_headers, 
                                record_delimiter = current$record_delimiter, 
                                attribute_orientation = current$attribute_orientation, 
                                field_delimiter = current$field_delimiter,
                                data_url = current$data_url)
}
physical_list 

# Append Attribute Information 
## Nominal 
attribute_list <- list()
nominal_table <- read_excel("data-raw/template/template.xlsx", sheet = "nominal")
for (i in 1:nrow(nominal_table)) {
  current <- nominal_table[i,]
  attribute_list <- add_attribute(attribute_list = list(),
                                  attribute_name = current$attribute_name,
                                  attribute_definition = current$attribute_definition,
                                  storage_type = current$storage_type,
                                  measurement_scale = current$measurement_scale,
                                  domain = current$domain,
                                  definition = current$definition,
                                  text_pattern = current$text_pattern,
                                  attribute_label = current$attribute_label)
}
attribute_list 

## Ordinal 

ordinal_table <- read_excel("data-raw/template/template.xlsx", sheet = "ordinal")
for (i in 1:nrow(ordinal_table)) {
  current <- ordinal_table[i,]
  attribute_list <- add_attribute(attribute_name = current$attribute_name,
                                  attribute_definition = current$attribute_definition,
                                  storage_type = current$storage_type,
                                  measurement_scale = current$measurement_scale,
                                  domain = current$domain,
                                  definition = current$definition,
                                  text_pattern = current$text_pattern,
                                  attribute_label = current$attribute_label)
}
attribute_list

## Interval 
interval_table <- read_excel("data-raw/template/template.xlsx", sheet = "interval")
for (i in 1:nrow(interval_table)) {
  current <- interval_table[i,]
  attribute_list <- add_attribute(attribute_name = current$attribute_name,
                                  attribute_definition = current$attribute_definition,
                                  storage_type = current$storage_type,
                                  measurement_scale = current$measurement_scale,
                                  type = current$type,
                                  units = current$units,
                                  unit_precision = current$unit_precision,
                                  number_type = current$number_type,
                                  minimum = current$minimum,
                                  maximum = current$maximum,
                                  attribute_label = current$attribute_label)
}
attribute_list

## Ratio 

ratio_table <- read_excel("data-raw/template/template.xlsx", sheet = "ratio")
for (i in 1:nrow(ratio_table)) {
  current <- ratio_table[i,]
  attribute_list <- add_attribute(attribute_name = current$attribute_name,
                                  attribute_definition = current$attribute_definition,
                                  storage_type = current$storage_type,
                                  measurement_scale = current$measurement_scale,
                                  type = current$type,
                                  units = current$units,
                                  unit_precision = current$unit_precision,
                                  number_type = current$number_type,
                                  minimum = current$minimum,
                                  maximum = current$maximum,
                                  attribute_label = current$attribute_label)
}
attribute_list

## dateTime

dateTime_table <- read_excel("data-raw/template/template.xlsx", sheet = "dateTime")
for (i in 1:nrow(dateTime_table)) {
  current <- dateTime_table[i,]
  attribute_list <- add_attribute(attribute_name = current$attribute_name,
                                  attribute_definition = current$attribute_definition,
                                  storage_type = current$storage_type,
                                  measurement_scale = current$measurement_scale,
                                  date_time_format = current$date_time_format,
                                  date_time_precision = current$date_time_precision,
                                  minimum = current$minimum,
                                  maximum = current$maximum,
                                  attribute_label = current$attribute_label)
}
attribute_list

attribute_list %>%
  EML::write_eml('eml.xml')

parent_element %>%
  EML::write_eml('eml.xml')

# keyword_table <- read_excel("data-raw/template/template.xlsx", sheet = "keyword")
# for (i in 1:nrow(keyword_table)) {
#   current <- keyword_table[,i]
#   parent_element <- add_keyword_set(parent_element = parent_element,
#                                     keyword_set = current$keyword_set)
# }
# parent_element


# taxonomic_coverage <- list()
# taxonomic_coverage_table <- read_excel("data-raw/template/template.xlsx", sheet = "taxonomic_coverage")
# taxonomic_coverage_table %>%
#   pwalk(function(...) {
#     current <- tibble(...)
#     taxonomic_coverage <<- add_taxonomic_coverage(kingdum_value = current$kingdum_value, 
#                                      phylum_value = current$phylum_value, 
#                                      class_value = current$class_value, 
#                                      order_value = current$order_value, 
#                                      family_value = current$family_value,
#                                      genus_value = current$genus_value,
#                                      species_value = current$species_value,
#                                      common_name = current$common_name,
#                                      taxon_id = current$taxon_id)
#   })
# taxonomic_coverage


coverage_table <- read_excel("data-raw/template/template.xlsx", sheet = "coverage")

chinook <- add_taxonomic_coverage(CVPIA_common_species = "chinook")

steelhead <- add_taxonomic_coverage(CVPIA_common_species = "steelhead")

taxonomic_coverage <- list(chinook, steelhead)

parent_element %>%
  EML::write_eml('eml.xml')



parent_element <- list()
for (i in 1:nrow(coverage_table)) {
  current <- coverage_table[i, ]
  parent_element <- add_coverage(parent_element = parent_element,
                                 geographic_description = current$geographic_description,
                                 west_bounding_coordinate = current$west_bounding_coordinate,
                                 east_bounding_coordinate = current$east_bounding_coordinate,
                                 north_bounding_coordinate = current$north_bounding_coordinate,
                                 south_bounding_coordinate = current$south_bounding_coordinate,
                                 begin_date = current$begin_date,
                                 end_date = current$end_date,
                                 taxonomic_coverage = for (i in 1:2) {
                                   taxonomic_coverage <- taxonomic_coverage[i]
                                 })
}
parent_element


for (i in 1:nrow(funding_table)) {
  current <- funding_table[i, ]
  parent_element <- add_funding(parent_element = parent_element,
                                funder_name = current$funder_name,
                                funder_identifier = current$funder_identifier,
                                award_number = current$award_number,
                                award_title = current$award_title,
                                award_url = current$award_url,
                                funding_description = current$funding_description)
}
parent_element


for (i in 1:nrow(funding_table)) {
  current <- funding_table[i, ]
  print(current$award_number)
}
parent_element

taxonomic_coverage <- add_taxonomic_coverage(CVPIA_common_species = "chinook")
parent_element <- add_coverage(parent_element = list(), geographic_description = "Description",
                          west_bounding_coordinate = "-160.594000", 
                          east_bounding_coordinate = "-134.104800",
                          north_bounding_coordinate = "71.238300",
                          south_bounding_coordinate = "67.865000",
                          begin_date = "1980-01-01", end_date = "2010-12-31", taxonomic_coverage = taxonomic_coverage)

