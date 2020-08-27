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
title <- read_excel("data-raw/template/template.xlsx", sheet = "title")
parent_element <- add_title(parent_element = parent_element, title = title$title, 
                            short_name = title$short_name)

#Append Keyword Set 

keyword_set <- read_excel("data-raw/template/template.xlsx", sheet = "keyword")
thesaurus <- unique(keyword_set$keywordThesaurus)
for (i in 1:length(thesaurus)) {
  keywords <- keyword_set$keyword[keyword_set$keywordThesaurus == thesaurus[i]]
  parent_element <- add_keyword_set(parent_element = parent_element, 
                                    keyword_set = list(keyword = keywords, 
                                                       keywordThesaurus = thesaurus[i]))
}


#Append Abstract 
# rmarkdown::pandoc_convert('data-raw/template/abstract_template.docx', to = 'markdown', 
#                           wd = getwd(), output = 'data-raw/template/abstract.md')
# 
# abstract <- read_file('data-raw/template/abstract.md')
# parent_element <- add_abstract(parent_element = parent_element, abstract = abstract)
parent_element <- add_abstract(parent_element = parent_element, 
                               abstract = "/Users/lizzyshaw/FlowWest/cvpiaEDIutils/data-raw/template/abstract_template.docx")

#Append License and Intellectual Rights Information 
license <- read_excel("data-raw/template/template.xlsx", sheet = "license")
parent_element <- add_license(parent_element = parent_element, default_license = license$default_license)

#Append Funding Information
funding <- read_excel("data-raw/template/template.xlsx", sheet = "funding")
parent_element <- add_funding(parent_element = parent_element, funder_name = funding$funder,
                              funder_identifier = funding$funder_identifier,
                              award_number = funding$award_number,
                              award_title = funding$award_title,
                              award_url = funding$award_url,
                              funding_description = funding$funding_description)

#Append Maintenance Information 
maintenance <- read_excel("data-raw/template/template.xlsx", sheet = "maintenance")
parent_element <- add_maintenance(parent_element = parent_element, status = maintenance$status,
                                  update_frequency = maintenance$update_frequency)

#Append Method Information 
method_table <- read_excel("data-raw/template/template.xlsx", sheet = "methods")
for (i in 1:nrow(method_table)) {
  current <- method_table[i, ]
  parent_element <- add_method(parent_element = parent_element,
                               title = current$title,
                               description = current$description,
                               instrumentation = current$instrumentation)
  
}

#Append Coverage Information
taxonomic_coverage <- list()
taxonomic_coverage <- add_taxonomic_coverage(CVPIA_common_species =  cvpiaEDIutils::CVPIA_common_species$chinook)
taxonomic_coverage_table <- read_excel("data-raw/template/template.xlsx", sheet = "taxonomic_coverage")
for (i in 1:nrow(taxonomic_coverage_table)) {
  current <- taxonomic_coverage_table[i, ]
  taxonomic_coverage <- add_coverage(kingdom_value = current$kingdom_value,
                                     phylum_value = current$phylum_value,
                                     class_value = current$class_value,
                                     order_value = current$order_value,
                                     family_value = current$family_value,
                                     genus_value = current$genus_value,
                                     species_value = current$species_value,
                                     common_name = current$common_name,
                                     taxon_id = current$taxon_id)
}
taxonomic_coverage

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

## For a  nominal or ordinal an enumerated measusrement scale, use this to create code definition:
code_def_0 = list(code = "0", definition = "0 insects per meter of branch")
code_def_1 = list(code = "1", definition = "1-10 insects per meter")
code_def_2 = list(code = "2", definition = "11 – 100 insects per meter")
code_def_3 = list(code = "3", definition = "more than 100 insects per meter")
code_definition = list(code_def_0, code_def_1, code_def_2, code_def_3)

# nominal <- add_attribute(attribute_name = "site_id",
#                          attribute_definition = "Site id as used in sites table",
#                          storage_type = cvpiaEDIutils::storage_type$integer,
#                          attribute_label = "NA",
#                          measurement_scale = cvpiaEDIutils::measurement_scale$nominal,
#                          domain = "text",
#                          text_pattern = "NA",
#                          definition = "Site id as used in sites table.")
# 
# ordinal <- add_attribute(attribute_name = "hwa",
#                          attribute_definition = "Hemlock woolly adelgid density per meter of branch",
#                          storage_type = cvpiaEDIutils::storage_type$decimal,
#                          attribute_label = "NA",
#                          measurement_scale = cvpiaEDIutils::measurement_scale$ordinal,
#                          domain = "enumerated",
#                          definition = code_definition)
# 
# attribute_1_list <- list(nominal, ordinal)
# 
# interval <- add_attribute(attribute_name = "Count",
#                           attribute_definition = "Number of individuals observed",
#                           measurement_scale = cvpiaEDIutils::measurement_scale$interval,
#                           storage_type = cvpiaEDIutils::storage_type$integer,
#                           attribute_label = "NA",
#                           type = "interval",
#                           units = "number",
#                           unit_precision = "1",
#                           number_type = cvpiaEDIutils::number_type$whole,
#                           minimum = "0",
#                           maximum = "10")
# 
# ratio <- add_attribute(attribute_name = "pH",
#                        attribute_definition = "pH of soil solution",
#                        storage_type = cvpiaEDIutils::storage_type$float,
#                        measurement_scale = cvpiaEDIutils::measurement_scale$ratio,
#                        attribute_label = "NA",
#                        type = "ratio",
#                        units = "dimensionless",
#                        unit_precision = "0.01",
#                        number_type = cvpiaEDIutils::number_type$real,
#                        minimum = "0",
#                        maximum = "14")
# 
# dateTime <- add_attribute(attribute_name = "Yrs",
#                           attribute_definition = "Calendar year of the observation from years 1990 - 2010.",
#                           storage_type = cvpiaEDIutils::storage_type$integer,
#                           measurement_scale = cvpiaEDIutils::measurement_scale$dateTime,
#                           attribute_label = "Years",
#                           date_time_format = "YYYY",
#                           date_time_precision = "1",
#                           minimum = "1993",
#                           maximum = "2003")
# attribute_2_list <- list(interval, ratio, dateTime)
# 
# all_attributes <- list(attribute_1_list, attribute_2_list)
# Append Data Table Methods 

method_list <- list()
data_table_methods <- read_excel("data-raw/template/template.xlsx", sheet = "data_table_methods")
for (i in 1:nrow(data_table_methods)) {
  current <- data_table_methods[i, ]
  method_list <- add_method(parent_element = method_list,
                            title = current$title,
                            description = current$description,
                            instrumentation = current$instrumentation)
  
}
method_list

# Append Data Table 
data_table <- read_excel("data-raw/template/template.xlsx", sheet = "data_table")
for (i in 1:nrow(data_table)) {
  current <- data_table[i, ]
  phy <- physical_list[i]
  att <- attributeList[i]
  # method <- method_list$methods[i]
  parent_element <- add_data_table(parent_element = parent_element,
                                   entity_name = current$entity_name,
                                   entity_description = current$entity_description,
                                   physical = phy,
                                   attribute_list = att,
                                   # methods = method,
                                   number_of_records = current$number_of_records,
                                   alternate_identifier = current$alternate_identifier)
}
parent_element <- list()


              



# EML_doc <- function(eml = list(), parent_element) {
#   eml$dataset <- parent_element 
#   return(eml)
# }
# 
# EML <- EML_doc(parent_element = parent_element)

EML %>%
  EML::write_eml('eml.xml')

EML::eml_validate('eml.xml')


# Append Attribute Information
attribute_list <- list()
attribute_table <- read_excel("data-raw/template/template.xlsx", sheet = "attribute")
attributeList <- list()
## Nominal
for (i in 1:nrow(attribute_table)) {
  current <- attribute_table[i,]
  attributeList <- add_attribute(attribute_list = attributeList, attribute_name = current$attribute_name,
                                                       attribute_definition = current$attribute_definition,
                                                       storage_type = current$storage_type,
                                                       measurement_scale = current$measurement_scale,
                                                       domain = current$domain,
                                                       definition = current$definition,
                                                       text_pattern = current$text_pattern,
                                                       attribute_label = current$attribute_label)
}
parent_element

## Ordinal
for (i in 1:nrow(attribute_table)) {
  current <- attribute_table[i,]
  attribute_list <- add_attribute(attribute_name = current$attribute_name,
                                  attribute_definition = current$attribute_definition,
                                  storage_type = current$storage_type,
                                  measurement_scale = current$measurement_scale,
                                  domain = current$domain,
                                  definition = current$definition,
                                  text_pattern = current$text_pattern,
                                  attribute_label = current$attribute_label)
}

## Interval
for (i in 1:nrow(attribute_table)) {
  current <- attribute_table[i,]
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

## Ratio
for (i in 1:nrow(attribute_table)) {
  current <- attribute_table[i,]
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

## dateTime
for (i in 1:nrow(attribute_table)) {
  current <- attribute_table[i,]
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

for (i in 1:length(attribute_list)) {
  parent_element$attribute <- append(parent_element$attribute, attribute_list[i])
}
# 
# attribute_list %>%
#   EML::write_eml('eml.xml')
# 
# parent_element %>%
#   EML::write_eml('eml.xml')

dataset <- list(dataset = parent_element)
eml <- list(
  packageId = "uuid::UUIDgenerate()",
  system = "uuid", # type of identifier
  dataset = parent_element)

write_eml(parent_element, "eml.xml")
