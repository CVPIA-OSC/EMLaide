library(tidyverse)
library(readxl)
library(EML)

parent_element <- list() 

#Append Publication date 
parent_element <- add_pub_date(parent_element = parent_element, date = NULL)

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

keyword_set <- read_excel("data-raw/template/template.xlsx", sheet = "keyword_set")
thesaurus <- unique(keyword_set$keywordThesaurus)
for (i in 1:length(thesaurus)) {
  keywords <- keyword_set$keyword[keyword_set$keywordThesaurus == thesaurus[i]]
  parent_element <- add_keyword_set(parent_element = parent_element, 
                                    keyword_set = list(keyword = keywords, 
                                                       keywordThesaurus = thesaurus[i]))
}

#Append Abstract 
setwd("~/FlowWest/cvpiaEDIutils")
parent_element <- add_abstract(parent_element = parent_element, 
                               abstract = "inst/extdata/abstract_template.docx")

#Append License and Intellectual Rights Information 
license <- read_excel("data-raw/template/template.xlsx", sheet = "license")
parent_element <- add_license(parent_element = parent_element, default_license = license$default_license)

#Append Funding Information
funding <- read_excel("data-raw/template/template.xlsx", sheet = "funding")
parent_element <- add_funding(parent_element = parent_element, 
                              funder_name = funding$funder_name,
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
setwd("~/FlowWest/cvpiaEDIutils")

for (i in 1:nrow(method_table)) {
  setwd("~/FlowWest/cvpiaEDIutils")
  current <- method_table[i, ]
  new_method <- add_method(parent_element = parent_element, methods_file = current$methods_file,
                           instrumentation = current$instrumentation)
  if (is.null(parent_element$methods)) {
    parent_element$methods <- new_method
  } else {
    parent_element$methods <- list(parent_element$methods, new_method)
  }
}

#Append Coverage Information
taxonomic_coverage <- list()
taxonomic_coverage_table <- read_excel("data-raw/template/template.xlsx", sheet = "taxonomic_coverage")
for (i in 1:nrow(taxonomic_coverage_table)) {
  current <- taxonomic_coverage_table[i, ]
  new_taxon <- add_taxonomic_coverage(CVPIA_common_species = current$CVPIA_common_species,
                                     kingdom_value = current$kingdom_value,
                                     phylum_value = current$phylum_value,
                                     class_value = current$class_value,
                                     order_value = current$order_value,
                                     family_value = current$family_value,
                                     genus_value = current$genus_value,
                                     species_value = current$species_value,
                                     common_name = current$common_name,
                                     taxon_id = current$taxon_id)
  if (is.null(taxonomic_coverage)) {
    taxonomic_coverage <- new_taxon
  } else {
    taxonomic_coverage <- append(taxonomic_coverage, new_taxon)
  }
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
                                 taxonomic_coverage = tax)
}

# Append Physical Information 
physical_list <- list()
physical_table <- read_excel("data-raw/template/template.xlsx", sheet = "physical")
for (i in 1:nrow(physical_table)) {
  current <- physical_table[i,]
  new_physical <- add_physical(file_path = current$file_path, 
                               number_of_headers = current$number_of_headers,
                               record_delimiter = current$record_delimiter,
                               attribute_orientation = current$attribute_orientation,
                               field_delimiter = current$field_delimiter,
                               data_url = current$data_url)
  if (is.null(physical_list$physical)) {
    physical_list$physical <- new_physical
  } else {
    physical_list$physical <- list(physical_list$physical, new_physical)
  }
}

# Append Attribute Information  

## For a  nominal or ordinal an enumerated measusrement scale, use this to create code definition:
code_def_0 = list(code = "0", definition = "0 insects per meter of branch")
code_def_1 = list(code = "1", definition = "1-10 insects per meter")
code_def_2 = list(code = "2", definition = "11 – 100 insects per meter")
code_def_3 = list(code = "3", definition = "more than 100 insects per meter")
code_definition = list(code_def_0, code_def_1, code_def_2, code_def_3)

# Append Attributes 

attribute_list <- list()
attribute_table <- read_excel("data-raw/template/template.xlsx", sheet = "attribute")
for (i in 1:nrow(attribute_table)) {
  current <- attribute_table[i,]
  new_attribute <- add_attribute(attribute_name = current$attribute_name,
                                 attribute_definition = current$attribute_definition,
                                 storage_type = current$storage_type,
                                 measurement_scale = current$measurement_scale,
                                 domain = current$domain,
                                 definition = current$definition,
                                 text_pattern = current$text_pattern,
                                 type = current$type,
                                 units = current$units,
                                 unit_precision = current$unit_precision,
                                 number_type = current$number_type,
                                 date_time_format = current$date_time_format,
                                 date_time_precision = current$date_time_precision,
                                 minimum = current$minimum,
                                 maximum = current$maximum,
                                 attribute_label = current$attribute_label)
  if (is.null(attribute_list$attribute)) {
    attribute_list$attribute <- new_attribute
  } else {
    attribute_list$attribute <- list(attribute_list$attribute, new_attribute)
  }
}
# Append Data Table Methods 

method_list <- list()
data_table_methods <- read_excel("data-raw/template/template.xlsx", sheet = "data_table_methods")
setwd("~/FlowWest/cvpiaEDIutils")
for (i in 1:nrow(data_table_methods)) {
  setwd("~/FlowWest/cvpiaEDIutils")
  current <- data_table_methods[i, ]
  new_method <- add_method(parent_element = method_list, methods_file = current$methods_file,
                            instrumentation = current$instrumentation)
  if (is.null(method_list$methods)) {
    method_list$methods <- new_method
  } else {
    method_list$methods <- list(method_list$methods, new_method)
  }
}
method_list

# Append Data Table 
data_table <- read_excel("data-raw/template/template.xlsx", sheet = "data_table")
for (i in 1:nrow(data_table)) {
  current <- data_table[i, ]
  phy <- physical_list[i]
  att <- attribute_list[i]
  method <- method_list[i]
  parent_element <- add_data_table(parent_element = parent_element,
                                   entity_name = current$entity_name,
                                   entity_description = current$entity_description,
                                   physical = phy,
                                   attribute_list = att,
                                   methods = method,
                                   number_of_records = current$number_of_records,
                                   alternate_identifier = current$alternate_identifier)
}

# dataset <- list(dataset = parent_element)
eml <- list(
  packageId = "uuid::UUIDgenerate()",
  system = "uuid", # type of identifier
  dataset = parent_element)

write_eml(eml, "eml.xml")
EML::eml_validate('eml.xml')
