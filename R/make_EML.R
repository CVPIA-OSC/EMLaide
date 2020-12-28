
# The following libraries are needed to create a working EML document. 
library(EDIutils)
library(tidyverse)
library(readxl)
library(EML)

# Our Example
#-------------------------------------------------------------------------------
# files and parameters to enter directly into the R script
excel_path <- "data-raw/Hannon-Example/example-metadata.xlsx"
sheets <- readxl::excel_sheets(excel_path)
metadata_sheets <- lapply(sheets, function(x) readxl::read_excel(excel_path, sheet = x))
names(metadata_sheets) <- sheets

abstract_docx <- "data-raw/Hannon-Example/hannon_example_abstract.docx"
methods_docx <- "data-raw/Hannon-Example/hannon_example_methods.docx"
physical_entity <- "data-raw/Hannon-Example/hannon_example_physical_data.csv"

edi_number = "edi.687.1"

# Add Access -------------------------------------------------------------------
access <- add_access()

### Add Creating the Parent Element --------------------------------------------
parent_element <- list() 

### Add Publication Date -------------------------------------------------------
parent_element <- add_pub_date(parent_element = parent_element, date = NULL)

### Add Personnel: Creator, Contact, and Associated Parties.--------------------
personnel_table <- metadata_sheets$personnel
for (i in 1:nrow(personnel_table)) {
  current <- personnel_table[i, ]
  parent_element <- add_personnel(parent_element = parent_element, 
                                  first_name = current$first_name, 
                                  last_name = current$last_name, 
                                  email = current$email,
                                  role = current$role, 
                                  organization = current$organization,
                                  orcid = current$orcid)
}

### Add Title and Short Name ---------------------------------------------------
title <- metadata_sheets$title
parent_element <- add_title(parent_element = parent_element,
                            title = title$title, 
                            short_name = title$short_name)

### Add Keyword Set ------------------------------------------------------------
keyword_set <- metadata_sheets$keyword_set
if (length(!is.na(metadata_sheets$keywordThesaurus)) == 0) {
  parent_element <- add_keyword_set(parent_element = parent_element,
                                    keyword_set = keyword_set[,1])
} else {
  thesaurus <- unique(keyword_set$keywordThesaurus)
  for (i in 1:length(thesaurus)) {
    keywords <- keyword_set$keyword[keyword_set$keywordThesaurus == thesaurus[i]]
    parent_element <- add_keyword_set(parent_element = parent_element,
                                      keyword_set = list(keyword = keywords,
                                                         keywordThesaurus = thesaurus[i]))
  }
 }

### Add Abstract ---------------------------------------------------------------
parent_element <- add_abstract(parent_element = parent_element, 
                               abstract = abstract_docx)

### Add License and Intellectual Rights ----------------------------------------
license <- metadata_sheets$license

if (license$default_license == "CCO" | license$default_license == "CCBY") {
  parent_element <- add_license(parent_element = parent_element, 
                                default_license = license$default_license)
} else {
  parent_element <- add_license(parent_element = parent_element,
                              license_name = license$license_name,
                              license_url = license$license_url,
                              license_identifier = license$license_identifier,
                              intellectual_rights_description = license$intellectual_rights_description)
}

### Add Methods ----------------------------------------------------------------
parent_element$methods <- add_method(parent_element = parent_element,
                                     methods_file = methods_docx)

### Add Maintenance 
maintenance <- metadata_sheets$maintenance
parent_element <- add_maintenance(parent_element = parent_element, 
                                  status = maintenance$status,
                                  update_frequency = maintenance$update_frequency)

### Add Project: Title, Personnel, and Funding ---------------------------------
project_title <- metadata_sheets$project$project_title

#### Add Project personnel ----------------------------------------------------- 
project_personnel_table <- metadata_sheets$project
for (i in 1:nrow(project_personnel_table)) {
  current <- project_personnel_table[i, ]
  project_personnel <- add_personnel(parent_element = list(), 
                                     first_name = current$first_name, 
                                     last_name = current$last_name, 
                                     email = current$email,
                                     role = current$role, 
                                     organization = current$organization,
                                     orcid = current$orcid)
}
if (!is.null(project_personnel$associatedParty)) {
  project_personnel <- project_personnel$associatedParty
}
if (!is.null(project_personnel$creator)){
  project_personnel <- project_personnel$creator
}

#### Add Project funding -------------------------------------------------------
funding_table <- metadata_sheets$funding
for (i in 1:nrow(funding_table)) {
  current <- funding_table[i, ]
  award_information <- add_funding(funder_name = current$funder_name,
                                   funder_identifier = current$funder_identifier,
                                   award_number = current$award_number,
                                   award_title = current$award_title,
                                   award_url = current$award_url,
                                   funding_description = current$funding_description)
} 

#### Add Combining Project Elements --------------------------------------------

# TODO change to using add_project function once merged 
# parent_element <- add_project(parent_element = parent_element,
#                               project_title = project_title,
#                               award_information = award_information,
#                               project_personnel = project_personnel)
parent_element$project <- list(title = project_title,
                               personnel = project_personnel,
                               award = award_information)

### Add Coverage: Geographic, Temporal, Taxonomic ------------------------------
taxonomic_coverage <- list()
taxonomic_coverage_table <- metadata_sheets$taxonomic_coverage
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
    taxonomic_coverage <- list(new_taxon)
  } else {
    taxonomic_coverage <- append(taxonomic_coverage, list(new_taxon))
  }
}

#### Add Combining Coverage Elements -------------------------------------------  
coverage_table <- metadata_sheets$coverage
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
                                 taxonomic_coverage = taxonomic_coverage)
} 

### Add DataTable or SpatialRaster or SpatialVector ----------------------------
#### Add Physical --------------------------------------------------------------
physical <- add_physical(file_path = physical_entity, 
                         data_url = current$data_url)

#### Add Attribute List --------------------------------------------------------
attribute_table <- metadata_sheets$attribute
codes <- metadata_sheets$code_definitions

attribute_list <- list()
attribute_names <- unique(codes$attribute_name)

for (i in 1:nrow(attribute_table)) {
  current <- attribute_table[i, ]
  if (current$domain %in% "enumerated") { 
    definition <- list()
    current_codes <- codes[codes$attribute_name == current$attribute_name, ]
    for (j in 1:nrow(current_codes)) {
      codeDefinition <- list(code = current_codes$code[j], definition = current_codes$definitions[j])
      if (is.null(definition$codeDefinition)) {
        definition$codeDefinition <- codeDefinition
      } else {
        definition$codeDefinition <- list(definition$codeDefinition, codeDefinition)
      }
    }
  } else {
    definition = current$attribute_definition
  }
  new_attribute <- add_attribute(attribute_name = current$attribute_name,
                                 attribute_definition = current$attribute_definition,
                                 storage_type = current$storage_type,
                                 measurement_scale = current$measurement_scale,
                                 domain = current$domain,
                                 definition = definition,
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

#### Putting the Data Table Together -------------------------------------------
if (metadata_sheets$dataset$type == "tabular") {
  parent_element$dataTable <- list(entityName = physical_entity,
                                   entityDescription = metadata_sheets$dataset$name,
                                   physical = physical,
                                   attributeList = attribute_list)
} 
if (metadata_sheets$dataset$type == "vector") {
  parent_element$spatialVector <- list(entityName = physical_entity,
                                       entityDescription = metadata_sheets$dataset$name,
                                       physical = physical,
                                       attributeList = attribute_list,
                                       geometry = metadata_sheets$dataset$geometry)
}

## TODO RASTER DATA 

## Making the EML document -----------------------------------------------------
eml <- list(packageId = edi_number,
            system = "EDI",
            access = access,
            dataset = parent_element)

file_name <- paste(edi_number, "xml", sep = ".")
EML::write_eml(eml, file_name)
eml_validate(file_name)



