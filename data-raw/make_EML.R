
# The following libraries are needed to create a working EML document. 
library(EMLaide)
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

# EDI number -------------------------------------------------------------------
edi_number = "edi.687.1"

# Add Access -------------------------------------------------------------------
access <- add_access()

### Add Publication Date -------------------------------------------------------
pub_date <- add_pub_date(list())

### Add Personnel: Creator, Contact, and Associated Parties.--------------------
personnel <- list()
adds_person <- function(first_name, last_name, email, role, organization, orcid) {
  personnel <- add_personnel(personnel, first_name, last_name, email,
                             role, organization, orcid)
}
personnel <- purrr::pmap(metadata$personnel, adds_person) %>% flatten()

### Add Title and Short Name ---------------------------------------------------
title <- add_title(list(), title = metadata$title$title,  
                   short_name = metadata$title$short_name)

### Add Keyword Set ------------------------------------------------------------
unique_thesaurus <- unique(metadata$keyword_set$keywordThesaurus)
keywords <- list()
add_keywords = function(unique_thesaurus){
  keywords <- add_keyword_set(keywords, 
                              keyword_set = list(keyword = metadata$keyword_set$keyword[keyword_set$keywordThesaurus == unique_thesaurus],
                                                 keywordThesaurus = unique_thesaurus))
}
all_keywords <- purrr::map(unique_thesaurus, add_keywords) %>% flatten()
### Add Abstract ---------------------------------------------------------------
abstract <- add_abstract(list(), abstract = abstract_docx)

### Add License and Intellectual Rights ----------------------------------------
license <- add_license(list(), default_license = metadata$license$default_license)

### Add Methods ----------------------------------------------------------------
methods <- add_method(list(), methods_file = methods_docx)

### Add Maintenance 
maintenance <- add_maintenance(list(), status = metadata$maintenance$status,
                               update_frequency = metadata$maintenance$update_frequency)

### Add Project: 

#### Add Project personnel ----------------------------------------------------- 
project_personnel <- personnel$creator 

#### Add Project funding -------------------------------------------------------
award_information <- purrr::pmap(metadata$funding, add_funding) %>% flatten()

#### Add Combining Project Elements --------------------------------------------

project <- add_project(list(), 
                       project_title = metadata$title$short_name, 
                       award_information,
                       project_personnel)

### Add Coverage: Geographic, Temporal, Taxonomic ------------------------------
taxonomic_coverage <- purrr::pmap(metadata$taxonomic_coverage, add_taxonomic_coverage)

#### Add Combining Coverage Elements -------------------------------------------  
coverage <- add_coverage(list(),
                         geographic_description = metadata$coverage$geographic_description,
                         west_bounding_coordinate = metadata$coverage$west_bounding_coordinate,
                         east_bounding_coordinate = metadata$coverage$east_bounding_coordinate,
                         north_bounding_coordinate = metadata$coverage$north_bounding_coordinate,
                         south_bounding_coordinate = metadata$coverage$south_bounding_coordinate,
                         begin_date = metadata$coverage$begin_date,
                         end_date = metadata$coverage$end_date,
                         taxonomic_coverage = taxonomic_coverage)

### Add DataTable or SpatialRaster or SpatialVector ----------------------------
#### Add Physical --------------------------------------------------------------
physical <- add_physical(file_path = dataset_file, data_url = NULL)


#### Add Attribute List --------------------------------------------------------
# Create helper function to add code definitions if domain is "enumerated"
code_helper <- function(code, definitions, attribute_name) {
  codeDefinition <- list(code = code, definition = definitions)
}

attribute_list <- list()
# Adds both enumerated and non enumerated attributes 
adds_attribute <- function(attribute_name, attribute_definition, storage_type, 
                           measurement_scale, domain, type, units,
                           unit_precision, number_type, date_time_format, 
                           date_time_precision, minimum, maximum,
                           attribute_label){
  # If statement adds definition for enumerated attribute using code_helper()
  if (domain %in% "enumerated") { 
    definition <- list()
    codes <- metadata$code_definitions
    current_codes <- codes[codes$attribute_name == attribute_name, ] 
    definition$codeDefinition <- purrr::pmap(current_codes, code_helper) 
    # Else statement adds definition for non-enumerated attribute 
  } else {
    definition = attribute_definition
  }
  new_attribute <- add_attribute(attribute_name = attribute_name, 
                                 attribute_definition = attribute_definition,
                                 storage_type = storage_type,
                                 measurement_scale = measurement_scale, 
                                 domain = domain, definition = definition, 
                                 type = type, units = units, 
                                 unit_precision = unit_precision, 
                                 number_type = number_type, 
                                 date_time_format = date_time_format, 
                                 date_time_precision = date_time_precision, 
                                 minimum = minimum, maximum = maximum, 
                                 attribute_label = attribute_label)
}
# Maps through entire attribute sheet adding to attribute_list
attribute_list$attribute <- purrr::pmap(metadata$attribute, adds_attribute) 

#### Putting the Data Table Together -------------------------------------------
dataTable <- list(entityName = dataset_file_name,
                  entityDescription = metadata$dataset$name,
                  physical = physical,
                  attributeList = attribute_list)

# Appending all to dataset list
dataset <- list(title = title$title,
                shortName = title$shortName,
                creator = personnel$creator,
                contact = personnel$contact,
                pubDate = pub_date,
                abstract = abstract$abstract,
                associatedParty = list(personnel[[3]], personnel[[4]], personnel[[5]]),
                keywordSet = keywords$keywordSet,
                coverage = coverage$coverage,
                intellectualRights = license$intellectualRights,
                licensed = license$licensed,
                methods = methods,
                maintenance = maintenance$maintenance,
                dataTable = dataTable) 

## Making the EML document -----------------------------------------------------
eml <- list(packageId = edi_number,
            system = "EDI",
            access = access,
            dataset = dataset)

file_name <- paste(edi_number, "xml", sep = ".")
EML::write_eml(eml, file_name)
eml_validate(file_name)



