library(EMLaide)
library(tidyverse)
library(EDIutils)
library(readxl)
library(EML)
# load files to input into emlaide add element funcitons 
excel_path <- "data-raw/Hannon-Example/enclosure-study-growth-rates-metadata.xlsx"
sheets <- readxl::excel_sheets(excel_path)
metadata <- lapply(sheets, function(x) readxl::read_excel(excel_path, sheet = x))
names(metadata) <- sheets

abstract_doc <- "data-raw/Hannon-Example/hannon_example_abstract.docx"
method_doc <- "data-raw/Hannon-Example/hannon_example_methods.docx"

# Add all datatables and associated metadata in a datatable_metadata tibble to be used by the add_datatable() function
datatable_metadata <- dplyr::tibble(filepath =  c("data-raw/Hannon-Example/enclosure-study-growth-rate-data.csv",
                                                  "data-raw/Hannon-Example/enclosure-study-gut-contents-data.csv"), 
                                    attribute_info = c("data-raw/Hannon-Example/enclosure-study-growth-rates-metadata.xlsx",
                                                       "data-raw/Hannon-Example/enclosure-study-gut-contents-metadata.xlsx"),
                      datatable_description = c("Growth Rates - Enclosure Study",
                                                "Gut Contents - Enclosure Study"),
                      datatable_url = c("https://raw.githubusercontent.com/FlowWest/CVPIA_Salmonid_Habitat_Monitoring/make-xml/data/enclosure-study-growth-rate-data.csv?token=AMGEQ7R4E5RMNKRMD57BBQTAOSW6W",
                                        "https://raw.githubusercontent.com/FlowWest/CVPIA_Salmonid_Habitat_Monitoring/make-xml/data/enclosure-study-gut-contents-data.csv?token=AMGEQ7VJADFEYARKPUM4AYTAOSXAQ"))

# Create dataset list and pipe on metadata elements 
dataset <- list() %>% 
  add_pub_date() %>% 
  add_title(metadata$title) %>%
  add_personnel(metadata$personnel) %>%
  add_keyword_set(metadata$keyword_set) %>%
  add_abstract(abstract_doc) %>%
  add_license(metadata$license) %>%
  add_method(method_doc) %>%
  add_maintenance(metadata$maintenance) %>%
  add_project(metadata$funding) %>%
  add_coverage(metadata$coverage, metadata$taxonomic_coverage) %>%
  add_datatable(datatable_metadata)

# Create custom units to add to the additional metadata section of EML 
custom_units <- data.frame(id = c("fishPerEnclosure", "thermal unit", "day", "fishPerSchool"),
                           unitType = c("density", "temperature", "dimensionless", "density"),
                           parentSI = c(NA, NA, NA, NA),
                           multiplierToSI = c(NA, NA, NA, NA),
                           description = c("Fish density in the enclosure, number of fish in total enclosure space", 
                                           "thermal unit of energy given off of fish",
                                           "count of number of days that go by",
                                           "Number of fish counted per school"))

unitList <- EML::set_unitList(custom_units)

# Add dataset and additiobal elements of eml to eml list 
eml <- list(packageId = "edi.749.1",
            system = "EDI",
            access = add_access(),
            dataset = dataset,
            additionalMetadata = list(metadata = list(
              unitList = unitList)))

# Write and validate EML 
EML::write_eml(eml, "edi.749.1.xml")
EML::eml_validate("edi.749.1.xml")

