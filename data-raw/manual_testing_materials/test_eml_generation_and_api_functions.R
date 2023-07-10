library(EMLaide)
library(tidyverse)
library(readxl)
library(EML)

# read in files (located in inst/ext-data)
# datatable metadata objects 
datatable_metadata <- 
  dplyr::tibble(filepath = c(system.file("extdata", "Banet-Example", "data", "enclosure-study-growth-rate-data.csv", 
                                         package = "EMLaide", mustWork = TRUE),
                             system.file("extdata", "Banet-Example", "data", "enclosure-study-gut-contents-data.csv", 
                                         package = "EMLaide", mustWork = TRUE),
                             system.file("extdata", "Banet-Example", "data", "microhabitat-use-data-2018-2020.csv", 
                                         package = "EMLaide", mustWork = TRUE),
                             system.file("extdata", "Banet-Example", "data", "seining-weight-lengths-2018-2020.csv", 
                                         package = "EMLaide", mustWork = TRUE),
                             system.file("extdata", "Banet-Example", "data", "snorkel-index-data-2015-2020.csv", 
                                         package = "EMLaide", mustWork = TRUE)),  
                attribute_info = c(system.file("extdata", "Banet-Example", 
                                               "metadata", "enclosure-study-growth-rates-metadata.xlsx", 
                                               package = "EMLaide", mustWork = TRUE),
                                   system.file("extdata", "Banet-Example", "metadata", "enclosure-study-gut-contents-metadata.xlsx", 
                                               package = "EMLaide", mustWork = TRUE),
                                   system.file("extdata", "Banet-Example", "metadata", "microhabitat-use-metadata.xlsx", 
                                               package = "EMLaide", mustWork = TRUE),
                                   system.file("extdata", "Banet-Example", "metadata", "seining-weight-length-metadata.xlsx", 
                                               package = "EMLaide", mustWork = TRUE),
                                   system.file("extdata", "Banet-Example", "metadata", "snorkel-index-metadata.xlsx", 
                                               package = "EMLaide", mustWork = TRUE)),
                datatable_description = c("Growth Rates - Enclosure Study",
                                          "Gut Contents - Enclosure Study",
                                          "Microhabitat Data",
                                          "Seining Weight Lengths Data",
                                          "Snorkel Survey Data"),
                datatable_url = paste0("https://raw.githubusercontent.com/FlowWest/edi.749.1/main/data/",
                                       c("enclosure-study-growth-rate-data.csv",
                                         "enclosure-study-gut-contents-data.csv",
                                         "microhabitat-use-data-2018-2020.csv",
                                         "seining-weight-lengths-2018-2020.csv",
                                         "snorkel-index-data-2015-2020.csv")))
# package metadata objects 
excel_path <- system.file("extdata", "Banet-Example", "metadata", "data-package-metadata.xlsx", 
                          package = "EMLaide", mustWork = TRUE) 
sheets <- readxl::excel_sheets(excel_path)
metadata <- purrr::map(sheets, function(x) readxl::read_excel(excel_path, sheet = x))
names(metadata) <- sheets

abstract_docx <- system.file("extdata", "Banet-Example", "metadata","abstract.docx", 
                             package = "EMLaide", mustWork = TRUE) 

methods_docx <- system.file("extdata", "Banet-Example", "metadata", "methods.docx", 
                            package = "EMLaide", mustWork = TRUE)

# Rerun and replace set edi number below 
# edi_number <- reserve_edi_id(user_id = Sys.getenv("user_id"), password = Sys.getenv("password"), environment = "staging")
edi_number <- "edi.1046.1"

# generate master list of metadata elements 
dataset <- list() %>%
  add_pub_date() %>%
  add_title(metadata$title) %>%
  add_personnel(metadata$personnel) %>%
  add_keyword_set(metadata$keyword_set) %>%
  add_abstract(abstract_docx) %>%
  add_license(metadata$license) %>%
  add_method(methods_docx) %>%
  add_maintenance(metadata$maintenance) %>%
  add_project(metadata$funding) %>%
  add_coverage(metadata$coverage, metadata$taxonomic_coverage) %>%
  add_datatable(datatable_metadata)

# specify custom units
custom_units <- data.frame(id = c("fishPerEnclosure", "thermal unit", "day", "fishPerSchool"),
                           unitType = c("density", "temperature", "dimensionless", "density"),
                           parentSI = c(NA, NA, NA, NA),
                           multiplierToSI = c(NA, NA, NA, NA),
                           description = c("Fish density in the enclosure, number of fish in total enclosure space",
                                           "thermal unit of energy given off of fish",
                                           "count of number of days that go by",
                                           "Number of fish counted per school"))

unitList <- EML::set_unitList(custom_units)

# create full eml list 
eml <- list(packageId = edi_number,
            system = "EDI",
            access = add_access(),
            dataset = dataset,
            additionalMetadata = list(metadata = list(unitList = unitList)))

# write to EML document and check that it is valid
EML::write_eml(eml, paste0("data-raw/manual_testing_materials/", edi_number, ".xml"))
EML::eml_validate(paste0("data-raw/manual_testing_materials/", edi_number, ".xml"))

# Test that it passes EDIs evaluation criteria 
evaluate_edi_package(user_id = Sys.getenv("user_id"), 
                     password = Sys.getenv("password"),
                     eml_file_path = paste0("data-raw/manual_testing_materials/", edi_number, ".xml"), 
                     environment = "staging")

# Upload package 
upload_edi_package(user_id = Sys.getenv("user_id"), 
                     password = Sys.getenv("password"),
                     eml_file_path = paste0("data-raw/manual_testing_materials/", edi_number, ".xml"), 
                     environment = "staging",
                     package_size = "medium")

past_version <- paste0("edi.", unlist(strsplit(edi_number, "\\."))[2], ".", as.character(as.numeric(unlist(strsplit(edi_number, "\\."))[3]) - 1))
# update edi package 
update_edi_package(user_id = Sys.getenv("user_id"), 
                   password = Sys.getenv("password"),
                   eml_file_path = paste0("data-raw/manual_testing_materials/", edi_number, ".xml"),
                   package_size = "medium", 
                   existing_package_identifier = past_version, 
                   environment = "staging")