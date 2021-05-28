library(testthat)

parent_element <- list()

# Load in all metadata elements 
title_metadata <- structure(list(title = "Salmonid habitat use monitoring used to determine effectiveness of habitat improvement projects in the Sacramento River, CA", 
                                 short_name = "CVPIA Salmonid Habitat Monitoring Data"), 
                            row.names = c(NA, -1L), 
                            class = c("tbl_df", "tbl", "data.frame"))
keyword_metadata <- structure(list(keyword = c("Sacramento River", "Salmonid Habitat Restoration Projects", 
                                               "Effectiveness Monitoring", "Pacific Salmon", "CVPIA"), 
                                   keywordThesaurus = c(NA, NA, NA, NA, NA)), row.names = c(NA, -5L), class = c("tbl_df", "tbl", "data.frame"))
abstract_docx <- "abstract_test.docx"
license_metadata <- structure(list(default_license = "CCO", license_name = NA, license_url = NA, 
                                   license_identifier = NA, intellectual_rights_description = NA), row.names = c(NA, -1L), class = c("tbl_df", "tbl", "data.frame"))
method_docx <- "methods_test.docx"
maintenance_metadata <- structure(list(status = "ongoing", update_frequency = "annually"), 
                                  row.names = c(NA, -1L), class = c("tbl_df", "tbl", "data.frame"))
funding_metadata <- structure(list(funder_name = "USBR", funder_identifier = NA, 
                                   award_number = "R14AC00096", award_title = "Salmonid Spawning and Rearing Habitat Restoration in the Sacramento River", 
                                   award_url = NA, funding_description = NA), row.names = c(NA, -1L), class = c("tbl_df", "tbl", "data.frame"))
personnel_metadata <- structure(list(first_name = c("Amanda", "John", "Steve", "Ryan"), 
                                     last_name = c("Banet", "Hannon", "Tussing", "Greathouse"), 
                                     email = c("abanet@csuchico edu", "jhannon@usbr.gov", "sptussing@earthlink.net", "rgreathouse@psmfc.org"), 
                                     role = c("creator", "program founder",  "other", "other"), 
                                     organization = c("CSU Chico", "US Bureau of Reclamation","Tussing Ecological Services", "PSMFC"), 
                                     orcid = c("https://orcid.org/0000-0002-6225-2057", "http://orcid.org/0000-0001-9057-9084", NA, NA)), 
                                row.names = c(NA, -4L), class = c("tbl_df", "tbl", "data.frame"))
coverage_metadata <- structure(list(geographic_description = "Sacramento Riverin northern California in the reach of the river between Keswick Dam and Sacramento", 
                                    west_bounding_coordinate = -122.448217, east_bounding_coordinate = -121.484446, 
                                    north_bounding_coordinate = 40.612354, south_bounding_coordinate = 38.509402, 
                                    begin_date = structure(1449532800, class = c("POSIXct", "POSIXt"), 
                                                           tzone = "UTC"), 
                                    end_date = structure(1593648000, class = c("POSIXct", "POSIXt"), tzone = "UTC")), row.names = c(NA, -1L), 
                               class = c("tbl_df", "tbl", "data.frame"))

taxonomic_metadata <- structure(list(CVPIA_common_species = c("chinook", "steelhead"), common_name = c(NA, NA), kingdom = c(NA, NA), phylum = c(NA, NA), class = c(NA, NA), order = c(NA, NA), family = c(NA, NA), genus = c(NA, NA), species = c(NA, NA), taxon_id = c(NA,NA)), row.names = c(NA, -2L), class = c("tbl_df", "tbl", "data.frame"))


# Tests individual add functions
test_that("Add pub date adds the correct pub date to parent element", {
  expected_return <- list(pubDate = structure(Sys.Date(), class = "Date"))
  expect_equal(add_pub_date(parent_element), expected_return)
})

test_that("Add title adds the correct title to parent element", {
  expected_title <- list(title = "Salmonid habitat use monitoring used to determine effectiveness of habitat improvement projects in the Sacramento River, CA", 
                        shortName = "CVPIA Salmonid Habitat Monitoring Data")
  expect_equal(add_title(parent_element, title_metadata), expected_title)
})

test_that("Add personnel adds the correct personell to parent element", {
  expected_personnel <- list(creator = list(individualName = list(givenName = "Amanda", 
                                                                  surName = "Banet"), electronicMailAddress = "abanet@csuchico edu", 
                                            organizationName = "CSU Chico", `@id` = "https://orcid.org/0000-0002-6225-2057"), 
                             contact = list(individualName = list(givenName = "Amanda", 
                                                                  surName = "Banet"), electronicMailAddress = "abanet@csuchico edu", 
                                            organizationName = "CSU Chico"), 
                             associatedParty = list(list(individualName = list(givenName = "John", surName = "Hannon"), 
                                                   electronicMailAddress = "jhannon@usbr.gov", organizationName = "US Bureau of Reclamation", 
                                                   `@id` = "http://orcid.org/0000-0001-9057-9084", role = "program founder"), 
                                               list(individualName = list(givenName = "Steve", surName = "Tussing"), 
                                                   electronicMailAddress = "sptussing@earthlink.net", 
                                                   organizationName = "Tussing Ecological Services", 
                                                   role = "other"), 
                                               list(individualName = list(givenName = "Ryan", surName = "Greathouse"), 
                                                    electronicMailAddress = "rgreathouse@psmfc.org", organizationName = "PSMFC", role = "other")))
  expect_equal(add_personnel(parent_element, personnel_metadata), expected_personnel)
})


test_that("Add keyword set adds the correct keyword set to the parent element", {
  expected_keywords <- list(keywordSet = list(list(keyword = c("Sacramento River", "Salmonid Habitat Restoration Projects", 
                                                               "Effectiveness Monitoring", "Pacific Salmon", "CVPIA"))))
  expect_equal(add_keyword_set(parent_element, keyword_metadata), expected_keywords)
})

test_that("Add abstract adds the appropriate abstract to the parent element", {
  expected_abstract <- list(abstract = list(section = list(), para = list("\n    This is a test for the abstract. This abstract is of sufficient\n    length. Please make sure your abstract meets the word limit of\n    twenty words or more.\n  ")))
  expect_equal(add_abstract(parent_element, abstract_docx), expected_abstract)
})

test_that("Add license adds the appropriate license to the parent element", {
  expected_license <- list(intellectualRights = list(para = "This data package is released to the \"public domain\" under Creative Commons CC0 1.0 \"No Rights Reserved\" (see: https://creativecommons.org/publicdomain/zero/1.0/). It is considered professional etiquette to provide attribution of the original work if this data package is shared in whole or by individual components. A generic citation is provided for this data package on the website https://portal.edirepository.org (herein \"website\") in the summary metadata page. Communication (and collaboration) with the creators of this data package is recommended to prevent duplicate research or publication. This data package (and its components) is made available \"as is\" and with no warranty of accuracy or fitness for use. The creators of this data package and the website shall not be liable for any damages resulting from misinterpretation or misuse of the data package or its components. Periodic updates of this data package may be available from the website. Thank you."), 
                           licensed = list(licenseName = "Creative Commons Zero v1.0 Universal", 
                                           url = "https://spdx.org/licenses/CC0-1.0.html", identifier = "CC0-1.0"))
  expect_equal(add_license(parent_element, license_metadata), expected_license)
})

test_that("Add method adds the appropriate methods to the parent element", {
  expected_method <- list(methods = list(sampling = NULL, methodStep = list(instrumentation = character(0), 
                                                                            software = NULL, description = list(section = list("<title>Title 1</title>\n<para>\n      This is the first paragraph.\n    </para>\n<para>\n      This is the second paragraph.\n    </para>", 
                                                                                                                               "<title>Title 2 </title>\n<para>\n      This is the third paragraph.\n    </para>\n<para>\n      This is the fourth paragraph.\n    </para>"), 
                                                                                                                para = list()))))
  expect_equal(add_method(parent_element, method_docx), expected_method)
})

test_that("Add maintenance adds the appropriate maintenance infomration to the parent element", {
  expected_maintenance <- list(maintenance = list(description = "ongoing", maintenanceUpdateFrequency = "annually"))
  expect_equal(add_maintenance(parent_element, maintenance_metadata), expected_maintenance)
})

test_that("Add project adds the approprite project information to the parent element", {
  expected_project <- list(project = list(title = "My project", personnel = list(individualName = list(givenName = "Amanda", surName = "Banet"), 
    electronicMailAddress = "abanet@csuchico edu", 
    organizationName ="CSU Chico", role = "Project Lead"), 
    award = list(list(funderName = "United States Bureau of Reclamation", 
                      funderIdentifier = "https://www.wikidata.org/wiki/Q1010548", 
                      title = "Salmonid Spawning and Rearing Habitat Restoration in the Sacramento River", 
                      awardNumber = "R14AC00096", description = NA, awardUrl = NA))))
  expect_equal(add_project(parent_element, funding_metadata, project_title = "My project", project_lead = personnel_metadata), expected_project)
})

test_that("Add coverage adds the appropriate coverage information to the parent element", {
  expected_coverage <-list(coverage = list(geographicCoverage = list(geographicDescription = "Sacramento Riverin northern California in the reach of the river between Keswick Dam and Sacramento", 
                                                                     boundingCoordinates = list(westBoundingCoordinate = -122.448217, 
                                                                                                eastBoundingCoordinate = -121.484446, 
                                                                                                northBoundingCoordinate = 40.612354, 
                                                                                                southBoundingCoordinate = 38.509402)), 
                                           temporalCoverage = list(rangeOfDates = list(beginDate = list(calendarDate = structure(1449532800, class = c("POSIXct", "POSIXt"), tzone = "UTC")), 
                                                                                       endDate = list(calendarDate = structure(1593648000, class = c("POSIXct", "POSIXt"), tzone = "UTC")))), 
                                           taxonomicCoverage = list(
                                             list(taxonomicClassification = list(taxonRankName = "kingdom", 
                                             taxonRankValue = "Animalia", taxonomicClassification = list(
                                               taxonRankName = "phylum", taxonRankValue = "Chordata", 
                                               taxonomicClassification = list(taxonRankName = "class", 
                                                                              taxonRankValue = "Teleostei", taxonomicClassification = list(
                                                                                taxonRankName = "order", taxonRankValue = "Salmoniformes", 
                                                                                taxonomicClassification = list(taxonRankName = "family", 
                                                                                                               taxonRankValue = "Salmonidae", taxonomicClassification = list(
                                                                                                                 taxonRankName = "genus", taxonRankValue = "Oncorhynchus", 
                                                                                                                 taxonomicClassification = list(taxonRankName = "species", 
                                                                                                                                                taxonRankValue = "Oncorhynchus tshawytscha", 
                                                                                                                                                commonName = "Chinook Salmon", taxonId = list(
                                                                                                                                                  provider = "https://itis.gov", taxonId = "161980"))))))))), 
                                             list(taxonomicClassification = list(taxonRankName = "kingdom", 
                                                  taxonRankValue = "Animalia", taxonomicClassification = list(
                                                    taxonRankName = "phylum", taxonRankValue = "Chordata", 
                                                    taxonomicClassification = list(taxonRankName = "class", 
                                                                                   taxonRankValue = "Teleostei", taxonomicClassification = list(
                                                                                     taxonRankName = "order", taxonRankValue = "Salmoniformes", 
                                                                                     taxonomicClassification = list(taxonRankName = "family", 
                                                                                                                    taxonRankValue = "Salmonidae", taxonomicClassification = list(
                                                                                                                      taxonRankName = "genus", taxonRankValue = "Oncorhynchus", 
                                                                                                                      taxonomicClassification = list(taxonRankName = "species", 
                                                                                                                                                     taxonRankValue = "Oncorhynchus mykiss", 
                                                                                                                                                     commonName = "Steelhead Trout", taxonId = list(
                                                                                                                                                       provider = "https://itis.gov", taxonId = "161989"))))))))))))
  expect_equal(add_coverage(parent_element, coverage_metadata, taxonomic_metadata), expected_coverage)
  })


# tests add functions in a pipe
test_that("You can append all metadata inputs (except datatable) to parent element", {
  expected_parent_element <- list(pubDate = structure(Sys.Date(), class = "Date"), title = "Salmonid habitat use monitoring used to determine effectiveness of habitat improvement projects in the Sacramento River, CA", 
                                shortName = "CVPIA Salmonid Habitat Monitoring Data", creator = list(
                                  individualName = list(givenName = "Amanda", surName = "Banet"), 
                                  electronicMailAddress = "abanet@csuchico edu", organizationName = "CSU Chico", 
                                  `@id` = "https://orcid.org/0000-0002-6225-2057"), contact = list(
                                    individualName = list(givenName = "Amanda", surName = "Banet"), 
                                    electronicMailAddress = "abanet@csuchico edu", organizationName = "CSU Chico"), 
                                associatedParty = list(list(individualName = list(givenName = "John", 
                                                                                  surName = "Hannon"), electronicMailAddress = "jhannon@usbr.gov", 
                                                            organizationName = "US Bureau of Reclamation", `@id` = "http://orcid.org/0000-0001-9057-9084", 
                                                            role = "program founder"), list(individualName = list(
                                                              givenName = "Steve", surName = "Tussing"), electronicMailAddress = "sptussing@earthlink.net", 
                                                              organizationName = "Tussing Ecological Services", role = "other"), 
                                                       list(individualName = list(givenName = "Ryan", surName = "Greathouse"), 
                                                            electronicMailAddress = "rgreathouse@psmfc.org", 
                                                            organizationName = "PSMFC", role = "other")), keywordSet = list(
                                                              list(keyword = c("Sacramento River", "Salmonid Habitat Restoration Projects", 
                                                                               "Effectiveness Monitoring", "Pacific Salmon", "CVPIA"
                                                              ))), abstract = list(section = list(), para = list("\n    This is a test for the abstract. This abstract is of sufficient\n    length. Please make sure your abstract meets the word limit of\n    twenty words or more.\n  ")), 
                                intellectualRights = list(para = "This data package is released to the \"public domain\" under Creative Commons CC0 1.0 \"No Rights Reserved\" (see: https://creativecommons.org/publicdomain/zero/1.0/). It is considered professional etiquette to provide attribution of the original work if this data package is shared in whole or by individual components. A generic citation is provided for this data package on the website https://portal.edirepository.org (herein \"website\") in the summary metadata page. Communication (and collaboration) with the creators of this data package is recommended to prevent duplicate research or publication. This data package (and its components) is made available \"as is\" and with no warranty of accuracy or fitness for use. The creators of this data package and the website shall not be liable for any damages resulting from misinterpretation or misuse of the data package or its components. Periodic updates of this data package may be available from the website. Thank you."), 
                                licensed = list(licenseName = "Creative Commons Zero v1.0 Universal", 
                                                url = "https://spdx.org/licenses/CC0-1.0.html", identifier = "CC0-1.0"), 
                                methods = list(sampling = NULL, methodStep = list(instrumentation = character(0), 
                                                                                  software = NULL, description = list(section = list("<title>Title 1</title>\n<para>\n      This is the first paragraph.\n    </para>\n<para>\n      This is the second paragraph.\n    </para>", 
                                                                                                                                     "<title>Title 2 </title>\n<para>\n      This is the third paragraph.\n    </para>\n<para>\n      This is the fourth paragraph.\n    </para>"), 
                                                                                                                      para = list()))), maintenance = list(description = "ongoing", 
                                                                                                                                                           maintenanceUpdateFrequency = "annually"), project = list(
                                                                                                                                                             title = "Salmonid habitat use monitoring used to determine effectiveness of habitat improvement projects in the Sacramento River, CA", 
                                                                                                                                                             personnel = list(individualName = list(givenName = "Amanda", 
                                                                                                                                                                                                    surName = "Banet"), electronicMailAddress = "abanet@csuchico edu", 
                                                                                                                                                                              organizationName = "CSU Chico", role = "Project Lead"), 
                                                                                                                                                             award = list(list(funderName = "United States Bureau of Reclamation", 
                                                                                                                                                                               funderIdentifier = "https://www.wikidata.org/wiki/Q1010548", 
                                                                                                                                                                               title = "Salmonid Spawning and Rearing Habitat Restoration in the Sacramento River", 
                                                                                                                                                                               awardNumber = "R14AC00096", description = NA, awardUrl = NA))), 
                                coverage = list(geographicCoverage = list(geographicDescription = "Sacramento Riverin northern California in the reach of the river between Keswick Dam and Sacramento", 
                                                                          boundingCoordinates = list(westBoundingCoordinate = -122.448217, 
                                                                                                     eastBoundingCoordinate = -121.484446, northBoundingCoordinate = 40.612354, 
                                                                                                     southBoundingCoordinate = 38.509402)), temporalCoverage = list(
                                                                                                       rangeOfDates = list(beginDate = list(calendarDate = structure(1449532800, class = c("POSIXct", 
                                                                                                                                                                                           "POSIXt"), tzone = "UTC")), 
                                                                                                                           endDate = list(calendarDate = structure(1593648000, class = c("POSIXct", 
                                                     "POSIXt"), tzone = "UTC")))), taxonomicCoverage = list(
                                                       list(taxonomicClassification = list(taxonRankName = "kingdom", 
                                                                                           taxonRankValue = "Animalia", taxonomicClassification = list(
                                                                                             taxonRankName = "phylum", taxonRankValue = "Chordata", 
                                                                                             taxonomicClassification = list(taxonRankName = "class", 
                                                                                                                            taxonRankValue = "Teleostei", taxonomicClassification = list(
                                                                                                                              taxonRankName = "order", taxonRankValue = "Salmoniformes", 
                                                                                                                              taxonomicClassification = list(taxonRankName = "family", 
                                                                                                                                                             taxonRankValue = "Salmonidae", taxonomicClassification = list(
                                                                                                                                                               taxonRankName = "genus", taxonRankValue = "Oncorhynchus", 
                                                                                                                                                               taxonomicClassification = list(taxonRankName = "species", 
                                                                                                                                                                                              taxonRankValue = "Oncorhynchus tshawytscha", 
                                                                                                                                                                                              commonName = "Chinook Salmon", taxonId = list(
                                                                                                                                                                                                provider = "https://itis.gov", taxonId = "161980"))))))))), 
                                                       list(taxonomicClassification = list(taxonRankName = "kingdom", 
                                                                                           taxonRankValue = "Animalia", taxonomicClassification = list(
                                                                                             taxonRankName = "phylum", taxonRankValue = "Chordata", 
                                                                                             taxonomicClassification = list(taxonRankName = "class", 
                                                                                                                            taxonRankValue = "Teleostei", taxonomicClassification = list(
                                                                                                                              taxonRankName = "order", taxonRankValue = "Salmoniformes", 
                                                                                                                              taxonomicClassification = list(taxonRankName = "family", 
                                                                                                                                                             taxonRankValue = "Salmonidae", taxonomicClassification = list(
                                                                                                                                                               taxonRankName = "genus", taxonRankValue = "Oncorhynchus", 
                                                                                                                                                               taxonomicClassification = list(taxonRankName = "species", 
                                                                                                                                                                                              taxonRankValue = "Oncorhynchus mykiss", 
                                                                                                                                                                                              commonName = "Steelhead Trout", taxonId = list(provider = "https://itis.gov", taxonId = "161989"))))))))))))
    dataset <- list() %>%
    add_pub_date() %>% 
    add_title(title_metadata) %>%
    add_personnel(personnel_metadata) %>%
    add_keyword_set(keyword_metadata) %>%
    add_abstract(abstract_docx) %>%
    add_license(license_metadata) %>%
    add_method(method_docx) %>%
    add_maintenance(maintenance_metadata) %>%
    add_project(funding_metadata) %>%
    add_coverage(coverage_metadata, taxonomic_metadata) 
  expect_equal(dataset, expected_parent_element)
})

# Test add datatable element 
datatable_metadata <- 
  dplyr::tibble(filepath = c(system.file("extdata", "Banet-Example", "data", "enclosure-study-growth-rate-data.csv", 
                                         package = "EMLaide", mustWork = TRUE)),  
                attribute_info = c(system.file("extdata", "Banet-Example", 
                                               "metadata", "enclosure-study-growth-rates-metadata.xlsx", 
                                               package = "EMLaide", mustWork = TRUE)),
                datatable_description = c("Growth Rates - Enclosure Study"))
expected_datatabletable <- list(dataTable = list(list(entityName = "enclosure-study-growth-rate-data.csv", 
                           entityDescription = "Growth Rates - Enclosure Study", physical = list(
                             objectName = "enclosure-study-growth-rate-data.csv", 
                             size = list(unit = "bytes", size = "32636"), authentication = list(
                               method = "MD5", authentication = "44b508c45e3635e8d65a719631530a75"), 
                             dataFormat = list(textFormat = list(numHeaderLines = "1", 
                                                                 recordDelimiter = "\\r\\n", attributeOrientation = "column", 
                                                                 simpleDelimited = list(fieldDelimiter = ","))), distribution = list(
                                                                   online = list(url = list(url = NULL, `function` = "download")))), 
                           attributeList = list(attribute = list(list(attributeName = "enclosure", 
                                                                      attributeDefinition = "Enclosure ID. Enclosures were number 1-6 in each site for identification.", 
                                                                      storageType = "dbl", attributeLabel = NA, measurementScale = list(
                                                                        nominal = list(nonNumericDomain = list(textDomain = list(
                                                                          definition = "Enclosure ID. Enclosures were number 1-6 in each site for identification."))))), 
                                                                 list(attributeName = "fish_code", attributeDefinition = "Unique identifying number for fish within an enclosure. Fish were numbered 1-20 within each enclosure.", 
                                                                      storageType = "dbl", attributeLabel = NA, measurementScale = list(
                                                                        nominal = list(nonNumericDomain = list(textDomain = list(
                                                                          definition = "Unique identifying number for fish within an enclosure. Fish were numbered 1-20 within each enclosure."))))), 
                                                                 list(attributeName = "length_begin", attributeDefinition = "fish fork length at the start of the study", 
                                                                      storageType = "dbl", attributeLabel = NA, measurementScale = list(
                                                                        ratio = list(unit = list(standardUnit = "millimeter"), 
                                                                                     numericDomain = list(numberType = "real", bounds = list(
                                                                                       minimum = list(exclusive = "false", minimum = 48), 
                                                                                       maximum = list(exclusive = "false", maximum = 78)))))), 
                                                                 list(attributeName = "length_middle", attributeDefinition = "fish fork length at the mid-point of the study", 
                                                                      storageType = "dbl", attributeLabel = NA, measurementScale = list(
                                                                        ratio = list(unit = list(standardUnit = "millimeter"), 
                                                                                     numericDomain = list(numberType = "real", bounds = list(
                                                                                       minimum = list(exclusive = "false", minimum = 68), 
                                                                                       maximum = list(exclusive = "false", maximum = 84)))))), 
                                                                 list(attributeName = "length_end", attributeDefinition = "fish fork length at the end of the study", 
                                                                      storageType = "dbl", attributeLabel = NA, measurementScale = list(
                                                                        ratio = list(unit = list(standardUnit = "millimeter"), 
                                                                                     numericDomain = list(numberType = "real", bounds = list(
                                                                                       minimum = list(exclusive = "false", minimum = 65), 
                                                                                       maximum = list(exclusive = "false", maximum = 95)))))), 
                                                                 list(attributeName = "condition_beginning", attributeDefinition = "Fulton's condition factor at the beginning of the study", 
                                                                      storageType = "dbl", attributeLabel = NA, measurementScale = list(
                                                                        ratio = list(unit = list(standardUnit = "dimensionless"), 
                                                                                     numericDomain = list(numberType = "real", bounds = list(
                                                                                       minimum = list(exclusive = "false", minimum = 0.3350022758), 
                                                                                       maximum = list(exclusive = "false", maximum = 2.700318882)))))), 
                                                                 list(attributeName = "condition_ending", attributeDefinition = "Fulton's condition factor at the end of the study", 
                                                                      storageType = "dbl", attributeLabel = NA, measurementScale = list(
                                                                        ratio = list(unit = list(standardUnit = "dimensionless"), 
                                                                                     numericDomain = list(numberType = "real", bounds = list(
                                                                                       minimum = list(exclusive = "false", minimum = 0.732756773), 
                                                                                       maximum = list(exclusive = "false", maximum = 1.590976199)))))), 
                                                                 list(attributeName = "density_start", attributeDefinition = "density at the start of the study, represented as the number of fish in the enclosure", 
                                                                      storageType = "dbl", attributeLabel = NA, 
                                                                      measurementScale = list(
                                                                        ratio = list(unit = list(customUnit = "fishPerEnclosure"), 
                                                                                     numericDomain = list(numberType = "whole", 
                                                                                                          bounds = list(minimum = list(exclusive = "false", 
                                                                                                                                       minimum = 18), 
                                                                                                                        maximum = list(exclusive = "false", 
                                                                                                                                       maximum = 20)))))), 
                                                                 list(attributeName = "density_middle",  attributeDefinition = "density at the mid-point of the study, represented as the number of fish in the enclosure", 
                                                                      storageType = "dbl", attributeLabel = NA, 
                                                                      measurementScale = list(ratio = list(unit = list(customUnit = "fishPerEnclosure"), 
                                                                                                           numericDomain = list(numberType = "whole", bounds = list(minimum = list(exclusive = "false",  minimum = 11), maximum = list(exclusive = "false", maximum = 20)))))), 
                                                                 list(attributeName = "density_end",  attributeDefinition = "density at the end of the study, represented as the number of fish in the enclosure", storageType = "dbl", attributeLabel = NA, 
                                                                      measurementScale = list(ratio = list(unit = list(customUnit = "fishPerEnclosure"), numericDomain = list(numberType = "whole",  bounds = list(minimum = list(exclusive = "false",  minimum = 5),
                                                                                                                                                                                                                   maximum = list(exclusive = "false", maximum = 20)))))), 
                                                                 list(attributeName = "atu",  attributeDefinition = "acumulated thermal units experienced by the fish over the course of the study", storageType = "dbl", 
                                                                      attributeLabel = NA, measurementScale = list( ratio = list(unit = list(customUnit = "thermal unit"), 
                                                                                                                                 numericDomain = list(numberType = "real", bounds = list(
                                                                                                                                   minimum = list(exclusive = "false", minimum = 593.193),  maximum = list(exclusive = "false", maximum = 628.55)))))), 
                                                                 list(attributeName = "habitat", attributeDefinition = "The habitat type (mainstem/sidechannel) and treatment (control/restored) for each study site", 
                                                                      storageType = "string", attributeLabel = NA, measurementScale = list(
                                                                        nominal = list(nonNumericDomain = list(textDomain = list(
                                                                          definition = "The habitat type (mainstem/sidechannel) and treatment (control/restored) for each study site"))))), 
                                                                 list(attributeName = "site_id", attributeDefinition = "site name for each study site", 
                                                                      storageType = "string", attributeLabel = NA, measurementScale = list(
                                                                        nominal = list(nonNumericDomain = list(textDomain = list(
                                                                          definition = "site name for each study site"))))), 
                                                                 list(attributeName = "days", attributeDefinition = "number of days fish were in enclosure", 
                                                                      storageType = "dbl", attributeLabel = NA, measurementScale = list(
                                                                        ratio = list(unit = list(customUnit = "day"), 
                                                                                     numericDomain = list(numberType = "whole", 
                                                                                                          bounds = list(minimum = list(exclusive = "false", 
                                                                                                                                       minimum = 43), maximum = list(exclusive = "false", 
                                                                                                                                                                     maximum = 48)))))))), numberOfRecords = 353L)))
test_that("Add datatable adds the datatable", {
  
 expect_equal(add_datatable(list(), datatable_metadata), expected_datatabletable)
})
               
               