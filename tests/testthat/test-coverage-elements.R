#Tests for add_coverage function 

test_that('Coverage function errors when missing mandatory identifier inputs', {
  
  expect_error(add_coverage(parent_element = list(), west_bounding_coordinate = "-160.594000",
                            east_bounding_coordinate = "-134.104800",
                            north_bounding_coordinate = "71.238300",
                            south_bounding_coordinate = "67.865000",
                            begin_date = "1980-01-01", end_date = "2010-12-31"), 
               "Please supply a brief description of the locations of research sites and areas related to this dataset.")
  
  expect_error(add_coverage(parent_element = list(), geographic_description = "North Slope drainage basin:Bounding box encompasses 42 drainage basins totaling the North Slope drainage basin, Alaska, USA.",
                            east_bounding_coordinate = "-134.104800",
                            north_bounding_coordinate = "71.238300",
                            south_bounding_coordinate = "67.865000",
                            begin_date = "1980-01-01", end_date = "2010-12-31"),
               "Please supply the west cardinality limit.")
  
  expect_error(add_coverage(parent_element = list(), geographic_description = "North Slope drainage basin:Bounding box encompasses 42 drainage basins totaling the North Slope drainage basin, Alaska, USA.",
                            west_bounding_coordinate = "-160.594000",
                            north_bounding_coordinate = "71.238300",
                            south_bounding_coordinate = "67.865000",
                            begin_date = "1980-01-01", end_date = "2010-12-31"),
               "Please supply the east cardinality limit.")
  
  expect_error(add_coverage(parent_element = list(), geographic_description = "North Slope drainage basin:Bounding box encompasses 42 drainage basins totaling the North Slope drainage basin, Alaska, USA.",
                            west_bounding_coordinate = "-160.594000",
                            east_bounding_coordinate = "-134.104800",
                            south_bounding_coordinate = "67.865000",
                            begin_date = "1980-01-01", end_date = "2010-12-31"),
               "Please supply the north cardinality limit.")
  
  expect_error(add_coverage(parent_element = list(), geographic_description = "North Slope drainage basin:Bounding box encompasses 42 drainage basins totaling the North Slope drainage basin, Alaska, USA.",
                            west_bounding_coordinate = "-160.594000",
                            east_bounding_coordinate = "-134.104800",
                            north_bounding_coordinate = "71.238300",
                            begin_date = "1980-01-01", end_date = "2010-12-31"),
               "Please supply the south cardinality limit.")
  
  expect_error(add_coverage(parent_element = list(), geographic_description = "North Slope drainage basin:Bounding box encompasses 42 drainage basins totaling the North Slope drainage basin, Alaska, USA.",
                            west_bounding_coordinate = "-160.594000",
                            east_bounding_coordinate = "-134.104800",
                            north_bounding_coordinate = "71.238300",
                            south_bounding_coordinate = "67.865000",
                            end_date = "2010-12-31"),
               "Please suppply the starting date of this project.")
  
  expect_error(add_coverage(parent_element = list(), geographic_description = "North Slope drainage basin:Bounding box encompasses 42 drainage basins totaling the North Slope drainage basin, Alaska, USA.",
                            west_bounding_coordinate = "-160.594000",
                            east_bounding_coordinate = "-134.104800",
                            north_bounding_coordinate = "71.238300",
                            south_bounding_coordinate = "67.865000",
                            begin_date = "1980-01-01"),
               "Please supply the end or projected end date for this project.")
  
  
})

test_that('The coverage function adds the coverage elements', {
  expect_equal(add_coverage(parent_element = list(), geographic_description = "Description",
                            west_bounding_coordinate = "-160.594000", 
                            east_bounding_coordinate = "-134.104800",
                            north_bounding_coordinate = "71.238300",
                            south_bounding_coordinate = "67.865000",
                            begin_date = "1980-01-01", end_date = "2010-12-31", taxonomic_coverage = NULL),
               list(coverage = list(geographicCoverage = list(geographicDescription = "Description", 
                                                              boundingCoordinates = list(
                                                                westBoundingCoordinate = "-160.594000", 
                                                                eastBoundingCoordinate = "-134.104800",
                                                                northBoundingCoordinate = "71.238300", 
                                                                southBoundingCoordinate = "67.865000")),
                                    temporalCoverage = list(rangeOfDates = list(
                                      beginDate = list(calendarDate = "1980-01-01"), 
                                      endDate = list(calendarDate = "2010-12-31")))))
               
  )
  
})

# Tests for add_taxonomic_coverage function

test_that('The taxonomic coverage function adds the taxonomic coverage elements', {
  
  expect_equal(add_taxonomic_coverage(CVPIA_common_species = "chinook"),
               list(taxonomicClassification = 
                      list(taxonRankName = "kingdom", 
                           taxonRankValue = "Animalia",
                           taxonomicClassification = 
                             list(taxonRankName = "phylum",
                                  taxonRankValue = "Chordata", 
                                  taxonomicClassification =
                                    list(taxonRankName = "class", 
                                         taxonRankValue = "Teleostei",
                                         taxonomicClassification = 
                                           list(taxonRankName = "order", 
                                                taxonRankValue = "Salmoniformes", 
                                                taxonomicClassification = 
                                                  list(taxonRankName = "family", 
                                                       taxonRankValue = "Salmonidae", 
                                                       taxonomicClassification = 
                                                         list(taxonRankName = "genus", 
                                                              taxonRankValue = "Oncorhynchus", 
                                                              taxonomicClassification = 
                                                                list(taxonRankName = "species", 
                                                                     taxonRankValue = "Oncorhynchus tshawytscha", 
                                                                     commonName = "Chinook Salmon", 
                                                                     taxonId = 
                                                                       list(provider = "https://itis.gov", 
                                                                            taxonId = "161980"))))))))))
  expect_equal(add_taxonomic_coverage(CVPIA_common_species = "white_sturgeon"),
               list(taxonomicClassification = 
                      list(taxonRankName = "kingdom", 
                           taxonRankValue = "Animalia", 
                           taxonomicClassification =
                             list(taxonRankName = "phylum",
                                  taxonRankValue = "Chordata", 
                                  taxonomicClassification = 
                                    list(taxonRankName = "class", 
                                         taxonRankValue = "Chondrostei",
                                         taxonomicClassification = 
                                           list(taxonRankName = "order",
                                                taxonRankValue = "Acipenseriformes", 
                                                taxonomicClassification = 
                                                  list(taxonRankName = "family", 
                                                       taxonRankValue = "Acipenseridae",
                                                       taxonomicClassification = list(
                                                         taxonRankName = "genus",
                                                         taxonRankValue = "Acipenser", 
                                                         taxonomicClassification = 
                                                           list(taxonRankName = "species", 
                                                                taxonRankValue = "Acipenser transmontanus", 
                                                                commonName = "White Sturgeon", 
                                                                taxonId = 
                                                                  list(provider = "https://itis.gov", 
                                                                       taxonId = "161068"))))))))))
  
  expect_equal(add_taxonomic_coverage(CVPIA_common_species = "steelhead"),
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
                                                                                                                                                        provider = "https://itis.gov", taxonId = "161989"))))))))))
  expect_equal(add_taxonomic_coverage(CVPIA_common_species = "delta_smelt"),
               list(taxonomicClassification = 
                      list(taxonRankName = "kingdom", 
                           taxonRankValue = "Animalia", 
                           taxonomicClassification = 
                             list(taxonRankName = "phylum", 
                                  taxonRankValue = "Chordata", 
                                  taxonomicClassification = 
                                    list(taxonRankName = "class", 
                                         taxonRankValue = "Teleostei",
                                         taxonomicClassification = 
                                           list(taxonRankName = "order", 
                                                taxonRankValue = "Osmeriformes", 
                                                taxonomicClassification =
                                                  list(taxonRankName = "family", 
                                                       taxonRankValue = "Osmeridae", 
                                                       taxonomicClassification = 
                                                         list(taxonRankName = "genus", 
                                                              taxonRankValue = "Hypomesus", 
                                                              taxonomicClassification = 
                                                                list(taxonRankName = "species", 
                                                                     taxonRankValue = "Hypomesus transpacificus", 
                                                                     commonName = "Delta Smelt", 
                                                                     taxonId = 
                                                                       list(provider = "https://itis.gov",
                                                                            taxonId = "162032"))))))))))
  expect_equal(add_taxonomic_coverage(CVPIA_common_species = "green_sturgeon"),
               list(taxonomicClassification = 
                      list(taxonRankName = "kingdom", 
                           taxonRankValue = "Animalia", 
                           taxonomicClassification = 
                             list(taxonRankName = "phylum", 
                                  taxonRankValue = "Chordata", 
                                  taxonomicClassification = 
                                    list(taxonRankName = "class", 
                                         taxonRankValue = "Chondrostei", 
                                         taxonomicClassification = 
                                           list(taxonRankName = "order", 
                                                taxonRankValue = "Acipenseriformes", 
                                                taxonomicClassification =
                                                  list(taxonRankName = "family", 
                                                       taxonRankValue = "Acipenseridae",
                                                       taxonomicClassification =
                                                         list(taxonRankName = "genus", 
                                                              taxonRankValue = "Acipenser", 
                                                              taxonomicClassification = 
                                                                list(taxonRankName = "species", 
                                                                     taxonRankValue = "Acipenser medirostris", 
                                                                     commonName = "Green Sturgeon", 
                                                                     taxonId = 
                                                                       list(provider = "https://itis.gov", 
                                                                            taxonId = 161067))))))))))
  expect_equal(add_taxonomic_coverage(kingdom_value = "Animalia",
                                      phylum_value = "Chordata",
                                      class_value = "Mammalia",
                                      order_value = "Carnivora",
                                      family_value = "Felidae",
                                      genus_value = "Panthera",
                                      species_value = "Panthera Leo",
                                      common_name = "Lion",
                                      taxon_id = "183803"),
               list(taxonomicClassification = 
                      list(taxonRankName = "kingdom", 
                           taxonRankValue = "Animalia", 
                           taxonomicClassification = 
                             list(taxonRankName = "phylum", 
                                  taxonRankValue = "Chordata", 
                                  taxonomicClassification = 
                                    list(taxonRankName = "class", 
                                         taxonRankValue = "Mammalia",
                                         taxonomicClassification = 
                                           list(taxonRankName = "order", 
                                                taxonRankValue = "Carnivora", 
                                                taxonomicClassification = 
                                                  list(taxonRankName = "family", 
                                                       taxonRankValue = "Felidae", 
                                                       taxonomicClassification = 
                                                         list(taxonRankName = "genus",
                                                              taxonRankValue = "Panthera", 
                                                              taxonomicClassification = 
                                                                list(taxonRankName = "species", 
                                                                     taxonRankValue = "Panthera Leo", 
                                                                     commonName = "Lion", 
                                                                     taxonId = 
                                                                       list(provider = "https://itis.gov", 
                                                                            taxonId = "183803"))))))))))
})

test_that('The taxonomic coverage function errors when missing mandatory identifier inputs.', {
  
  expect_error(add_taxonomic_coverage(phylum_value = "Chordata",
                                      class_value = "Mammalia",
                                      order_value = "Carnivora",
                                      family_value = "Felidae",
                                      genus_value = "Panthera",
                                      species_value = "Panthera Leo",
                                      common_name = "Lion",
                                      taxon_id = "183803"),
               "Please provide a kingdom.")
  
  expect_error(add_taxonomic_coverage(kingdom_value = "Animalia",
                                      class_value = "Mammalia",
                                      order_value = "Carnivora",
                                      family_value = "Felidae",
                                      genus_value = "Panthera",
                                      species_value = "Panthera Leo",
                                      common_name = "Lion",
                                      taxon_id = "183803"),
               "Please provide a phylum.")
  
  expect_error(add_taxonomic_coverage(kingdom_value = "Animalia",
                                      phylum_value = "Chordata",
                                      order_value = "Carnivora",
                                      family_value = "Felidae",
                                      genus_value = "Panthera",
                                      species_value = "Panthera Leo",
                                      common_name = "Lion",
                                      taxon_id = "183803"),
               "Please provide a class.")
  
  expect_error(add_taxonomic_coverage(kingdom_value = "Animalia",
                                      phylum_value = "Chordata",
                                      class_value = "Mammalia",
                                      family_value = "Felidae",
                                      genus_value = "Panthera",
                                      species_value = "Panthera Leo",
                                      common_name = "Lion",
                                      taxon_id = "183803"),
               "Please provide an order.")
  
  expect_error(add_taxonomic_coverage(kingdom_value = "Animalia",
                                      phylum_value = "Chordata",
                                      class_value = "Mammalia",
                                      order_value = "Carnivora",
                                      genus_value = "Panthera",
                                      species_value = "Panthera Leo",
                                      common_name = "Lion",
                                      taxon_id = "183803"),
               "Please provide a family.")
  
  expect_error(add_taxonomic_coverage(kingdom_value = "Animalia",
                                      phylum_value = "Chordata",
                                      class_value = "Mammalia",
                                      order_value = "Carnivora",
                                      family_value = "Felidae",
                                      species_value = "Panthera Leo",
                                      common_name = "Lion",
                                      taxon_id = "183803"),
               "Please provide a genus.")
  
  expect_error(add_taxonomic_coverage(kingdom_value = "Animalia",
                                      phylum_value = "Chordata",
                                      class_value = "Mammalia",
                                      order_value = "Carnivora",
                                      family_value = "Felidae",
                                      genus_value = "Panthera",
                                      common_name = "Lion",
                                      taxon_id = "183803"),
               "Please provide a species.")
  
  expect_error(add_taxonomic_coverage(kingdom_value = "Animalia",
                                      phylum_value = "Chordata",
                                      class_value = "Mammalia",
                                      order_value = "Carnivora",
                                      family_value = "Felidae",
                                      genus_value = "Panthera",
                                      species_value = "Panthera Leo",
                                      taxon_id = "183803"),
               "Please provide a common name")
  
  expect_message(add_taxonomic_coverage(kingdom_value = "Animalia",
                                      phylum_value = "Chordata",
                                      class_value = "Mammalia",
                                      order_value = "Carnivora",
                                      family_value = "Felidae",
                                      genus_value = "Panthera",
                                      species_value = "Panthera Leo",
                                      common_name = "Lion"),
               "No taxon id has been provided. This number can be found at ITIS.gov if you wish to append it.")
  
})

test_that('The coverage function works appropriately with the taxonomic function.', {
  taxonomic_coverage <- add_taxonomic_coverage(CVPIA_common_species = "chinook")
  expect_equal(add_coverage(parent_element = list(), geographic_description = "Description",
                            west_bounding_coordinate = "-160.594000", 
                            east_bounding_coordinate = "-134.104800",
                            north_bounding_coordinate = "71.238300",
                            south_bounding_coordinate = "67.865000",
                            begin_date = "1980-01-01", end_date = "2010-12-31", taxonomic_coverage = taxonomic_coverage), 
               list(coverage = 
                      list(geographicCoverage = 
                             list(geographicDescription = "Description",
                                  boundingCoordinates = 
                                    list(westBoundingCoordinate = "-160.594000", 
                                         eastBoundingCoordinate = "-134.104800", 
                                         northBoundingCoordinate = "71.238300", 
                                         southBoundingCoordinate = "67.865000")), 
                           temporalCoverage = 
                             list(rangeOfDates =
                                    list(beginDate = 
                                           list(calendarDate = "1980-01-01"), 
                                         endDate = 
                                           list(calendarDate = "2010-12-31"))), 
                           taxonomicCoverage = 
                             list(taxonomicClassification = 
                                    list(taxonRankName = "kingdom", 
                                         taxonRankValue = "Animalia",
                                         taxonomicClassification = 
                                           list(taxonRankName = "phylum", 
                                                taxonRankValue = "Chordata", 
                                                taxonomicClassification = 
                                                  list(taxonRankName = "class", 
                                                       taxonRankValue = "Teleostei", 
                                                       taxonomicClassification = 
                                                         list(taxonRankName = "order", 
                                                              taxonRankValue = "Salmoniformes", 
                                                              taxonomicClassification = 
                                                                list(taxonRankName = "family", 
                                                                     taxonRankValue = "Salmonidae", 
                                                                     taxonomicClassification = 
                                                                       list(taxonRankName = "genus", 
                                                                            taxonRankValue = "Oncorhynchus", 
                                                                            taxonomicClassification = 
                                                                              list(taxonRankName = "species", 
                                                                                   taxonRankValue = "Oncorhynchus tshawytscha", 
                                                                                   commonName = "Chinook Salmon",
                                                                                   taxonId = 
                                                                                     list(provider = "https://itis.gov", 
                                                                                          taxonId = "161980"))))))))))))
  
  
  
})

test_that('The coverage function can append multiple taxonomic coverages.', {
  delta <- add_taxonomic_coverage(CVPIA_common_species = "delta_smelt")
  chinook <- add_taxonomic_coverage(CVPIA_common_species = "chinook")
  white <- add_taxonomic_coverage(CVPIA_common_species = "white_sturgeon")
  taxonomic_coverage <- list(delta, chinook, white)
  expect_equal(add_coverage(parent_element = list(), geographic_description = "Description",
                            west_bounding_coordinate = "-160.594000", 
                            east_bounding_coordinate = "-134.104800",
                            north_bounding_coordinate = "71.238300",
                            south_bounding_coordinate = "67.865000",
                            begin_date = "1980-01-01", end_date = "2010-12-31",
                            taxonomic_coverage = taxonomic_coverage),
               list(coverage = list(geographicCoverage = 
                                      list(geographicDescription = "Description", 
                                           boundingCoordinates = list(westBoundingCoordinate = "-160.594000", 
                                                                      eastBoundingCoordinate = "-134.104800", 
                                                                      northBoundingCoordinate = "71.238300", 
                                                                      southBoundingCoordinate = "67.865000")), 
                                    temporalCoverage = list(rangeOfDates = 
                                                              list(beginDate = 
                                                                     list(calendarDate = "1980-01-01"), 
                                                                   endDate = list(calendarDate = "2010-12-31"))), 
                                    taxonomicCoverage = list(list(taxonomicClassification = 
                                                                    list(taxonRankName = "kingdom",
                                                                         taxonRankValue = "Animalia",
                                                                         taxonomicClassification = 
                                                                           list(taxonRankName = "phylum", 
                                                                                taxonRankValue = "Chordata", 
                                                                                taxonomicClassification = 
                                                                                  list(taxonRankName = "class", 
                                                                                       taxonRankValue = "Teleostei", 
                                                                                       taxonomicClassification = 
                                                                                         list(taxonRankName = "order", 
                                                                                              taxonRankValue = "Osmeriformes", 
                                                                                              taxonomicClassification = 
                                                                                                list(taxonRankName = "family", 
                                                                                                     taxonRankValue = "Osmeridae", 
                                                                                                     taxonomicClassification = 
                                                                                                       list(taxonRankName = "genus", 
                                                                                                            taxonRankValue = "Hypomesus", 
                                                                                                            taxonomicClassification = 
                                                                                                              list(taxonRankName = "species", 
                                                                                                                   taxonRankValue = "Hypomesus transpacificus", 
                                                                                                                   commonName = "Delta Smelt", 
                                                                                                                   taxonId = 
                                                                                                                     list(provider = "https://itis.gov", 
                                                                                                                          taxonId = "162032"))))))))), 
                                                             list(taxonomicClassification = 
                                                                    list(taxonRankName = "kingdom", 
                                                                         taxonRankValue = "Animalia", 
                                                                         taxonomicClassification = 
                                                                           list(taxonRankName = "phylum", 
                                                                                taxonRankValue = "Chordata", 
                                                                                taxonomicClassification = 
                                                                                  list(taxonRankName = "class", 
                                                                                       taxonRankValue = "Teleostei",
                                                                                       taxonomicClassification = 
                                                                                         list(taxonRankName = "order", 
                                                                                              taxonRankValue = "Salmoniformes", 
                                                                                              taxonomicClassification = 
                                                                                                list(taxonRankName = "family", 
                                                                                                     taxonRankValue = "Salmonidae",
                                                                                                     taxonomicClassification = 
                                                                                                       list(taxonRankName = "genus",
                                                                                                            taxonRankValue = "Oncorhynchus", 
                                                                                                            taxonomicClassification = 
                                                                                                              list(taxonRankName = "species",
                                                                                                                   taxonRankValue = "Oncorhynchus tshawytscha", 
                                                                                                                   commonName = "Chinook Salmon", 
                                                                                                                   taxonId = 
                                                                                                                     list(provider = "https://itis.gov", 
                                                                                                                          taxonId = "161980"))))))))), 
                                                             list(taxonomicClassification = 
                                                                    list(taxonRankName = "kingdom", 
                                                                         taxonRankValue = "Animalia", 
                                                                         taxonomicClassification =
                                                                           list(taxonRankName = "phylum", 
                                                                                taxonRankValue = "Chordata", 
                                                                                taxonomicClassification = 
                                                                                  list(taxonRankName = "class", 
                                                                                       taxonRankValue = "Chondrostei",
                                                                                       taxonomicClassification =
                                                                                         list(taxonRankName = "order",
                                                                                              taxonRankValue = "Acipenseriformes", 
                                                                                              taxonomicClassification = 
                                                                                                list(taxonRankName = "family", 
                                                                                                     taxonRankValue = "Acipenseridae", 
                                                                                                     taxonomicClassification = 
                                                                                                       list(taxonRankName = "genus", 
                                                                                                            taxonRankValue = "Acipenser", 
                                                                                                            taxonomicClassification = 
                                                                                                              list(taxonRankName = "species",
                                                                                                                   taxonRankValue = "Acipenser transmontanus", 
                                                                                                                   commonName = "White Sturgeon",
                                                                                                                   taxonId = 
                                                                                                                     list( provider = "https://itis.gov", 
                                                                                                                           taxonId = "161068")))))))))))))
  
})
