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
               list(TaxonomicClassification = list(TaxonRankName = "kingdom", 
                                                   TaxonRankValue = "Animalia", TaxonomicClassification = list(
                                                     TaxonRankName = "phylum", TaxonRankValue = "Chordata", 
                                                     TaxonomicClassification = list(TaxonRankName = "class", 
                                                                                    TaxonRankValue = "Teleostei", TaxonomicClassification = list(
                                                                                      TaxonRankName = "order", TaxonRankValue = "Salmoniformes", 
                                                                                      TaxonomicClassification = list(TaxonRankName = "family", 
                                                                                                                     TaxonRankValue = "Salmonidae", TaxonomicClassification = list(
                                                                                                                       TaxonRankName = "genus", TaxonRankValue = "Oncorhynchus", 
                                                                                                                       TaxonomicClassification = list(TaxonRankName = "species", 
                                                                                                                                                      TaxonRankValue = "Oncorhynchus tshawytscha", 
                                                                                                                                                      commonName = "Chinook Salmon")))))))))
  expect_equal(add_taxonomic_coverage(CVPIA_common_species = "white_sturgeon"),
               list(TaxonomicClassification = list(TaxonRankName = "kingdom", 
                                                   TaxonRankValue = "Animalia", TaxonomicClassification = list(
                                                     TaxonRankName = "phylum", TaxonRankValue = "Chordata", 
                                                     TaxonomicClassification = list(TaxonRankName = "class", 
                                                                                    TaxonRankValue = "Chondrostei", TaxonomicClassification = list(
                                                                                      TaxonRankName = "order", TaxonRankValue = "Acipenseriformes", 
                                                                                      TaxonomicClassification = list(TaxonRankName = "family", 
                                                                                                                     TaxonRankValue = "Acipenseridae", TaxonomicClassification = list(
                                                                                                                       TaxonRankName = "genus", TaxonRankValue = "Acipenser", 
                                                                                                                       TaxonomicClassification = list(TaxonRankName = "species", 
                                                                                                                                                      TaxonRankValue = "Acipenser transmontanus", 
                                                                                                                                                      commonName = "White Sturgeon")))))))))
  
  expect_equal(add_taxonomic_coverage(CVPIA_common_species = "steelhead"),
               list(TaxonomicClassification = list(TaxonRankName = "kingdom", 
                                                   TaxonRankValue = "Animalia", TaxonomicClassification = list(
                                                     TaxonRankName = "phylum", TaxonRankValue = "Chordata", 
                                                     TaxonomicClassification = list(TaxonRankName = "class", 
                                                                                    TaxonRankValue = "Teleostei", TaxonomicClassification = list(
                                                                                      TaxonRankName = "order", TaxonRankValue = "Salmoniformes", 
                                                                                      TaxonomicClassification = list(TaxonRankName = "family", 
                                                                                                                     TaxonRankValue = "Salmonidae", TaxonomicClassification = list(
                                                                                                                       TaxonRankName = "genus", TaxonRankValue = "Oncorhynchus", 
                                                                                                                       TaxonomicClassification = list(TaxonRankName = "species", 
                                                                                                                                                      TaxonRankValue = "Oncorhynchus mykiss", 
                                                                                                                                                      commonName = "Steelhead Trout")))))))))
  expect_equal(add_taxonomic_coverage(CVPIA_common_species = "delta_smelt"),
               list(TaxonomicClassification = list(TaxonRankName = "kingdom", 
                                                   TaxonRankValue = "Animalia", TaxonomicClassification = list(
                                                     TaxonRankName = "phylum", TaxonRankValue = "Chordata", 
                                                     TaxonomicClassification = list(TaxonRankName = "class", 
                                                                                    TaxonRankValue = "Teleostei", TaxonomicClassification = list(
                                                                                      TaxonRankName = "order", TaxonRankValue = "Osmeriformes", 
                                                                                      TaxonomicClassification = list(TaxonRankName = "family", 
                                                                                                                     TaxonRankValue = "Osmeridae", TaxonomicClassification = list(
                                                                                                                       TaxonRankName = "genus", TaxonRankValue = "Hypomesus", 
                                                                                                                       TaxonomicClassification = list(TaxonRankName = "species", 
                                                                                                                                                      TaxonRankValue = "Hypomesus transpacificus", 
                                                                                                                                                      commonName = "Delta Smelt")))))))))
  expect_equal(add_taxonomic_coverage(CVPIA_common_species = "green_sturgeon"),
               list(TaxonomicClassification = list(TaxonRankName = "kingdom", 
                                                   TaxonRankValue = "Animalia", TaxonomicClassification = list(
                                                     TaxonRankName = "phylum", TaxonRankValue = "Chordata", 
                                                     TaxonomicClassification = list(TaxonRankName = "class", 
                                                                                    TaxonRankValue = "Chondrostei", TaxonomicClassification = list(
                                                                                      TaxonRankName = "order", TaxonRankValue = "Acipenseriformes", 
                                                                                      TaxonomicClassification = list(TaxonRankName = "family", 
                                                                                                                     TaxonRankValue = "Acipenseridae", TaxonomicClassification = list(
                                                                                                                       TaxonRankName = "genus", TaxonRankValue = "Acipenser", 
                                                                                                                       TaxonomicClassification = list(TaxonRankName = "species", 
                                                                                                                                                      TaxonRankValue = "Acipenser medirostris", 
                                                                                                                                                      commonName = "Green Sturgeon")))))))))
  expect_equal(add_taxonomic_coverage(kingdom_value = "Animalia",
                                      phylum_value = "Chordata",
                                      class_value = "Mammalia",
                                      order_value = "Carnivora",
                                      family_value = "Felidae",
                                      genus_value = "Panthera",
                                      species_value = "Panthera Leo",
                                      common_name = "Lion"),
               list(TaxonomicClassification = list(TaxonRankName = "kingdom", 
                                                   TaxonRankValue = "Animalia", TaxonomicClassification = list(
                                                     TaxonRankName = "phylum", TaxonRankValue = "Chordata", 
                                                     TaxonomicClassification = list(TaxonRankName = "class", 
                                                                                    TaxonRankValue = "Mammalia", TaxonomicClassification = list(
                                                                                      TaxonRankName = "order", TaxonRankValue = "Carnivora", 
                                                                                      TaxonomicClassification = list(TaxonRankName = "family", 
                                                                                                                     TaxonRankValue = "Felidae", TaxonomicClassification = list(
                                                                                                                       TaxonRankName = "genus", TaxonRankValue = "Panthera", 
                                                                                                                       TaxonomicClassification = list(TaxonRankName = "species", 
                                                                                                                                                      TaxonRankValue = "Panthera Leo", commonName = "Lion")))))))))
})

test_that('The taxonomic coverage function errors when missing mandatory identifier inputs.', {
  
  expect_error(add_taxonomic_coverage(phylum_value = "Chordata",
                                      class_value = "Mammalia",
                                      order_value = "Carnivora",
                                      family_value = "Felidae",
                                      genus_value = "Panthera",
                                      species_value = "Panthera Leo",
                                      common_name = "Lion"),
               "Please provide a kingdom.")
  
  expect_error(add_taxonomic_coverage(kingdom_value = "Animalia",
                                      class_value = "Mammalia",
                                      order_value = "Carnivora",
                                      family_value = "Felidae",
                                      genus_value = "Panthera",
                                      species_value = "Panthera Leo",
                                      common_name = "Lion"),
               "Please provide a phylum.")
  
  expect_error(add_taxonomic_coverage(kingdom_value = "Animalia",
                                      phylum_value = "Chordata",
                                      order_value = "Carnivora",
                                      family_value = "Felidae",
                                      genus_value = "Panthera",
                                      species_value = "Panthera Leo",
                                      common_name = "Lion"),
               "Please provide a class.")
  
  expect_error(add_taxonomic_coverage(kingdom_value = "Animalia",
                                      phylum_value = "Chordata",
                                      class_value = "Mammalia",
                                      family_value = "Felidae",
                                      genus_value = "Panthera",
                                      species_value = "Panthera Leo",
                                      common_name = "Lion"),
               "Please provide an order.")
  
  expect_error(add_taxonomic_coverage(kingdom_value = "Animalia",
                                      phylum_value = "Chordata",
                                      class_value = "Mammalia",
                                      order_value = "Carnivora",
                                      genus_value = "Panthera",
                                      species_value = "Panthera Leo",
                                      common_name = "Lion"),
               "Please provide a family.")
  
  expect_error(add_taxonomic_coverage(kingdom_value = "Animalia",
                                      phylum_value = "Chordata",
                                      class_value = "Mammalia",
                                      order_value = "Carnivora",
                                      family_value = "Felidae",
                                      species_value = "Panthera Leo",
                                      common_name = "Lion"),
               "Please provide a genus.")
  
  expect_error(add_taxonomic_coverage(kingdom_value = "Animalia",
                                      phylum_value = "Chordata",
                                      class_value = "Mammalia",
                                      order_value = "Carnivora",
                                      family_value = "Felidae",
                                      genus_value = "Panthera",
                                      common_name = "Lion"),
               "Please provide a species.")
  
  expect_error(add_taxonomic_coverage(kingdom_value = "Animalia",
                                      phylum_value = "Chordata",
                                      class_value = "Mammalia",
                                      order_value = "Carnivora",
                                      family_value = "Felidae",
                                      genus_value = "Panthera",
                                      species_value = "Panthera Leo"),
               "Please provide a common name")
  
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
                                    list(beginDate = list(calendarDate = "1980-01-01"),
                                         endDate = list(calendarDate = "2010-12-31"))),
                           taxonomicCoverage = list(TaxonomicClassification = 
                                                      list(TaxonRankName = "kingdom",
                                                           TaxonRankValue = "Animalia",
                                                           TaxonomicClassification =
                                                             list(TaxonRankName = "phylum",
                                                                  TaxonRankValue = "Chordata",
                                                                  TaxonomicClassification = 
                                                                    list(TaxonRankName = "class",
                                                                         TaxonRankValue = "Teleostei",
                                                                         TaxonomicClassification = list(
                                                                           TaxonRankName = "order",
                                                                           TaxonRankValue = "Salmoniformes",
                                                                           TaxonomicClassification = 
                                                                             list(TaxonRankName = "family", 
                                                                                  TaxonRankValue = "Salmonidae",
                                                                                  TaxonomicClassification = 
                                                                                    list(TaxonRankName = "genus",
                                                                                         TaxonRankValue = "Oncorhynchus",
                                                                                         TaxonomicClassification =
                                                                                           list(TaxonRankName = "species",
                                                                                                TaxonRankValue = "Oncorhynchus tshawytscha",
                                                                                                commonName = "Chinook Salmon")))))))))))
  
  
  
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
               list(
                 coverage = list(
                   geographicCoverage = list(
                     geographicDescription = "Description",
                     boundingCoordinates = list(
                       westBoundingCoordinate = "-160.594000",
                       eastBoundingCoordinate = "-134.104800",
                       northBoundingCoordinate = "71.238300",
                       southBoundingCoordinate = "67.865000")),
                   temporalCoverage = list(rangeOfDates = list(
                     beginDate = list(calendarDate = "1980-01-01"),
                     endDate = list(calendarDate = "2010-12-31"))),
                   taxonomicCoverage = list(
                     list(
                       TaxonomicClassification = list(
                         TaxonRankName = "kingdom",
                         TaxonRankValue = "Animalia",
                         TaxonomicClassification = list(
                           TaxonRankName = "phylum",
                           TaxonRankValue = "Chordata",
                           TaxonomicClassification = list(
                             TaxonRankName = "class",
                             TaxonRankValue = "Teleostei",
                             TaxonomicClassification = list(
                               TaxonRankName = "order",
                               TaxonRankValue = "Osmeriformes",
                               TaxonomicClassification = list(
                                 TaxonRankName = "family",
                                 TaxonRankValue = "Osmeridae",
                                 TaxonomicClassification = list(
                                   TaxonRankName = "genus",
                                   TaxonRankValue = "Hypomesus",
                                   TaxonomicClassification = list(
                                     TaxonRankName = "species",
                                     TaxonRankValue = "Hypomesus transpacificus",
                                     commonName = "Delta Smelt")))))))),
                     list(
                       TaxonomicClassification = list(
                         TaxonRankName = "kingdom",
                         TaxonRankValue = "Animalia",
                         TaxonomicClassification = list(
                           TaxonRankName = "phylum",
                           TaxonRankValue = "Chordata",
                           TaxonomicClassification = list(
                             TaxonRankName = "class",
                             TaxonRankValue = "Teleostei",
                             TaxonomicClassification = list(
                               TaxonRankName = "order",
                               TaxonRankValue = "Salmoniformes",
                               TaxonomicClassification = list(
                                 TaxonRankName = "family",
                                 TaxonRankValue = "Salmonidae",
                                 TaxonomicClassification = list(
                                   TaxonRankName = "genus",
                                   TaxonRankValue = "Oncorhynchus",
                                   TaxonomicClassification = list(
                                     TaxonRankName = "species",
                                     TaxonRankValue = "Oncorhynchus tshawytscha",
                                     commonName = "Chinook Salmon")))))))),
                     list(
                       TaxonomicClassification = list(
                         TaxonRankName = "kingdom",
                         TaxonRankValue = "Animalia",
                         TaxonomicClassification = list(
                           TaxonRankName = "phylum",
                           TaxonRankValue = "Chordata",
                           TaxonomicClassification = list(
                             TaxonRankName = "class",
                             TaxonRankValue = "Chondrostei",
                             TaxonomicClassification = list(
                               TaxonRankName = "order",
                               TaxonRankValue = "Acipenseriformes",
                               TaxonomicClassification = list(
                                 TaxonRankName = "family",
                                 TaxonRankValue = "Acipenseridae",
                                 TaxonomicClassification = list(
                                   TaxonRankName = "genus",
                                   TaxonRankValue = "Acipenser",
                                   TaxonomicClassification = list(
                                     TaxonRankName = "species",
                                     TaxonRankValue = "Acipenser transmontanus",
                                     commonName = "White Sturgeon"))))))))))))
  
})
