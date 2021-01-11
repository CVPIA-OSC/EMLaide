storage_type <- list(string = "string", boolean = "boolean", decimal = "decimal",
                     float = "float", double = "double", duration = "duration",
                     dateTime = "dateTime", time = "time", date = "date", 
                     gYearMonth = "gYearMonth", gYear = "gYear", 
                     gMonthDay = "gMonthDay", gDay = "gDay", gMonth = "gMonth")
usethis::use_data(storage_type)

measurement_scale <- list(nominal = "nominal", ordinal = "ordinal",
                          interval = "interval", ratio = "ratio",
                          dateTime = "dateTime")
usethis::use_data(measurement_scale)

number_type <- list(natural = "natural", whole = "whole", integer = "integer",
                    real = "real")
usethis::use_data(number_type)

CVPIA_common_species <- list(chinook = list(taxonomicClassification = 
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
                             steelhead = list(taxonomicClassification = 
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
                                                                                             taxonRankValue = "Oncorhynchus mykiss", 
                                                                                             commonName = "Steelhead Trout", 
                                                                                             taxonId = 
                                                                                             list(provider = "https://itis.gov", 
                                                                                             taxonId = "161989"))))))))), 
                             delta_smelt = list(taxonomicClassification = 
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
                             white_sturgeon =  list(taxonomicClassification = 
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
                                                                                                       taxonId = "161068"))))))))),
                             green_sturgeon = list(taxonomicClassification = 
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
                                                                                                           taxonId = "161067"))))))))))
usethis::use_data(CVPIA_common_species)