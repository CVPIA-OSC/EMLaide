# store helper data objects and update as needed

# CVPIA common species
CVPIA_common_species <- list(chinook = list(taxonomicClassification = list(taxonRankName = "kingdom", 
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
                             steelhead = list(taxonomicClassification = list(taxonRankName = "kingdom", 
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
                                                                                                                                                                                  provider = "https://itis.gov", taxonId = "161989"))))))))), 
                             delta_smelt = list(taxonomicClassification = list(taxonRankName = "kingdom", 
                                                                               taxonRankValue = "Animalia", taxonomicClassification = list(
                                                                                 taxonRankName = "phylum", taxonRankValue = "Chordata", 
                                                                                 taxonomicClassification = list(taxonRankName = "class", 
                                                                                                                taxonRankValue = "Teleostei", taxonomicClassification = list(
                                                                                                                  taxonRankName = "order", taxonRankValue = "Osmeriformes", 
                                                                                                                  taxonomicClassification = list(taxonRankName = "family", 
                                                                                                                                                 taxonRankValue = "Osmeridae", taxonomicClassification = list(
                                                                                                                                                   taxonRankName = "genus", taxonRankValue = "Hypomesus", 
                                                                                                                                                   taxonomicClassification = list(taxonRankName = "species", 
                                                                                                                                                                                  taxonRankValue = "Hypomesus transpacificus", 
                                                                                                                                                                                  commonName = "Delta Smelt", taxonId = list(
                                                                                                                                                                                    provider = "https://itis.gov", taxonId = "162032"))))))))), 
                             white_sturgeon = list(taxonomicClassification = list(taxonRankName = "kingdom", 
                                                                                  taxonRankValue = "Animalia", taxonomicClassification = list(
                                                                                    taxonRankName = "phylum", taxonRankValue = "Chordata", 
                                                                                    taxonomicClassification = list(taxonRankName = "class", 
                                                                                                                   taxonRankValue = "Chondrostei", taxonomicClassification = list(
                                                                                                                     taxonRankName = "order", taxonRankValue = "Acipenseriformes", 
                                                                                                                     taxonomicClassification = list(taxonRankName = "family", 
                                                                                                                                                    taxonRankValue = "Acipenseridae", taxonomicClassification = list(
                                                                                                                                                      taxonRankName = "genus", taxonRankValue = "Acipenser", 
                                                                                                                                                      taxonomicClassification = list(taxonRankName = "species", 
                                                                                                                                                                                     taxonRankValue = "Acipenser transmontanus", 
                                                                                                                                                                                     commonName = "White Sturgeon", taxonId = list(
                                                                                                                                                                                       provider = "https://itis.gov", taxonId = "161068"))))))))), 
                             green_sturgeon = list(taxonomicClassification = list(taxonRankName = "kingdom", 
                                                                                  taxonRankValue = "Animalia", taxonomicClassification = list(
                                                                                    taxonRankName = "phylum", taxonRankValue = "Chordata", 
                                                                                    taxonomicClassification = list(taxonRankName = "class", 
                                                                                                                   taxonRankValue = "Chondrostei", taxonomicClassification = list(
                                                                                                                     taxonRankName = "order", taxonRankValue = "Acipenseriformes", 
                                                                                                                     taxonomicClassification = list(taxonRankName = "family", 
                                                                                                                                                    taxonRankValue = "Acipenseridae", taxonomicClassification = list(
                                                                                                                                                      taxonRankName = "genus", taxonRankValue = "Acipenser", 
                                                                                                                                                      taxonomicClassification = list(taxonRankName = "species", 
                                                                                                                                                                                     taxonRankValue = "Acipenser medirostris", 
                                                                                                                                                                                     commonName = "Green Sturgeon", taxonId = list(
                                                                                                                                                                                       provider = "https://itis.gov", taxonId = "161067"))))))))))
usethis::use_data(CVPIA_common_species, overwrite = TRUE)

# CVPIA funders
CVPIA_funders <- list(USBR = list(funderName = "United States Bureau of Reclamation", 
                                  funderIdentifier = "https://www.wikidata.org/wiki/Q1010548"), 
                      CDWR = list(funderName = "California Department of Water Resources", 
                                  funderIdentifier = "https://www.wikidata.org/wiki/Q5020440"), 
                      CDFW = list(funderName = "California Department of Fish and Wildlife", 
                                  funderIdentifier = "https://www.wikidata.org/wiki/Q5020421"),
                      USFWS = list(funderName = "United States Fish and Wildlife Service",
                                   funderIdentifier = "https://www.wikidata.org/wiki/Q674113"))
usethis::use_data(CVPIA_funders, overwrite = TRUE)

# measurement scale
measurement_scale <- list(nominal = "nominal", ordinal = "ordinal", interval = "interval", 
                          ratio = "ratio", dateTime = "dateTime")
usethis::use_data(measurement_scale, overwrite = TRUE)

# number type
number_type <- list(natural = "natural", whole = "whole", integer = "integer", 
                    real = "real")
usethis::use_data(number_type, overwrite = TRUE)

# standard_units
standard_units <- c("kilogram", "nanogram", "microgram", "milligram", "centigram", 
                    "decigram", "gram", "dekagram", "hectogram", "megagram", "tonne", 
                    "pound", "ton", "acre", "ampere", "amperePerMeter", "amperePerMeterSquared", 
                    "amperePerSquareMeter", "are", "atmosphere", "bar", "becquerel", 
                    "britishThermalUnit", "bushel", "bushelPerAcre", "bushelsPerAcre", 
                    "calorie", "candela", "candelaPerMeterSquared", "candelaPerSquareMeter", 
                    "celsius", "centimeterCubed", "centimeterPerSecond", "centimeterPerYear", 
                    "centimeterSquared", "centimetersPerSecond", "centisecond", "coulomb", 
                    "cubicCentimetersPerCubicCentimeters", "cubicFeetPerSecond", 
                    "cubicInch", "cubicMeter", "cubicMeterPerKilogram", "cubicMetersPerSecond", 
                    "cubicMicrometersPerGram", "decibar", "decisecond", "dekasecond", 
                    "dimensionless", "equivalentPerLiter", "fahrenheit", "farad", 
                    "feetPerDay", "feetPerHour", "feetPerSecond", "feetSquaredPerDay", 
                    "footCubedPerSecond", "footPerDay", "footPerHour", "footPerSecond", 
                    "footPound", "footSquared", "footSquaredPerDay", "gallon", "gramPerCentimeterCubed", 
                    "gramPerCentimeterSquaredPerSecond", "gramPerDayPerHectare", 
                    "gramPerDayPerLiter", "gramPerGram", "gramPerLiter", "gramPerMeterSquared", 
                    "gramPerMeterSquaredPerDay", "gramPerMeterSquaredPerYear", "gramPerMilliliter", 
                    "gramPerYear", "gramPercentimeterSquared", "gramsPerCentimeterSquaredPerSecond", 
                    "gramsPerCubicCentimeter", "gramsPerGram", "gramsPerHectarePerDay", 
                    "gramsPerLiter", "gramsPerLiterPerDay", "gramsPerMeterSquaredPerYear", 
                    "gramsPerMilliliter", "gramsPerSquareMeter", "gramsPerYear", 
                    "gray", "hectare", "hectopascal", "hectosecond", "henry", "hertz", 
                    "hour", "inchCubed", "inchPerHour", "inverseCentimeter", "inverseMeter", 
                    "joule", "katal", "kelvin", "kilogramPerCubicMeter", "kilogramPerHectare", 
                    "kilogramPerHectarePerYear", "kilogramPerMeterCubed", "kilogramPerMeterSquared", 
                    "kilogramPerMeterSquaredPerDay", "kilogramPerMeterSquaredPerSecond", 
                    "kilogramPerMeterSquaredPerYear", "kilogramPerSecond", "kilogramsPerHectare", 
                    "kilogramsPerHectarePerYear", "kilogramsPerMeterSquaredPerSecond", 
                    "kilogramsPerMeterSquaredPerYear", "kilogramsPerSecond", "kilogramsPerSquareMeter", 
                    "kilohertz", "kiloliter", "kilometerPerHour", "kilometerSquared", 
                    "kilometersPerHour", "kilopascal", "kilosecond", "kilovolt", 
                    "kilowatt", "kilowattPerMeterSquared", "knot", "knots", "langley", 
                    "langleyPerDay", "liter", "literPerHectare", "literPerLiter", 
                    "literPerMeterSquared", "literPerSecond", "litersPerHectare", 
                    "litersPerSecond", "litersPerSquareMeter", "lumen", "lux", "megagramPerMeterCubed", 
                    "megahertz", "megajoulePerMeterSquaredPerDay", "megapascal", 
                    "megasecond", "megavolt", "megawatt", "meterCubed", "meterCubedPerHectare", 
                    "meterCubedPerKilogram", "meterCubedPerMeterCubed", "meterCubedPerMeterSquared", 
                    "meterCubedPerSecond", "meterPerDay", "meterPerGram", "meterPerSecond", 
                    "meterPerSecondSquared", "meterSquared", "meterSquaredPerDay", 
                    "meterSquaredPerHectare", "meterSquaredPerKilogram", "meterSquaredPerSecond", 
                    "metersPerDay", "metersPerGram", "metersPerSecond", "metersPerSecondSquared", 
                    "metersSquaredPerDay", "metersSquaredPerSecond", "microequivalentPerLiter", 
                    "microgramPerGram", "microgramPerGramPerDay", "microgramPerGramPerHour", 
                    "microgramPerGramPerWeek", "microgramPerLiter", "microgramsPerGram", 
                    "microgramsPerLiter", "microliter", "microliterPerLiter", "micrometerCubedPerGram", 
                    "micromolePerCentimeterSquaredPerSecond", "micromolePerGram", 
                    "micromolePerGramPerDay", "micromolePerGramPerHour", "micromolePerGramPerSecond", 
                    "micromolePerKilogram", "micromolePerLiter", "micromolePerMeterSquaredPerSecond", 
                    "micromolePerMole", "microsecond", "microwattPerCentimeterSquaredPerNanometer", 
                    "microwattPerCentimeterSquaredPerNanometerPerStadianan", "microwattPerCentimeterSquaredPerSteradian", 
                    "milePerHour", "milePerMinute", "milePerSecond", "mileSquared", 
                    "milesPerHour", "milesPerMinute", "milesPerSecond", "milliGramsPerMilliLiter", 
                    "millibar", "milliequivalentPerLiter", "milligramPerKilogram", 
                    "milligramPerLiter", "milligramPerMeterCubed", "milligramPerMeterCubedPerDay", 
                    "milligramPerMeterSquared", "milligramPerMeterSquaredPerDay", 
                    "milligramPerMilliliter", "milligramsPerCubicMeter", "milligramsPerLiter", 
                    "milligramsPerSquareMeter", "millihertz", "milliliter", "milliliterPerLiter", 
                    "millimeterPerDay", "millimeterPerSecond", "millimeterSquared", 
                    "millimetersPerSecond", "millimolePerGram", "millimolePerKilogram", 
                    "millimolePerLiter", "millimolePerMeterCubed", "millimolePerMole", 
                    "millimolesPerGram", "millisecond", "millivolt", "milliwatt", 
                    "minute", "molality", "molarity", "mole", "molePerCubicMeter", 
                    "molePerGram", "molePerKilogram", "molePerKilogram", "molePerKilogramPerSecond", 
                    "molePerLiter", "molePerMeterCubed", "molePerMeterSquaredPerSecond", 
                    "molePerMole", "molesPerGram", "molesPerKilogram", "molesPerKilogramPerSecond", 
                    "nanogramPerGram", "nanogramPerGramPerHour", "nanoliterPerLiter", 
                    "nanomolePerGramPerDay", "nanomolePerGramPerHour", "nanomolePerGramPerSecond", 
                    "nanomolePerKilogram", "nanomolePerLiter", "nanomolePerMole", 
                    "nanomolesPerGramPerSecond", "nanosecond", "newton", "nominalDay", 
                    "nominalHour", "nominalLeapYear", "nominalMinute", "nominalWeek", 
                    "nominalYear", "number", "numberPerGram", "numberPerHectare", 
                    "numberPerKilometerSquared", "numberPerLiter", "numberPerMeterCubed", 
                    "numberPerMeterSquared", "numberPerMilliliter", "ohm", "ohmMeter", 
                    "pascal", "percent", "permil", "pint", "poundPerAcre", "poundPerInchSquared", 
                    "poundsPerSquareInch", "quart", "second", "siemen", "siemens", 
                    "siemensPerCentimeter", "siemensPerMeter", "sievert", "squareCentimeters", 
                    "squareFoot", "squareKilometers", "squareMeter", "squareMeterPerKilogram", 
                    "squareMile", "squareMillimeters", "squareYard", "tesla", "tonnePerHectare", 
                    "tonnePerYear", "tonnesPerYear", "volt", "watt", "wattPerMeterSquared", 
                    "wattPerMeterSquaredPerNanometer", "wattPerMeterSquaredPerNanometerPerSteradian", 
                    "wattPerMeterSquaredPerSteradian", "waveNumber", "weber", "yardPerSecond", 
                    "yardSquared", "radian", "degree", "grad", "steradian", "meter", 
                    "nanometer", "micrometer", "micron", "millimeter", "centimeter", 
                    "decimeter", "dekameter", "hectometer", "kilometer", "megameter", 
                    "angstrom", "inch", "Foot_US", "foot", "Foot_Gold_Coast", "fathom", 
                    "nauticalMile", "yard", "Yard_Indian", "Link_Clarke", "Yard_Sears", 
                    "mile")
usethis::use_data(standard_units, overwrite = TRUE)

# storage type
storage_type <- list(string = "string", boolean = "boolean", decimal = "decimal", 
                     float = "float", double = "double", duration = "duration", 
                     dateTime = "dateTime", time = "time", date = "date", gYearMonth = "gYearMonth", 
                     gYear = "gYear", gMonthDay = "gMonthDay", gDay = "gDay", 
                     gMonth = "gMonth")
usethis::use_data(storage_type, overwrite = TRUE)

# word example
word_example <- c("abstract_template.docx", "Banet-Example", "Hannon-Example", 
                  "methods-template.docx", "multi-dataset-example")
usethis::use_data(word_example, overwrite = TRUE)

# generate_report_df()
# TODO what is this function?
