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

CVPIA_common_species <- list(chinook = "chinook", steelhead = "steelhead", 
                             delta_smelt = "delta_smelt", white_sturgeon = "white_sturgeon",
                             green_sturgeon = "green_sturgeon")
usethis::use_data(CVPIA_common_species)