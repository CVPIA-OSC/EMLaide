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
