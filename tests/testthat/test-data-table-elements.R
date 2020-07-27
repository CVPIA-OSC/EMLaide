# Tests for add_attribute function 

test_that('Correct error and warning messages are produced.', {
  
  expect_error(add_attribute(attribute_definition = "Latitude", attribute_label = "Lat",
               storage_type = "coordinate", measurement_scale = "ordinal",
               domain= "text", text_definition = "Latitude", text_pattern = "Latitudes"),
               'Please provide attribute name.')
  
  expect_error(add_attribute(attribute_name = "LatitudeDD",
                             attribute_label = "Lat", storage_type = "coordinate", measurement_scale = "ordinal",
                             domain= "text", text_definition = "Latitude", text_pattern = "Latitudes"),
               'Please provide a brief definition of the attribute you are including.')
  
  expect_warning(add_attribute(attribute_name = "LatitudeDD", attribute_definition = "Latitude",
                             storage_type = "coordinate", measurement_scale = "ordinal",
                             domain= "text", text_definition = "Latitude", text_pattern = "Latitudes"),
               'No attribute label provided.')
  
  expect_error(add_attribute(attribute_name = "LatitudeDD", attribute_definition = "Latitude",
                             attribute_label = "Lat", measurement_scale = "ordinal",
                             domain= "text", text_definition = "Latitude", text_pattern = "Latitudes"),
               'Please provide a storage type.')
  
  expect_error(add_attribute(attribute_name = "LatitudeDD", attribute_definition = "Latitude",
                             attribute_label = "Lat", storage_type = "coordinate",
                             domain= "text", text_definition = "Latitude", text_pattern = "Latitudes"),
               'Please provide a measurement scale.')
  
  expect_error(add_attribute(attribute_name = "LatitudeDD", attribute_definition = "Latitude",
                             attribute_label = "Lat", storage_type = "coordinate", measurement_scale = "ordinal",
                             domain= "text", text_pattern = "Latitudes"),
               'Please provide the description for your ordinal measurment scale.')
  
  expect_warning(add_attribute(attribute_name = "LatitudeDD", attribute_definition = "Latitude",
                             attribute_label = "Lat", storage_type = "coordinate", measurement_scale = "ordinal",
                             domain= "text", text_definition = "Latitude"),
               'No text pattern is provided. Please add if applicable.')
  
  expect_error(add_attribute(attribute_name = "site_id", attribute_label = "sites", 
                             attribute_definition = "Site id as used in sites table",
                             storage_type = "string", measurement_scale = "nominal", domain= "text",
                             text_pattern = "ids"),
               'Please provide the description for your nominal measurment scale.')
  
  expect_error(add_attribute(attribute_name = "site_id", attribute_label = "sites", 
                             attribute_definition = "Site id as used in sites table",
                             storage_type = "string", measurement_scale = "nominal",
                             text_pattern = "ids"),
               'Please provide a domain of text or enumerated and supply the remaining applicable inputs.')
  
  expect_warning(add_attribute(attribute_name = "site_id", attribute_definition = "Site id as used in sites table",
                             storage_type = "string", measurement_scale = "nominal", domain= "text",
                             attribute_label = "sites", text_definition = "Site id as used in sites table."),
                 'No text pattern is provided. Please add if applicable.')
  
  expect_error(add_attribute(attribute_name = "Recap", attribute_definition = "Has the Turtle been captured and tagged previously",
                  storage_type = "text", attribute_label = "Turtles", measurement_scale = "nominal", domain = "enumerated"),
               'Please provide a list of your enumerated, nominal codes and their definitions.')
  
  expect_error(add_attribute(attribute_name = "LatitudeDD", attribute_definition = "Latitude",
                  attribute_label = "Latitude", storage_type = "coordinate", measurement_scale = "ordinal",
                  text_definition = "Latitude", text_pattern = "NA"),
               'Please provide a domain of text or enumerated and supply the remaining applicable inputs.')
  
  expect_error(add_attribute(attribute_name = "LatitudeDD", attribute_definition = "Latitude",
                             attribute_label = "Latitude", storage_type = "coordinate", measurement_scale = "ordinal",
                             domain = "text", text_pattern = "NA"),
               'Please provide the description for your ordinal measurment scale.')
  
  expect_warning(add_attribute(attribute_name = "LatitudeDD", attribute_definition = "Latitude",
                             attribute_label = "Latitude", storage_type = "coordinate", measurement_scale = "ordinal",
                             domain = "text", text_definition = "Latitude"),
               'No text pattern is provided. Please add if applicable.')
  
  expect_error(add_attribute(attribute_name = "hwa", attribute_definition = "Hemlock woolly adelgid density per meter of branch",
                             storage_type = "number", measurement_scale = "ordinal",
                             attribute_label = "hemlock wolly adelgid", domain = "enumerated"), 
               'Please provide a list of your enumerated, ordinal codes and their definitions.')
  
  expect_error(add_attribute(attribute_name = "Count", attribute_definition = "Number of individuals observed",
                             attribute_label = "NA", measurement_scale = "interval",  storage_type = "integer",
                             unit_precision = "1", number_type = "whole", minimum = "0", maximum = "10"),
               'Please provide what units your interval measurement scale uses.')
  
  expect_error(add_attribute(attribute_name = "Count", attribute_definition = "Number of individuals observed",
                             attribute_label = "NA", measurement_scale = "interval",  storage_type = "integer", units = "number",
                             number_type = "whole", minimum = "0", maximum = "10"),
               'Please provide what level of precision your interval measurments use.')
  
  expect_error(add_attribute(attribute_name = "Count", attribute_definition = "Number of individuals observed",
                             attribute_label = "NA", measurement_scale = "interval",  storage_type = "integer", units = "number",
                             unit_precision = "1", minimum = "0", maximum = "10"),
               'Please provide what type of numbers are being used.')
  
  expect_warning(add_attribute(attribute_name = "Count", attribute_definition = "Number of individuals observed",
                             attribute_label = "NA", measurement_scale = "interval",  storage_type = "integer", units = "number",
                             unit_precision = "1", number_type = "whole", maximum = "10"),
                 'Please provide a minimum theoretical value if applicable.')
  
  expect_warning(add_attribute(attribute_name = "Count", attribute_definition = "Number of individuals observed",
                             attribute_label = "NA", measurement_scale = "interval",  storage_type = "integer", units = "number",
                             unit_precision = "1", number_type = "whole", minimum = "0"),
               'Please provide a maximum theoretical value if applicable.')
  
  expect_error(add_attribute(attribute_name = "pH", attribute_definition = "pH of soil solution",
                             attribute_label = "NA", storage_type = "float", measurement_scale = "ratio",
                             unit_precision = "0.01", number_type = "real", minimum = "0", maximum = "14"),
               'Please provide what units your ratio measurement scale uses.')
  
  expect_error(add_attribute(attribute_name = "pH", attribute_definition = "pH of soil solution",
                             attribute_label = "NA", storage_type = "float", measurement_scale = "ratio",
                             units = "dimensionless", number_type = "real", minimum = "0", maximum = "14"),
               'Please provide what level of precision your ratio measurments use.')
  
  expect_error(add_attribute(attribute_name = "pH", attribute_definition = "pH of soil solution",
                             attribute_label = "NA", storage_type = "float", measurement_scale = "ratio",
                             units = "dimensionless", unit_precision = "0.01", minimum = "0", maximum = "14"),
               'Please provide what type of numbers are being used.')
  
  expect_warning(add_attribute(attribute_name = "pH", attribute_definition = "pH of soil solution",
                             attribute_label = "NA", storage_type = "float", measurement_scale = "ratio",
                             units = "dimensionless", unit_precision = "0.01", number_type = "real", maximum = "14"),
                 'Please provide a minimum theoretical value if applicable.')
  
  expect_warning(add_attribute(attribute_name = "pH", attribute_definition = "pH of soil solution",
                             attribute_label = "NA", storage_type = "float", measurement_scale = "ratio",
                             units = "dimensionless", unit_precision = "0.01", number_type = "real", minimum = "0"),
               'Please provide a maximum theoretical value if applicable')
  
  expect_error(add_attribute(attribute_name = "Yrs", attribute_label = "Years",
                             attribute_definition = "Calendar year of the observation from years 1990 - 2010.",
                             storage_type = "integer",
                             measurement_scale = "dateTime", 
                             date_time_precision = "1", minimum = "1993", maximum = "2003"),
               'Please provide the correct format of which your date time attribute is in.')
  
  expect_error(add_attribute(attribute_name = "Yrs", attribute_label = "Years",
                             attribute_definition = "Calendar year of the observation from years 1990 - 2010.",
                             storage_type = "integer",
                             measurement_scale = "dateTime", date_time_format = "YYYY",
                             minimum = "1993", maximum = "2003"),
               'Please provide the level of precision your date time attribute has.')
  
  expect_error(add_attribute(attribute_name = "Yrs", attribute_label = "Years",
                             attribute_definition = "Calendar year of the observation from years 1990 - 2010.",
                             storage_type = "integer",
                             measurement_scale = "dateTime", date_time_format = "YYYY",
                             date_time_precision = "1", maximum = "2003"),
               'Please provide the earliest date time used.')
  
  expect_error(add_attribute(attribute_name = "Yrs", attribute_label = "Years",
                             attribute_definition = "Calendar year of the observation from years 1990 - 2010.",
                             storage_type = "integer",
                             measurement_scale = "dateTime", date_time_format = "YYYY",
                             date_time_precision = "1", minimum = "1993"),
               'Please provide the latest date time used.')

})