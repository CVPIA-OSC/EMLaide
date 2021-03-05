# Tests for add_attribute function 

test_that('Correct error and warning messages are produced.', {
  
  expect_error(add_attribute(attribute_definition = "Latitude", attribute_label = "Lat",
                             storage_type = "coordinate", measurement_scale = "ordinal",
                             domain = "text", definition = "Latitude", text_pattern = "Latitudes"),
               'Please provide attribute name.')
  
  expect_error(add_attribute(attribute_name = "LatitudeDD",
                             attribute_label = "Lat", storage_type = "coordinate", measurement_scale = "ordinal",
                             domain = "text", definition = "Latitude", text_pattern = "Latitudes"),
               'Please provide a brief definition of the attribute you are including.')
  
  expect_message(add_attribute(attribute_name = "LatitudeDD", attribute_definition = "Latitude",
                               storage_type = "coordinate", measurement_scale = "ordinal",
                               domain = "text", definition = "Latitude", text_pattern = "Latitudes"),
                 'No attribute label provided.')
  
  expect_error(add_attribute(attribute_name = "LatitudeDD", attribute_definition = "Latitude",
                             attribute_label = "Lat", measurement_scale = "ordinal",
                             domain = "text", definition = "Latitude", text_pattern = "Latitudes"),
               'Please provide a storage type.')
  
  expect_error(add_attribute(attribute_name = "LatitudeDD", attribute_definition = "Latitude",
                             attribute_label = "Lat", storage_type = "coordinate",
                             domain = "text", definition = "Latitude", text_pattern = "Latitudes"),
               'Please provide a measurement scale.')
  
  expect_error(add_attribute(attribute_name = "LatitudeDD", attribute_definition = "Latitude",
                             attribute_label = "Lat", storage_type = "coordinate", measurement_scale = "ordinal",
                             domain = "text", text_pattern = "Latitudes"),
               'Please provide the description for your measurement scale.')
  
  expect_message(add_attribute(attribute_name = "LatitudeDD", attribute_definition = "Latitude",
                               attribute_label = "Lat", storage_type = "coordinate", measurement_scale = "ordinal",
                               domain = "text", definition = "Latitude"),
                 'Warning: Please add text pattern if applicable.')
  
  expect_error(add_attribute(attribute_name = "site_id", attribute_label = "sites", 
                             attribute_definition = "Site id as used in sites table",
                             storage_type = "string", measurement_scale = "nominal", domain = "text",
                             text_pattern = "ids"),
               'Please provide the description for your measurement scale.')
  
  expect_error(add_attribute(attribute_name = "site_id", attribute_label = "sites", 
                             attribute_definition = "Site id as used in sites table",
                             storage_type = "string", measurement_scale = "nominal",
                             definition = "Sites", text_pattern = "ids"),
               'Please provide a domain of "text" or "enumerated" and supply the remaining applicable inputs.')
  
  expect_message(add_attribute(attribute_name = "site_id", attribute_definition = "Site id as used in sites table",
                               storage_type = "string", measurement_scale = "nominal",
                               domain = "text", attribute_label = "sites",
                               definition = "Site id as used in sites table."),
                 'Warning: Please add text pattern if applicable.')
  
  expect_error(add_attribute(attribute_name = "Recap", attribute_definition = "Has the Turtle been captured and tagged previously",
                             storage_type = "text", attribute_label = "Turtles",
                             measurement_scale = "nominal", domain = "enumerated"),
               'Please provide the description for your measurement scale.')
  
  expect_error(add_attribute(attribute_name = "LatitudeDD", attribute_definition = "Latitude",
                             attribute_label = "Latitude", storage_type = "coordinate",
                             measurement_scale = "ordinal", definition = "Latitude",
                             text_pattern = "NA"),
               'Please provide a domain of "text" or "enumerated" and supply the remaining applicable inputs.')
  
  expect_error(add_attribute(attribute_name = "LatitudeDD", attribute_definition = "Latitude",
                             attribute_label = "Latitude", storage_type = "coordinate",
                             measurement_scale = "ordinal", domain = "text", text_pattern = "NA"),
               'Please provide the description for your measurement scale.')
  
  expect_message(add_attribute(attribute_name = "LatitudeDD", attribute_definition = "Latitude",
                               attribute_label = "Latitude", storage_type = "coordinate",
                               measurement_scale = "ordinal",
                               domain = "text", definition = "Latitude"),
                 'Warning: Please add text pattern if applicable.')
  
  expect_error(add_attribute(attribute_name = "hwa", attribute_definition = "Hemlock woolly adelgid density per meter of branch",
                             storage_type = "number", measurement_scale = "ordinal",
                             attribute_label = "hemlock wolly adelgid", domain = "enumerated"), 
               'Please provide the description for your measurement scale.')
  
  expect_error(add_attribute(attribute_name = "Count", attribute_definition = "Number of individuals observed",
                             attribute_label = "NA", measurement_scale = "interval",
                             storage_type = "integer", type = "interval", unit_precision = "1",
                             number_type = "whole", minimum = "0", maximum = "10"),
               'Please provide what units your measurement scale uses.')
  
  expect_warning(add_attribute(attribute_name = "Count", attribute_definition = "Number of individuals observed",
                             attribute_label = "NA", measurement_scale = "interval",
                             storage_type = "integer", type = "interval", units = "number",
                             number_type = "whole", minimum = "0", maximum = "10"),
               'Please provide what level of precision your measurements use.')
  
  expect_error(add_attribute(attribute_name = "Count", attribute_definition = "Number of individuals observed",
                             attribute_label = "NA", measurement_scale = "interval",
                             storage_type = "integer", type = "interval", units = "number",
                             unit_precision = "1", minimum = "0", maximum = "10"),
               'Please provide what type of numbers are being used.')
  
  expect_error(add_attribute(attribute_name = "Count", attribute_definition = "Number of individuals observed",
                             attribute_label = "NA", measurement_scale = "interval",
                             storage_type = "integer", units = "number",
                             unit_precision = "1", minimum = "0", maximum = "10"),
               "Please provide a type of 'interval' or 'ratio'.")
  
  expect_warning(add_attribute(attribute_name = "Count", attribute_definition = "Number of individuals observed",
                               attribute_label = "NA", measurement_scale = "interval",
                               storage_type = "integer", type = "interval", units = "number",
                               unit_precision = "1", number_type = "whole", maximum = "10"),
                 'Please provide a minimum theoretical value if applicable.')
  
  expect_warning(add_attribute(attribute_name = "Count", attribute_definition = "Number of individuals observed",
                               attribute_label = "NA", measurement_scale = "interval",
                               storage_type = "integer", type = "interval", units = "number",
                               unit_precision = "1", number_type = "whole", minimum = "0"),
                 'Please provide a maximum theoretical value if applicable.')
  
  expect_error(add_attribute(attribute_name = "pH", attribute_definition = "pH of soil solution",
                             attribute_label = "NA", storage_type = "float",
                             measurement_scale = "ratio", type = "ratio", unit_precision = "0.01",
                             number_type = "real", minimum = "0", maximum = "14"),
               'Please provide what units your measurement scale uses.')
  
  expect_warning(add_attribute(attribute_name = "pH", attribute_definition = "pH of soil solution",
                             attribute_label = "NA", storage_type = "float",
                             measurement_scale = "ratio", type = "ratio", units = "dimensionless",
                             number_type = "real", minimum = "0", maximum = "14"),
               'Please provide what level of precision your measurements use.')
  
  expect_error(add_attribute(attribute_name = "pH", attribute_definition = "pH of soil solution",
                             attribute_label = "NA", storage_type = "float",
                             measurement_scale = "ratio", type = "ratio", units = "dimensionless",
                             unit_precision = "0.01", minimum = "0", maximum = "14"),
               'Please provide what type of numbers are being used.')
  
  expect_error(add_attribute(attribute_name = "pH", attribute_definition = "pH of soil solution",
                             attribute_label = "NA", storage_type = "float",
                             measurement_scale = "ratio", units = "dimensionless",
                             unit_precision = "0.01", minimum = "0", maximum = "14"),
               "Please provide a type of 'interval' or 'ratio'.")
  
  expect_warning(add_attribute(attribute_name = "pH", attribute_definition = "pH of soil solution",
                               attribute_label = "NA", storage_type = "float",
                               measurement_scale = "ratio", type = "ratio", units = "dimensionless",
                               unit_precision = "0.01", number_type = "real", maximum = "14"),
                 'Please provide a minimum theoretical value if applicable.')
  
  expect_warning(add_attribute(attribute_name = "pH", attribute_definition = "pH of soil solution",
                               attribute_label = "NA", storage_type = "float",
                               measurement_scale = "ratio", type = "ratio", units = "dimensionless",
                               unit_precision = "0.01", number_type = "real", minimum = "0"),
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


test_that('The attribute function adds attribute elements.', {
  
  expect_equal(add_attribute(attribute_name = "site_id", attribute_definition = "Site id as used in sites table",
                             attribute_label = "NA", storage_type = "string",
                             measurement_scale = "nominal", domain = "text",
                             definition = "Site id as used in sites table.", text_pattern = "NA"),
               list(attributeName = "site_id", attributeDefinition = "Site id as used in sites table", 
                    storageType = "string", attributeLabel = "NA", measurementScale = list(
                      nominal = list(nonNumericDomain = list(textDomain = list(
                        definition = "Site id as used in sites table.", pattern = "NA"))))))
  
  expect_equal(add_attribute(attribute_name = "Recap", attribute_definition = "Has the Turtle been captured and tagged previously",
                             attribute_label = "NS", storage_type = "text",
                             measurement_scale = "nominal", domain = "enumerated",
                             definition = "code_definition"),
               list(attributeName = "Recap", attributeDefinition = "Has the Turtle been captured and tagged previously", 
                    storageType = "text", attributeLabel = "NS", measurementScale = list(
                      nominal = list(nonNumericDomain = list(enumeratedDomain = "code_definition")))))
  
  expect_equal(add_attribute(attribute_name = "LatitudeDD", attribute_definition = "Latitude",
                             attribute_label = "NA", storage_type = "coordinate", measurement_scale = "ordinal",
                             domain = "text", definition = "Latitude", text_pattern = "NA"),
               list(attributeName = "LatitudeDD", attributeDefinition = "Latitude", 
                    storageType = "coordinate", attributeLabel = "NA", measurementScale = list(
                      ordinal = list(nonNumericDomain = list(textDomain = list(
                        definition = "Latitude", pattern = "NA"))))))
  
  expect_equal(add_attribute(attribute_name = "hwa", attribute_definition = "Hemlock woolly adelgid density per meter of branch",
                             attribute_label = "NA", storage_type = "number",
                             measurement_scale = "ordinal", domain = "enumerated", 
                             definition = "code_definition"),
               list(attributeName = "hwa", attributeDefinition = "Hemlock woolly adelgid density per meter of branch", 
                    storageType = "number", attributeLabel = "NA", measurementScale = list(
                      ordinal = list(nonNumericDomain = list(enumeratedDomain = "code_definition")))))
  
  expect_equal(add_attribute(attribute_name = "Count", attribute_definition = "Number of individuals observed",
                             attribute_label = "NA", measurement_scale = "interval",
                             storage_type = "integer", type = "interval", units = "number", unit_precision = "1",
                             number_type = "whole", minimum = "0", maximum = "10"),
               list(attributeName = "Count", attributeDefinition = "Number of individuals observed", 
                    storageType = "integer", attributeLabel = "NA", measurementScale = list(
                      interval = list(unit = list(standardUnit = "number"), 
                                      numericDomain = 
                                        list(numberType = "whole",
                                             bounds = list(minimum = 
                                                             list(exclusive = "false",
                                                                  minimum = "0"), 
                                                           maximum = list(exclusive = "false",
                                                                          maximum = "10"))),
                                                                          precision = "1"))))
  
  expect_equal(add_attribute(attribute_name = "pH", attribute_definition = "pH of soil solution",
                             attribute_label = "NA", storage_type = "float",
                             measurement_scale = "ratio", type = "ratio", units = "dimensionless",
                             unit_precision = "0.01", number_type = "real",
                             minimum = "NA", maximum = "NA"),
               list(attributeName = "pH", attributeDefinition = "pH of soil solution", 
                    storageType = "float", attributeLabel = "NA", measurementScale = list(
                      ratio = list(unit = list(standardUnit = "dimensionless"), numericDomain = list(numberType = "real", 
                                                                            bounds = list(minimum = list(exclusive = "false", 
                                                                                                         minimum = "NA"), maximum = list(exclusive = "false", 
                                                                                                                                         maximum = "NA"))),
                                   precision = "0.01"))))
  
  expect_equal(add_attribute(attribute_name = "Yrs", attribute_label = "Years",
                             attribute_definition = "Calendar year of the observation from years 1990 - 2010.",
                             storage_type = "integer", measurement_scale = "dateTime",
                             date_time_format = "YYYY", date_time_precision = "1",
                             minimum = "1993", maximum = "2003"),
               list(attributeName = "Yrs", attributeDefinition = "Calendar year of the observation from years 1990 - 2010.", 
                    storageType = "integer", attributeLabel = "Years", measurementScale = list(
                      dateTime = list(formatString = "YYYY", dateTimePrecision = "1", 
                                      dateTimeDomain = list(bounds = list(minimum = list(
                                        exclusive = "false", minimum = "1993"), maximum = list(
                                          exclusive = "false", maximum = "2003")))))))
})


# test_that('Correct error and warning messages are produced', {
#   methods <- add_data_table(parent_element = list(), 
#                            entity_description = "Soil CO2 Fluxes 2013-2014",
#                            physical = "physical",
#                            attribute_list = "attribute_list",
#                            number_of_records = "1", 
#                            alternate_identifier = "NA",
#                            methods = method)
#   method <- add_method(methods_file = "tests/testthat/methods_test.docx")
#   expect_error(methods,
#   'Please provide an entity name i.e. a file name, name of database table, etc.')
#   
#   expect_error(add_data_table(parent_element = list(), 
#                               entity_name = "692_EML_IncubationByDepth_SoilCO2Fluxes.csv",
#                               physical = "physical",
#                               attribute_list = "attribute_list",
#                               number_of_records = "1",
#                               alternate_identifier = "NA",
#                               methods = method),
#                'Please provide a brief description of the entity and its contents.')
#   
#   expect_error(add_data_table(parent_element = list(), 
#                               entity_name = "692_EML_IncubationByDepth_SoilCO2Fluxes.csv",
#                               entity_description = "Soil CO2 Fluxes 2013-2014",
#                               attribute_list = "attribute_list",
#                               number_of_records = "1",
#                               alternate_identifier = "NA",
#                               methods = method),
#                'Please provide a full description of the full format of the physical element of your entity using the add_physical function.')
#   
#   expect_error(add_data_table(parent_element = list(), 
#                               entity_name = "692_EML_IncubationByDepth_SoilCO2Fluxes.csv",
#                               entity_description = "Soil CO2 Fluxes 2013-2014",
#                               physical = "physical",
#                               number_of_records = "1",
#                               alternate_identifier = "NA",
#                               methods = method),
#                'Please provide a list of attributes which were used in this data table.')
#   
#   expect_message(add_data_table(parent_element = list(), 
#                               entity_name = "692_EML_IncubationByDepth_SoilCO2Fluxes.csv",
#                               entity_description = "Soil CO2 Fluxes 2013-2014",
#                               physical = "physical",
#                               attribute_list = "attribute_list",
#                               alternate_identifier = "NA",
#                               methods = method),
#                'The number of records was not provided.')
#   
#   expect_message(add_data_table(parent_element = list(), 
#                               entity_name = "692_EML_IncubationByDepth_SoilCO2Fluxes.csv",
#                               entity_description = "Soil CO2 Fluxes 2013-2014",
#                               physical = "physical",
#                               attribute_list = "attribute_list",
#                               number_of_records = "1",
#                               methods = method),
#                'An alternate identifier was not provided.')
#   
#   expect_message(add_data_table(parent_element = list(), 
#                                 entity_name = "692_EML_IncubationByDepth_SoilCO2Fluxes.csv",
#                                 entity_description = "Soil CO2 Fluxes 2013-2014",
#                                 physical = "physical",
#                                 attribute_list = "attribute_list",
#                                 number_of_records = "1"),
#                  'No method of data collection was provided.')
# })

# test_that('The add_data_table function adds the appropriate elements.', {
#   
#   method <- add_method(methods_file = "tests/testthat/methods_test.docx")
# 
#   attribute_1 <- add_attribute(attribute_name = "site_id",
#                                attribute_definition = "Site id as used in sites table",
#                                storage_type = EDIutils::storage_type$integer,
#                                measurement_scale = EDIutils::measurement_scale$nominal,
#                                domain = "text",
#                                definition = "Site id as used in sites table.")
#   attribute_2 <- add_attribute(attribute_name = "LatitudeDD",
#                                attribute_definition = "Latitude",
#                                storage_type = EDIutils::storage_type$string,
#                                measurement_scale = EDIutils::measurement_scale$ordinal,
#                                domain = "text",
#                                definition = "Latitude")
#   attribute_list <- list(attribute_1, attribute_2)

#   physical <- add_physical(file_path = "test_data.csv",
#                            data_url = "https://mydata.org/etc")
#   
#   expect_equal(add_data_table(parent_element = list(), 
#                               entity_name = "692_EML_IncubationByDepth_SoilCO2Fluxes.csv",
#                               entity_description = "Soil CO2 Fluxes 2013-2014",
#                               physical = physical,
#                               attribute_list = attribute_list,
#                               number_of_records = "1",
#                               alternate_identifier = "NA", 
#                               methods = method), 
#                list(dataTable = list(entityName = "692_EML_IncubationByDepth_SoilCO2Fluxes.csv", 
#                                      entityDescription = "Soil CO2 Fluxes 2013-2014", physical = list(
#                                        objectName = "test_data.csv", size = list(unit = "bytes", 
#                                                                                  size = "322"), authentication = list(method = "MD5", 
#                                                                                                                       authentication = "ee28a90141e061821c891e1172f2eec1"), 
#                                        dataFormat = list(textFormat = list(numHeaderLines = "1", 
#                                                                            recordDelimiter = "\\r\\n", attributeOrientation = "column", 
#                                                                            simpleDelimited = list(fieldDelimiter = ","))), distribution = list(
#                                                                              online = list(url = list(url = "https://mydata.org/etc", 
#                                                                                                       `function` = "download")))), attributeList = list(
#                                                                                                         list(attributeName = "site_id", attributeDefinition = "Site id as used in sites table", 
#                                                                                                              storageType = NULL, measurementScale = list(nominal = list(
#                                                                                                                nonNumericDomain = list(textDomain = list(definition = "Site id as used in sites table."))))), 
#                                                                                                         list(attributeName = "LatitudeDD", attributeDefinition = "Latitude", 
#                                                                                                              storageType = "string", measurementScale = list(ordinal = list(
#                                                                                                                nonNumericDomain = list(textDomain = list(definition = "Latitude")))))), 
#                                      numberOfRecords = "1", alternateIdentifier = "NA", sampling = NULL, 
#                                      methodStep = list(instrumentation = character(0), software = NULL, 
#                                                        description = list(section = list("<title>Title 1</title>\n<para>\n    This is the first paragraph.\n  </para>\n<para>\n    This is the second paragraph.\n  </para>", 
#                                                                                          "<title>Title 2 </title>\n<para>\n    This is the third paragraph.\n  </para>\n<para>\n    This is the fourth paragraph.\n  </para>"), 
#                                                                           para = list())))))
#   
# })

#Tests for add_physical function

# test_that('Correct error and warning messages are produced for the add_physical function', {
#   physical <- add_physical(file_path = "test_data.csv")
#   expect_message(physical,
#                  'No url has been provided. Please input a url to which the data file can be downloaded if possible.')
# })

test_that('The add_physical function adds the physical elements', {
  
  expect_equal(add_physical(file_path = "test_data.csv",
                            data_url = "https://mydata.org/etc"),
               list(objectName = "test_data.csv", size = list(unit = "bytes", 
                                                              size = "322"), authentication = list(method = "MD5", authentication = "ee28a90141e061821c891e1172f2eec1"), 
                    dataFormat = list(textFormat = list(numHeaderLines = "1", 
                                                        recordDelimiter = "\\r\\n", attributeOrientation = "column", 
                                                        simpleDelimited = list(fieldDelimiter = ","))), distribution = list(
                                                          online = list(url = list(url = "https://mydata.org/etc", 
                                                                                   `function` = "download")))))
  
  expect_equal(add_physical(file_path = "test_data.csv",
                            number_of_headers = "1",
                            record_delimiter = "\\r\\n",
                            attribute_orientation = "column",
                            field_delimiter = ",",
                            data_url = "https://mydata.org/etc"),
               list(objectName = "test_data.csv", size = list(unit = "bytes", 
                                                              size = "322"), authentication = list(method = "MD5", authentication = "ee28a90141e061821c891e1172f2eec1"), 
                    dataFormat = list(textFormat = list(numHeaderLines = "1", 
                                                        recordDelimiter = "\\r\\n", attributeOrientation = "column", 
                                                        simpleDelimited = list(fieldDelimiter = ","))), distribution = list(
                                                          online = list(url = list(url = "https://mydata.org/etc", 
                                                                                   `function` = "download")))))
})



