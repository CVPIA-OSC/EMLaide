# if (missing(attribute_name)) {stop('Please provide this attributes name.', call. = FALSE)}
# attribute_name <- attribute_name
# 
# if (!is.null(attribute_label)) {
#   attribute_list$attribute$attributeLabel <- attribute_label
# }
# 
# if (missing(attribute_definition)) {stop('Please provide this attributes definition.', call. = FALSE)}
# attribute_definition <- attribute_definition
# 
# if (missing(storage_type)) {stop('Please provide the storage type.', call. = FALSE)}
# storage_type <- storage_type
# 
# if (missing(measurement_scale)) {stop('Please list one of the approved measurement scales.',call. = FALSE)}
# 
# if (!is.null(attribute_label)) {
#   attribute_list$attribute$attributeLabel <- attribute_label
# }
# 
# if (measurement_scale == "nominal") {
#   nominal_scale_definition <- nominal_scale_definition
#   attribute_list$attribute$measurementScale <- list(nominal = list(nonNumericDomain = list(textDomain =
#                                                                                              list(domain = nominal_scale_definition))))
# }else{
#   if (measurement_scale == "ratio") {
#     attribute_list$attribute$measurementScale <- list(ratio = list(unit = list(standardUnit = units,
#                                                                                precision = unit_precision),
#                                                                    numericDomain = list(numberType = number_type)))
#   }else{
#     if (measurement_scale == "ordinal") {
#       attribute_list$attribute$measurementScale <- list(ordinal = list(nonNumericDomain =
#                                                                          list(enumeratedDomain =
#                                                                                 list(codeDefinition = list(code = code_number,
#                                                                                                            definition = code_number_definition)))))
#     }else{
#       if (measurement_scale == "interval") {
#         attribute_list$attribute$measurementScale <- list(interval = list(unit = list(standardUnit = units),
#                                                                           precision = unit_precision,
#                                                                           numericDomain = list(numberType = number_type,
#                                                                                                bounds = list(minimum = minimum,
#                                                                                                              maximum = maximum))))
#       }else{
#         if (measurement_scale == "dateTime") {
#           attribute_list$attribute$measurementScale <- list(dateTime = list(formatString = ISO_date_time,
#                                                                             dateTimePrecision = date_time_precision,
#                                                                             dateTimeDomain = list(bounds = list(minimum = minimum,
#                                                                                                                 maximum = maximum))))
#         }
#       }}}}
# 
# # if (measurement_scale == "nominal") {
# #   attribute_list$attribute$measurementScale <- list(nominal = list(nonNumericDomain = list(textDomain =
# #                                                                                             list(domain = nominal_scale_definition))))
# # }
# # 
# # if (measurement_scale == "ratio") {
# #   units <- units
# #   unit_precision <- unit_precision
# #   number_type <- number_type
# #   attribute_list$attribute$measurementScale <- list(ratio = list(unit = list(standardUnit = units,
# #                                                                             precision = unit_precision),
# #                                                                 numericDomain = list(numberType = number_type)))
# # }
# # 
# # if (measurement_scale == "ordinal") {
# #   code_number <- code_number
# #   code_number_definition <- code_number_definition
# #   attribute_list$attribute$measurementScale <- list(ordinal = list(nonNumericDomain =
# #                                                                     list(enumeratedDomain =
# #                                                                            list(codeDefinition = list(code = code_number,
# #                                                                                                       definition = code_number_definition)))))
# # }
# # 
# # if (measurement_scale == "interval") {
# #   units <- units
# #   unit_precision <- unit_precision
# #   number_type <- number_type
# #   minimum <- minimum
# #   maximum <- maximum
# #   attribute_list$attribute$measurementScale <- list(interval = list(unit = list(standardUnit = units),
# #                                                                    precision = unit_precision,
# #                                                                    numericDomain = list(numberType = number_type,
# #                                                                                         bounds = list(minimum = minimum,
# #                                                                                                       maximum = maximum))))
# # }
# # 
# # if (measurement_scale == "dateTime") {
# #   ISO_date_time <- ISO_date_time
# #   date_time_precision <- date_time_precision
# #   minimum <- minimum
# #   maximum <- maximum
# #   attribute_list$attribute$measurementScale <- list(dateTime = list(formatString = ISO_date_time,
# #                                                                    dateTimePrecision = date_time_precision,
# #                                                                    dateTimeDomain = list(bounds = list(minimum = minimum,
# #                                                                                                        maximum = maximum))))
# # }
# 
# attribute_list$attribute <- list(attributeName = attribute_name,
#                                  attributeDefinition = attribute_definition,
#                                  storageType = storage_type)
# 
# return(attribute_list)
# }
# 
# 
# 
# # if (!is.null(attribute_label)) {
# #   attributeList$attribute$attributeLabel <- attribute_label
# # }
# # 
# # if (measurement_scale == "nominal") {
# #   attributeList$attribute$measurementScale <- list(nominal = list(nonNumericDomain = list(textDomain =
# #                                               list(domain = nominal_scale_definition))))
# # }else{
# # if (measurement_scale == "ratio") {
# #   attributeList$attribute$measurementScale <- list(ratio = list(unit = list(standardUnit = units,
# #                                                                             precision = unit_precision),
# #                                                                 numericDomain = list(numberType = number_type)))
# # }else{
# # if (measurement_scale == "ordinal") {
# #   attributeList$attribute$measurementScale <- list(ordinal = list(nonNumericDomain =
# #                                                              list(enumeratedDomain =
# #                                                              list(codeDefinition = list(code = code_number,
# #                                                                                         definition = code_number_definition)))))
# # }else{
# # if (measurement_scale == "interval") {
# #   attributeList$attribute$measurementScale <- list(interval = list(unit = list(standardUnit = units),
# #                                                                    precision = unit_precision,
# #                                                                    numericDomain = list(numberType = number_type,
# #                                                                                         bounds = list(minimum = minimum,
# #                                                                                                       maximum = maximum))))
# # }else{
# # if (measurement_scale == "dateTime") {
# #   attributeList$attribute$measurementScale <- list(dateTime = list(formatString = ISO_date_time,
# #                                                                    dateTimePrecision = date_time_precision,
# #                                                                    dateTimeDomain = list(bounds = list(minimum = minimum,
# #                                                                                                        maximum = maximum))))
# # }
# # }}}}