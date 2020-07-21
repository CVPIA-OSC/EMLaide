#' @title Add Atribute List 
#' @description adds the attribute list element according to EML standards 
#' @param attribute_name TODO
#' @param attribute_label TODO
#' @param attribute_defenition TODO
#' @param storage_type TODO
#' @param measurement_scale TODO
#' @param measurement_scale_description TODO
#' @param units TODO
#' @param number_type TODO
#' @return the project or dataset list with an attribute list appended
#' @examples TODO
#' @export

add_attribute_list <- function(attribute_name, attribute_label, attribute_definition,
                               storage_type, measurement_scale, measurement_scale_description,
                               units, number_type) {
  attributeList <- list(attribute = list(attributeName = attribute_name,
                                         attributeLabel = attribute_label,
                                         attributeDefinition = attribute_definition,
                                         storageType = storage_type))
  
  
  if (measurement_scale == "nominal") {
    attributeList$attribute$measurementScale <- list(nominal = list(nonNumericDomain = list(textDomain =
                                                list(domain = measurement_scale_description))))
  }
  
  if (measurement_scale == "ratio") {
    attributeList$attribute$measurementScale <- list(ratio = list(unit = list(standardUnit = units),
                                                                  numericDomain = list(numberType = number_type)))
  }
  
  
  
  

}