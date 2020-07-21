#' @title Add Data Table 
#' @description adds the data table elements according to EML standards 
#' @param parament_element a list representing the EML project or dataset
#' @param alternate_identifer TODO
#' @param entity_name TODO
#' @param entity_description TODO
#' @param object_name TODO
#' @param number_of_headers TODO
#' @param record_delimiter TODO
#' @param physical_line_delimiter TODO
#' @param attribute_orientation TODO
#' @param field_delimiter TODO
#' @param online_url TODO
#' @param attribute_list TODO 
#' @return the project or dataset list with a data table appended
#' @examples TODO
#' @export
#' 
add_data_table <- function(parent_element, alternate_identifier = NULL, entity_name,
                           entity_description, object_name, number_of_headers, record_delimiter,
                           physcial_line_delimiter, attribute_orientation, field_delimiter,
                           online_url, attribute_list) {
  
  parent_element$dataTable <- list(entityName = entity_name,
                                   entityDescription = entity_description,
                                   physical = list(objectName = object_name,
                                                   dataFormat = list(textFormat = list(numHeaderLines = number_of_headers,
                                                                                  recordDelimiter = record_delimiter,
                                                                                  physicalLineDelimiter = physcial_line_delimiter,
                                                                                  attributeOrientation = attribute_orientation,
                                                                                  simpleDelimited = list(fieldDelimiter = field_delimiter))),
                                                   distribution = list(online = list(url = online_url))))
  if (!is.null(attribute_list)) { 
    parent_element$dataTable$attributeList <- attribute_list
  }
  
  return(parent_element)
}