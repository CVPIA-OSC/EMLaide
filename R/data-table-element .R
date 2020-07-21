#' @title Add Data Table 
#' @description adds the data table elements according to EML standards 
#' @param parament_element a list representing the EML project or dataset
#' @param alternate_identifer TODO
#' @param entity_name The name of the table, file, or database table. Often thought of as the original ascii file name. 
#' @param entity_description A longer, more descriptive explination of the data in the entity. 
#' @param object_name The name of the file when downloaded or exported as text from a database.
#' @param number_of_headers TODO
#' @param record_delimiter TODO
#' @param physical_line_delimiter TODO
#' @param attribute_orientation TODO
#' @param field_delimiter TODO
#' @param online_url A link to access the data.
#' @param attribute_list Describes all variables in a data entity in individual attribute elements. These descriptions include the name and definition of each attribute, its domain, definition of coded values, and other pertinent information. It is further explained in the add_attribute_list function. 
#' @param case_sensitivity Designates the data table as case sensitive or not. Please provide "yes" or "no". 
#' @param number_of_records A count of the number of records in the data table. 
#' @param constraint Describes any integrity constraints between entities (e.g.tables), as they would	be maintained	in a relational	management system.	 
#' @return the project or dataset list with a data table appended
#' @examples TODO
#' @export
#' 
add_data_table <- function(parent_element, alternate_identifier = NULL, entity_name,
                           entity_description, object_name, number_of_headers, record_delimiter,
                           physcial_line_delimiter, attribute_orientation, field_delimiter,
                           online_url, attribute_list, case_sensitivity, number_of_records, constraint) {
  
  parent_element$dataTable <- list(entityName = entity_name,
                                   entityDescription = entity_description,
                                   physical = list(objectName = object_name,
                                                   dataFormat = list(textFormat = list(numHeaderLines = number_of_headers,
                                                                                  recordDelimiter = record_delimiter,
                                                                                  physicalLineDelimiter = physcial_line_delimiter,
                                                                                  attributeOrientation = attribute_orientation,
                                                                                  simpleDelimited = list(fieldDelimiter = field_delimiter))),
                                                   distribution = list(online = list(url = online_url))),
                                   caseSensitive = list(case_sensitivity),
                                   numberOfRecords = list(number_of_records),
                                   constraint = list(constraint))
  if (!is.null(attribute_list)) { 
    parent_element$dataTable$attributeList <- attribute_list
  }
  
  return(parent_element)
}