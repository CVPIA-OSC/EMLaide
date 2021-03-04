#' @title Add Data Table 
#' @description Adds the data table elements according to EML standards. 
#' @param parent_element A list representing the EML project or dataset.
#' @param alternate_identifier Optional. Provide when a dataset or project belongs 
#' to more than the contributing organization. Please include each additional 
#' individual ID with its own alternate_identifier.
#' @param entity_name The name of the table, file, or database table. Often 
#' thought of as the original ascii file name. 
#' @param entity_description A longer, more descriptive explanation of the data in the entity. 
#' @param physical A description of the physical format of the entity.
#' This includes it file name, authentication code, and data format. Further information
#' can be seen at \code{\link{add_physical}} . 
#' @param attribute_list Describes all variables in a data entity in individual 
#' attribute elements. These descriptions include the name and definition of each 
#' attribute, its domain, definition of coded values, and other pertinent information. 
#' It is further explained and can be appended with the \code{\link{add_attribute}} function.
#' @param methods A set of information of the specific methods used to collect
#' information in this entity.  
#' @param number_of_records Optional. A count of the number of records in the data table. 
#' @return the project or dataset list with a data table appended
#' @examples
#' attribute_1 <- add_attribute(attribute_name = "site_id",
#'                              attribute_definition = "Site id as used in sites table",
#'                              storage_type = EMLaide::storage_type$integer,
#'                              measurement_scale = EMLaide::measurement_scale$nominal,
#'                              domain = "text",
#'                              definition = "Site id as used in sites table.")
#' attribute_2 <- add_attribute(attribute_name = "LatitudeDD", 
#'                              attribute_definition = "Latitude",
#'                              storage_type = EMLaide::storage_type$string,
#'                              measurement_scale = EMLaide::measurement_scale$ordinal,
#'                              domain = "text", 
#'                              definition = "Latitude")
#' attribute_list <- list(attribute_1, attribute_2)
#' physical <- add_physical(file_path = "User/data/example.csv",
#'                          data_url = "https://mydata.org/etc")
#' method <- add_method(parent_element = method_list, 
#'                      methods_file = word_example("methods-template.docx"))
#' add_data_table(parent_element = list(), 
#'                entity_name = "692_EML_IncubationByDepth_SoilCO2Fluxes.csv",
#'                entity_description = "Soil CO2 Fluxes 2013-2014", 
#'                physical = physical, 
#'                attribute_list = attribute_list, 
#'                methods = method,
#'                number_of_records = "1")
#' @export
add_data_table <- function(parent_element, entity_name, entity_description, 
                           physical, attribute_list, methods = NULL,
                           number_of_records = NULL, alternate_identifier = NULL) {
  
  required_arguments <- c("entity_name", "entity_description", 
                          "physical", "attribute_list")
  missing_argument_index <- which(c(missing(entity_name), missing(entity_description),
                                    missing(physical), missing(attribute_list)))
  
  if (length(missing_argument_index) > 0) {
    missing <- required_arguments[missing_argument_index][1]
    error_message <- switch(missing,
                            entity_name = "Please provide an entity name i.e. a file name, name of database table, etc.",
                            entity_description = "Please provide a brief description of the entity and its contents.",
                            physical = "Please provide a full description of the full format of the physical element of your entity using the add_physical function.",
                            attribute_list = "Please provide a list of attributes which were used in this data table.")
    stop(error_message, call. = FALSE)
  }
  
  data_table <- list(entityName = entity_name,
                     entityDescription = entity_description,
                     physical = physical,
                     attributeList = attribute_list)
  

  if (is.null(number_of_records)) {
    message('The number of records was not provided.')
  } else {
    data_table$numberOfRecords <- number_of_records
  }
  
  if (is.null(alternate_identifier)) {
    message('An alternate identifier was not provided.')
  } else {
    data_table$alternateIdentifier <- alternate_identifier
  }
  
  if (is.null(methods)) {
    message('No method of data collection was provided.')
  } else {
    data_table <- append(data_table, methods)
  }
  
  if (is.null(parent_element$dataTable)) {
    parent_element$dataTable <- data_table 
  } else {
    parent_element$dataTable <- list(parent_element$dataTable, data_table)
  }
  return(parent_element)
}