#' Create Vector Element 
#' @title Create Vector Element
#' @description This function creates all the required elements for a \code{spatialVector} section of
#' an ELM document. 
#' @param parent_element A list to append \code{spatialVector} to 
#' @param file_name The name of your vector file
#' @param file_description A short description of your file
#' @param attribute_info File path to metadata excel that contains attribute 
#' information for vector data
#' @param physical A list of the physical descriptions of your file. Use 
#' \code{\link{create_physical}} to generate this physical list and see the 
#' documentation for \code{\link{create_physical}} for more information. 
#' @param geometry Geometric representation of the file
#'
#' @return A list that contains all the required elements of the spatialVector 
#' section of an EML document. 
#'
#' @examples
#' \dontrun{
#' create_vector(file_name = "vectorfiles.tif" ,
#'               file_description = "A vector File",
#'               attribute_info =  "metadata_vectorfiles.xlsx",
#'               physical = create_physical("vectorfiles.tif"),
#'               geometry = "pixel")}
#'
#' @export            
create_vector <- function(file_name, file_description, attribute_info, physical,
                          geometry) {
  
  required_arguments <- c("file_name", "file_description", "attribute_info", "physical",
                          "geometry")
  
  missing_argument_index <- which(c(missing(file_name), missing(file_description), 
                                    missing(attribute_info), missing(physical), missing(geometry)))
  
  if (length(missing_argument_index) > 0) {
    vector_error <- required_arguments[missing_argument_index][1]
    vector_error_message <- paste("Please supply the", vector_error)
    stop(vector_error_message, call. = FALSE)
  }
  attribute_table <- readxl::read_xlsx(attribute_info, sheet = "attribute")
  codes <- readxl::read_xlsx(attribute_info, sheet = "code_definitions")
  attribute_list <- list()
  attribute_names <- unique(codes$attribute_name)
  
  attributes_and_codes <- function(attribute_name, attribute_definition, storage_type, 
                                   measurement_scale, domain, type, units, unit_precision, 
                                   number_type, date_time_format, date_time_precision, minimum, maximum, 
                                   attribute_label){
    if (domain %in% "enumerated") { 
      definition <- list()
      current_codes <- codes[codes$attribute_name == attribute_name, ]
      definition$codeDefinition <- purrr::pmap(current_codes %>% select(-attribute_name), code_helper) 
    } else {
      definition = attribute_definition
    }
    new_attribute <- create_attribute(attribute_name = attribute_name, attribute_definition = attribute_definition,
                                      measurement_scale = measurement_scale, 
                                      domain = domain, definition = definition, type = type, units = units, 
                                      unit_precision = unit_precision, number_type = number_type, 
                                      date_time_format = date_time_format, date_time_precision = date_time_precision, 
                                      minimum = minimum, maximum = maximum, attribute_label = attribute_label)
  }
  attribute_list$attribute <- purrr::pmap(attribute_table, attributes_and_codes)
  
  spatialVector <- list(entityName = file_name, 
                        entityDescription = file_description,
                        attributeList = attribute_list,
                        physical = physical,
                        geometry = geometry)
  return(spatialVector)
}
#' Add vector File
#' @description Adds the vector elements to a dataset list according to EML standards. 
#' @param parent_element A list representing the EML project or dataset.
#' @param vector_metadata A named list or dataframe containing vector metadata elements
#' (file_name, file_description, attribute_info, physical, geometetry): see \code{\link{create_vector}} 
#' @return The dataset list or project with vector file information appended.
#' @examples  
#' vector_metadata <- list("file_name" = "vectorfiles.tif",
#'                         "file_description" = "A vector File",
#'                         attribute_info =  "metadata_vectorfiles.xlsx",
#'                         "physical" = create_physical("vectorfiles.tif"),
#'                         "geometry" = "pixel")
#' \dontrun{
#' dataset <- list() %>%
#'     add_vector(vector_metadata)
#' dataset
#' }
#' @export
add_vector <- function(parent_element, vector_metadata) {
  parent_element$spatialVector <- create_vector(file_name = vector_metadata$file_name, 
                                                file_description = vector_metadata$file_description, 
                                                attribute_info = vector_metadata$attribute_info, 
                                                physical = vector_metadata$physical,
                                                geometry = vector_metadata$geometry)
  return(parent_element)
}