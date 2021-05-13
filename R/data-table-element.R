#' @title Create Data Table 
#' @description Creates the data table elements according to EML standards. 
#' @param filename Name of the file you would like added to the datatable element
#' @param attribute_info File path to the attribute sheet that contains metadata on attributes for your file
#' @param datatable_description Short description of contents of datatable you are adding 
#' @param datatable_url The URL to the datatable you are adding
#' @param dataset_methods Optional if there are methods that are specific to a datatable
#' @param additional_info Option if there is additional metadata information that is important to the datatable 
#' @param methods A set of information of the specific methods used to collect
#' information in this entity.  
#' @return A data table element listed in a formate to be added to a dataset element and turned into EML 
#' @examples
#' 
#' @export
create_data_table <- function(filename, 
                              attribute_info, 
                              datatable_description, 
                              datatable_url = NULL, 
                              dataset_methods = NULL, 
                              additional_info = NULL){
  
  attribute_table <- readxl::read_xlsx(attribute_info, sheet = "attribute")
  codes <- readxl::read_xlsx(attribute_info, sheet = "code_definitions")
  attribute_list <- list()
  attribute_names <- unique(codes$attribute_name)
  
  # Code helper function 
  code_helper <- function(code, definitions) {
    codeDefinition <- list(code = code, definition = definitions)
  }
  # Attribute helper function to input into pmap
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
    new_attribute <- add_attribute(attribute_name = attribute_name, attribute_definition = attribute_definition,
                                   storage_type = storage_type, measurement_scale = measurement_scale, 
                                   domain = domain, definition = definition, type = type, units = units, 
                                   unit_precision = unit_precision, number_type = number_type, 
                                   date_time_format = date_time_format, date_time_precision = date_time_precision, 
                                   minimum = minimum, maximum = maximum, attribute_label = attribute_label)
  }
  attribute_list$attribute <- purrr::pmap(attribute_table, attributes_and_codes)
  
  physical <- add_physical(file_path = filename, data_url = datatable_url)
  dataTable <- list(entityName = filename,
                                 entityDescription = datatable_description,
                                 physical = physical,
                                 attributeList = attribute_list,
                                 numberOfRecords = nrow(read_csv(file_name)))
  return(dataTable)
}
#' Add Data Table 
#' @param parent_element A list representing the EML project or dataset.
#' @param datatable_metadata A named list or dataframe containing datatable elements: see \code{\link{create_data_table}} 
#' 
#' @export
#' datatable_metadata <- dplyr::tibble(filename =  c("enclosure-study-growth-rate-data.csv", "enclosure-study-gut-contents-data.csv"), 
#'                                     attribute_info = c("enclosure-study-growth-rates-metadata.xlsx","enclosure-study-gut-contents-metadata.xlsx"),
#'                                     datatable_description = c("Growth Rates - Enclosure Study","Gut Contents - Enclosure Study"),
#'                                     datatable_url = c("https://raw.githubusercontent.com/FlowWest/CVPIA_Salmonid_Habitat_Monitoring/make-xml/data/enclosure-study-growth-rate-data.csv?token=AMGEQ7R4E5RMNKRMD57BBQTAOSW6W",
#'                                                      "https://raw.githubusercontent.com/FlowWest/CVPIA_Salmonid_Habitat_Monitoring/make-xml/data/enclosure-study-gut-contents-data.csv?token=AMGEQ7VJADFEYARKPUM4AYTAOSXAQ"))
#' 
#' dataset <- list() %>%
#'     add_data_table(datatable_metadata)
#' 
add_data_table <- function(parent_element, datatable_metadata) {
  data_tables <- purrr::pmap(datatable_metadata, create_data_table) 
  parent_element$dataTable <- data_tables
  return(parent_element)
}