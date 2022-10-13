#' Create Data Table
#' @title Create Data Table 
#' @description Creates the data table elements according to EML standards. 
#' @param filepath Name of the file you would like added to the datatable element
#' @param attribute_info File path to the attribute sheet that contains metadata describing all attributes for your datatable.
#' @param datatable_description Short description of contents of datatable you are adding 
#' @param datatable_url (Optional) A URL that links to the datatable you are adding (ex: dataset on AWS public bucket or https://raw.githubusercontent.com/)
#' @param dataset_methods (Optional) metadata describing methods if there are methods that are specific to a datatable
#' @param additional_info (Optional) additional metadata info if there is additional metadata information that is important to the datatable 
#' @return A data table element listed in a format to be added to a dataset element and turned into EML 
#' @examples
#' filepath =  system.file("extdata", "Banet-Example", "data", "enclosure-study-growth-rate-data.csv", package = "EMLaide", mustWork = TRUE)
#' attribute_info = system.file("extdata", "Banet-Example", "metadata", "enclosure-study-growth-rates-metadata.xlsx", package = "EMLaide", mustWork = TRUE)
#' datatable_description = "Growth Rates - Enclosure Study"
#' 
#' \dontrun{create_datatable(filepath, attribute_info, datatable_description)}
#' @export
create_datatable <- function(filepath, 
                              attribute_info, 
                              datatable_description, 
                              datatable_url = NULL, 
                              dataset_methods = NULL, 
                              additional_info = NULL){
 
  raw_attribute_table <- readxl::read_xlsx(attribute_info, sheet = "attribute") 
  # Add in code to reorder columns to match columns in datatable 
  current_metadata_order <- pull(raw_attribute_table, attribute_name)
  current_data_order <- colnames(readr::read_csv(filepath))
  reorder_index <- match(current_data_order, current_metadata_order) 
  
  if (length(current_metadata_order) != length(current_data_order)) {
    error_message <- paste0("Datatable contains ", length(current_data_order)," columns.", 
                           "You provided attribute names for ", length(current_metadata_order), ". Please review ", attribute_info)
    stop(error_message, call. = FALSE)
  }
  if (any(is.na(reorder_index))) {
    current_metadata_diff <- paste0(setdiff(current_metadata_order, current_data_order), collapse = ", ")
    current_data_diff <- paste0(setdiff(current_data_order, current_metadata_order), collapse = ", ")
    error_message <- paste0("These columns from the metadata attribute tab: ", current_metadata_diff, " do not match these columns in the current datatable: ", current_data_diff, ". Please review ", attribute_info)
    stop(error_message, call. = FALSE)
  }
  
  attribute_table <- reorder_attributes_helper(raw_attribute_table, reorder_index)
  
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
  
  physical <- create_physical(file_path = filepath, data_url = datatable_url)
  dataTable <- list(entityName = basename(filepath),
                                 entityDescription = datatable_description,
                                 physical = physical,
                                 attributeList = attribute_list,
                                 numberOfRecords = nrow(read.csv(filepath)))
  return(dataTable)
}

#' Code helper function 
#' @keywords internal  
code_helper <- function(code, definitions) {
  codeDefinition <- list(code = code, definition = definitions)
}

#' Reorder helper function 
#' @keywords internal
reorder_attributes_helper <- function(attribute_metadata_to_reorder, index_of_csv_col_order) {
  attribute_metadata_to_reorder[index_of_csv_col_order, ]
}

#' Add Data Table 
#' @description Adds the data table elements to a dataset list according to EML standards. 
#' @param parent_element A list representing the EML project or dataset.
#' @param datatable_metadata A named list or dataframe containing datatable metadata elements
#' (filepath, attribute_info, datatable_description, and additional optional columns): see \code{\link{create_datatable}} 
#' @return The dataset list or project with datatable information appended.
#' @examples 
#' 
#' datatable_metadata <- dplyr::tibble(filepath = c(system.file("extdata", "Banet-Example", "data", "enclosure-study-growth-rate-data.csv", 
#'                                                              package = "EMLaide", mustWork = TRUE), 
#'                                                              system.file("extdata", "Banet-Example", "data", "enclosure-study-gut-contents-data.csv", 
#'                                                              package = "EMLaide", mustWork = TRUE)), 
#'                                      attribute_info = c(system.file("extdata", "Banet-Example", "metadata", "enclosure-study-growth-rates-metadata.xlsx", 
#'                                                              package = "EMLaide", mustWork = TRUE), 
#'                                      system.file("extdata", "Banet-Example", "metadata", "enclosure-study-gut-contents-metadata.xlsx", 
#'                                                              package = "EMLaide", mustWork = TRUE)),
#'                                      datatable_description = c("Growth Rates - Enclosure Study",
#'                                                                "Gut Contents - Enclosure Study"))
#' \dontrun{
#' dataset <- list() %>%
#'     add_datatable(datatable_metadata)
#' dataset
#' }
#' @export
add_datatable <- function(parent_element, datatable_metadata) {
  suppressMessages(suppressWarnings(
  data_tables <- purrr::pmap(datatable_metadata, create_datatable)))
  parent_element$dataTable <- data_tables
  return(parent_element)
}
