#' Create Other Entity Element
#' @title Create Other Entity Element
#' @description This function creates all the required elements for a \code{otherEntity} section of
#' an ELM document. This should be used if data cannot be added using another more structured format. 
#' @param parent_element A list to append \code{otherEntity} to 
#' @param file_name The name of your entity file
#' @param file_description A short description of your file
#' @param file_type Type of file added as another entity
#' @param physical Physical describtion of object use \code{physical} to generate
#' @return A list that contains all the required elements of the otherEntity
#' section of an EML document. 
#'
#' @examples
#' \dontrun{
#' create_other_entity(file_name = "other_entity.zip" ,
#'                     file_description = "An other entity zip",
#'                     "physical" = add_physical("other_entity.zip")
#'                     )
#' }
#'
#' @export            
create_other_entity <- function(file_name, file_description, file_type, physical) {
  
  required_arguments <- c("file_name", "file_description", "file_type", "physical")
  
  missing_argument_index <- which(c(missing(file_name), missing(file_description), 
                                    missing(file_type), missing(physical)))
  
  if (length(missing_argument_index) > 0) {
    other_entity_error <- required_arguments[missing_argument_index][1]
    other_entity_error_message <- paste("Please supply the", other_entity_error)
    stop(other_entity_error_message, call. = FALSE)
  }
  
  otherEntity <- list(entityName = file_name, 
                      entityDescription = file_description, 
                      entityType = file_type, 
                      physical = physical
  )
  return(otherEntity)
}
#' Add Other Entity 
#' @description Adds the other elements to a dataset list according to EML standards. 
#' @param parent_element A list representing the EML project or dataset.
#' @param other_entity_metadata A named list or dataframe containing other entity metadata elements
#' (file_name, file_description, file_type, physical): see \code{\link{create_other_entity}} 
#' @return The dataset list or project with other entity file information appended.
#' @examples  
#' other_entity_metadata <- list("file_name" = "other_entity.zip",
#'                         "file_description" = "An other entity File",
#'                         "physical" = add_physical("other_entity.zip")
#'                         )
#' \dontrun{
#' dataset <- list() %>%
#'     add_other_entity(other_entity_metadata)
#' dataset
#' }
#' @export
add_other_entity <- function(parent_element, other_entity_metadata) {
  parent_element$otherEntity <- create_other_entity(file_name = other_entity_metadata$file_name, 
                                                    file_description = other_entity_metadata$file_description, 
                                                    file_type = other_entity_metadata$file_type, 
                                                    physical = other_entity_metadata$physical
  )
  return(parent_element)
}