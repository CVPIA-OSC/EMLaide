#' @title Add Maintenance Element 
#' @description Adds the maintenance information of a dataset based off of EML standards.
#' @param parent_element A list representing the EML project or dataset.
#' @param status Provide the status of your project or dataset as either complete or ongoing.
#' @param update_frequency Only needed if the status of the project or dataset is ongoing. 
#' If this is the case, please provide the frequency of which the project or dataset is updated.  
#' @return The dataset or project with maintenance information appended.
#' @examples 
#' add_maintenance(parent_element = list(),
#'                 status = "complete")
#' 
#' add_maintenance(parent_element = list(),
#'                 status = "ongoing",
#'                 update_frequency = "Data are updated 
#'                 annually at the end of the calendar year.")
#' @export 
add_maintenance <- function(parent_element, status, update_frequency = NULL) {
  
  if (missing(status)) {stop('Please provide the status of your project or dataset.', call. = FALSE)}
  
  if(status == "complete") {
    parent_element$maintenance <- list(description = status)
  }
  
  if(status == "ongoing") {
    if (is.null(update_frequency))
      {stop('Please provide the frequency of when this project or dataset is updated.', call. = FALSE)}
    
    parent_element$maintenance$description = list(para = update_frequency)
  }
  
  return(parent_element)
}
