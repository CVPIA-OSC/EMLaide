#' @title Add Maintenance Element 
#' @description Adds the maintenance information of a dataset based off of EML standards.
#' @param parent_element A list representing the EML project or dataset.
#' @param status Provide the status of your project or dataset as either complete or ongoing. 
#' @return The dataset or project with maintenance information appended.
#' @examples 
#' add_maintenance(parent_element = list(), status = ongoing)
#' @export 
add_maintenance <- function(parent_element, status) {
  
  if (missing(status)){stop('Please provide the status of your project or dataset.', call. = FALSE)}
  
  parent_element$maintenance <- list(description = status)
  
  return(parent_element)
}
