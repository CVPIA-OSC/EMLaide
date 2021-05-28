#' Create Maintenance
#' @title Create Maintenance Element 
#' @description Creates the maintenance information of a dataset based off of EML standards.
#' @param status Provide the status of your project or dataset as either complete or ongoing.
#' @param update_frequency Only needed if the status of the project or dataset is ongoing. 
#' If this is the case, please provide the frequency of which the project or dataset is updated.  
#' @return A maintenance element that can be appended to a dataset list. 
#' @examples 
#' create_maintenance(status = "complete")
#' 
#' create_maintenance(status = "ongoing",
#'                    update_frequency = "annually")
#' @export 
create_maintenance <- function(status = c("complete", "ongoing"), 
                               update_frequency = NULL) {
  maintenance <- list()
  if (missing(status)) {stop('Please provide the status of your project or dataset.', call. = FALSE)}
  status <- match.arg(status)

    maintenance$description <- status

  if (status == "ongoing") {
    if (is.null(update_frequency)) {
      stop('Please provide the frequency of when this project or dataset is updated.', call. = FALSE)
      }
    maintenance$maintenanceUpdateFrequency <- update_frequency
  }
  return(maintenance)
}

#' Add Maintenance
#' @description Adds the maintenance metadata element to a dataset list according to EML standards. 
#' @param parent_element A list representing the EML project or dataset.
#' @param maintenance_metadata A table or list containing maintenance metadata: see \code{\link{create_maintenance}} 
#' @return The dataset list or project with maintenance information appended.
#' @examples 
#' maintenance_metadata <- list(status = "ongoing", update_frequency = "monthly")
#' dataset <- list() %>%
#'      add_maintenance(maintenance_metadata) 
#' dataset
#' @export

add_maintenance <- function(parent_element, maintenance_metadata) {
  parent_element$maintenance <- create_maintenance(status = maintenance_metadata$status,
                                                   update_frequency = maintenance_metadata$update_frequency)
  return(parent_element)
}
