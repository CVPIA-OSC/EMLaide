

#' @title Add project element 
#' @description This function creates a project node within the parent element that 
#' contains all the project information. 
#' @param parent_element A list in which the project node should be nested in 
#' @param project_title The title of the project that the funding is awarded to. 
#' @param award_information A list that includes that award title and the funder name 
#' for the funding award. 
#' @param project_personnel A list that includes the project personnel. It must include 
#' the first name, last name, organization, and position for this project. 
#'
#' @return This function returns the parent element with a new project node containing 
#' all project information. 
#' @examples
#' 
#' add_project(project_title = "my project title", 
#'             award_information = list(title = "Money up for grabs",
#'                                      funderName = "Bank",
#'                                      awardNumber = "000",
#'                                      funderIdentifier = "Funder 1",
#'                                      awardUrl = "awardforme.com"),
#'             personnel_info = list(individualName = list(givenName = "Smithy",
#'                                                         surName ="Smith"),
#'                                   organizationName = "US GOV",
#'                                   electronicMailAddress = "myemail@mail.gov",
#'                                   role = "Manager"))

add_project <- function(parent_element, project_title, 
                        award_information, project_personnel) {
  
  missing_argument_index <- which(c(missing(project_title), missing(award_information),
                                    missing(project_personnel)))
  
  if (length(missing_argument_index) > 0) {
    project_error <- required_arguments[missing_argument_index][1]
    project_error_message <- switch(project_error, 
                                    project_title = "Please provide project title",
                                    award_infomration = "Please provide the award information.",
                                    project_personnel = "Please provide the project personnel")
    if (missing(project_title) | missing(award_information) |
        missing(project_personnel)) {
      stop(project_error_message, call. = FALSE)
    }
  }
  if (class(award_information) != "list") {
    award_error = "Please provide a list that includes the award title and funderName."
    stop()
  }
  if (class(project_personnel) != "list") {
    award_error = "Please provide a list that includes the project personnel's name, organization, and position for this project"
    stop()
  }
  
  parent_element$project <- list(title = project_title,
                                 personnel = project_personnel,
                                 award = award_information)
  return(parent_element)
}


