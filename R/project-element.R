#' @title Add project element 
#' @description This function creates a project node within the parent element that 
#' contains all the required elements for the project section of an EML document.
#' This function can be used in combination with \code{add_personnel}
#' and \code{add_funding}. \code{add_personnel} can be used to generate the \code{project_personnel} 
#' input and \code{add_funding} can be used to generate the \code{award_information} input.  
#' @param parent_element A list in which the project node should be nested in. 
#' @param project_title The title of the project that the funding is awarded to. 
#' @param award_information A list that includes the required funding information 
#' for an EML document.This list must include the award title and the funderName.
#' This list can be created by calling the \code{add_funding} function 
#' on the funding information or by manually inputting the required information. 
#' If the list is written manually it must be formatted as follows. 
#'
#' \code{award_infomation = list(funderName = "Name", title = "Award Title")}
#'
#' Additional information about the funding may be added to the list. See the 
#' \code{\link{add_funding}} documentation for more information. 
#'                         
#' @param project_personnel A list that includes the required information on project 
#' personnel for an EML document. It must include the first name, last name, 
#' organization, and personnel role for this project. This list can be created 
#' by calling the \code{add_personnel} function on the project personnel or by manually inputting the required
#' information. If the list is written manually it must be formatted as follows. 
#'
#' \code{project_personnel = list(individualName = list(givenName = "First Name", surName = "Last Name"),
#' role = "Position", organization = "Organization")}
#'
#' Additional information about the project personnel may be added to the list. See the 
#' \code{\link{add_personnel}} documentation for more information.
#' 
#' @return This function returns the parent element with a new project node containing 
#' all project information required for an EML document. 
#' @examples
#' add_project(parent_element = list(), 
#'             project_title = "my project title", 
#'             award_information = add_funding(funder_name = "Bank",
#'                                             funder_identifier = "Funder 1",
#'                                             award_number = "000",
#'                                             award_title = "Money up for grabs",
#'                                             award_url = "awardforme.com"),
#'             project_personnel = add_personnel(parent_element = list(),
#'                                               first_name = "Smithy",
#'                                               last_name = "Smith",
#'                                               email = "myemail@mail.gov",
#'                                               role = "Manager",
#'                                               organization = "US GOV"))
#'                                               
#' add_project(parent_element = list(), 
#'             project_title = "my project title", 
#'             award_information = list(funderName = "Bank",
#'                                      funderIdentifier = "Funder 1",
#'                                      awardNumber = "000",
#'                                      title = "Money up for grabs",
#'                                      awardUrl = "awardforme.com"),
#'             project_personnel = list(individualName = list(givenName = "Smithy",
#'                                                            surName = "Smith"),
#'                                      electronicMailAddress = "myemail@mail.gov",
#'                                      role = "Manager",
#'                                      organizationName = "US GOV"))
#' 
#' @export                                                                          

add_project <- function(parent_element, project_title, 
                        award_information, project_personnel) {
  
  required_arguments <- c("project_title", "award_infomration", "project_personnel")
  
  missing_argument_index <- which(c(missing(project_title), missing(award_information),
                                    missing(project_personnel)))
  
  if (length(missing_argument_index) > 0) {
    project_error <- required_arguments[missing_argument_index][1]
    project_error_message <- switch(project_error, 
                                    project_title = "Please provide the project title",
                                    award_infomration = "Please provide the award information.",
                                    project_personnel = "Please provide the project personnel")
    
    stop(project_error_message, call. = FALSE)
  }
  required_information <- c("funderName", "title", "individualName", "organization",
                            "role")
  
  null_required_information <- which(c(is.null(award_information$funderName), 
                                       is.null(award_information$title),
                                       (is.null(project_personnel$individualName) & 
                                          is.null(project_personnel$associatedParty$individualName) &
                                          is.null(project_personnel$creator$individualName)),
                                       (is.null(project_personnel$organization) & 
                                          is.null(project_personnel$associatedParty$organization) &
                                          is.null(project_personnel$creator$organization))))
  
  if (length(null_required_information) > 0) {
    missing_requirment_error <- required_information[null_required_information][1]
    requirment_error_message <- switch(missing_requirment_error, 
                                       funderName = "Please provide a list that includes the funderName.",
                                       title = "Please provide a list that includes the title for this award.",
                                       individualName = "Please provide a name for the project personnel.",
                                       organization = "Please provide an organization for the project personnel.")

    stop(requirment_error_message, call. = FALSE)
  }
  project_personnel$role <- "Project Lead"
  parent_element$project <- list(title = project_title,
                                 personnel = project_personnel,
                                 award = award_information)
  return(parent_element)
}
