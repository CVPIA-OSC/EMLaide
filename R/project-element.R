

#' @title Add project element 
#' @description This function creates a project node within the parent element that 
#' contains all the project information. This function can be used in combination with add_personnel
#' and add_funding. Add_personnel can be used to generate the project_personnel 
#' input and add_funding can be used to generate the award_information input.  
#' @param parent_element A list in which the project node should be nested in. 
#' @param project_title The title of the project that the funding is awarded to. 
#' @param award_information A list that includes the award title and the funder name 
#' for the award.This list can be created by calling the add_funding function 
#' on the funding information. It can also be created manually. 
#' @param project_personnel A list that includes the project personnel. It must include 
#' the first name, last name, organization, and position for this project. This list can be created 
#' by calling the add_personnel function on the project personnel.It can also be created manually. 
#' @return This function returns the parent element with a new project node containing 
#' all project information. 
#' @examples
#' add_project(parent_element = parent_element, 
#'             project_title = "my project title", 
#'             award_information = add_funding(funder_name = "Bank",
#'                                             funder_identifer = "Funder 1",
#'                                             award_number = "000",
#'                                             award_title = "Money up for grabs",
#'                                             award_url = "awardforme.com"),
#'             project_personnel = add_personnel(parent_element = parent_element,
#'                                               first_name = "Smithy",
#'                                               last_name = "Smith",
#'                                               email = "myemail@mail.gov",
#'                                               role = "Manager",
#'                                               organization = "US GOV"))
#'                                               
#' add_project(parent_element = parent_element, 
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
    
    if (missing(project_title) | missing(award_information) |
        missing(project_personnel)) {
      stop(project_error_message, call. = FALSE)
    }
  }
  required_information <- c("funderName", "title", "individualName", "organization",
                            "role")
  
  null_required_information <- which(c(is.null(award_information$funderName), 
                                       is.null(award_information$title),
                                       (is.null(project_personnel$individualName)&
                                          is.null(project_personnel$associatedParty$individualName)),
                                       (is.null(project_personnel$organization)&
                                          is.null(project_personnel$associatedParty$organization)),
                                       (is.null(project_personnel$role)&
                                          is.null(project_personnel$associatedParty$role))))
  
  if (length(null_required_information) > 0) {
    missing_requirment_error <- required_information[null_required_information][1]
    requirment_error_message <- switch(missing_requirment_error, 
                                       funderName = "Please provide a list that includes the funderName.",
                                       title = "Please provide a list that includes the title for this award.",
                                       individualName = "Please provide a name for the project personnel.",
                                       organization = "Please provide an organization for the project personnel.",
                                       role = "Please provide a role for the project personnel.")
    
    if (is.null(award_information$funderName)| is.null(award_information$title) |
        (is.null(project_personnel$individualName) & 
         is.null(project_personnel$associatedParty$individualName) &
         is.null(project_personnel$creator$individualName))|
        (is.null(project_personnel$organization) & 
         is.null(project_personnel$associatedParty$organization) &
         is.null(project_personnel$creator$organization)) |
        (is.null(project_personnel$role) & 
         is.null(project_personnel$associatedParty$role) &
         is.null(project_personnel$creator$role))) {
      stop(requirment_error_message, call. = FALSE)
    }
  }

  parent_element$project <- list(title = project_title,
                                 personnel = project_personnel,
                                 award = award_information)
  return(parent_element)
}


