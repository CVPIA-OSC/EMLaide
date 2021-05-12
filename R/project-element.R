#' @title Create project element 
#' @description This function creates a project element from metadata inputs using  \code{add_personnel}
#' and \code{add_funding}.  
#' @param project_metadata A list or datatable describing the project, this should 
#' include a \code{project_title} and information on a \code{project_personnel} if project personnel differs from other personnel, see See structure of personnel information 
#' by looking at required inputs of \code{\link{create_person}}
#' @param personnel_metadata A named list or datatable describing the personnel. See structure by looking at required inputs of \code{\link{create_person}}
#' @param funding_metadata A named list or datatable describing the project funding. See structure by looking at required inputs of \code{\link{create_funding}}
#' @return This function returns project element containing 
#' all project information required for an EML document. 
#' 
#' @export                                                                          

create_project <- function(project_metadata, personnel_metadata, funding_metadata) {
  project_title <- project_metadata$project_title
  award_information <- create_funding(funder_name = funding_metadata$funder_name, 
                                      award_title = funding_metadata$award_title, 
                                      funder_identifier = funding_metadata$funder_identifier, 
                                      award_number = funding_metadata$award_number, 
                                      award_url = funding_metadata$award_url, 
                                      funding_description = funding_metadata$funding_description)
  
  if (!is.na(project_metadata$first_name)) {
    creator <- dplyr::filter(personnel_metadata, role == 'creator')
    project_personnel <- create_person(role = "Project Lead",
                                       first_name = creator$first_name,
                                       last_name = creator$last_name,
                                       email = creator$email,
                                       organization = creator$organization,
                                       orcid = NULL)
  }
  else {
    project_personnel <- create_person(role = project_metadata$role,
                                       first_name = project_metadata$first_name,
                                       last_name = project_metadata$last_name,
                                       email = project_metadata$email,
                                       organization = project_metadata$organization,
                                       orcid = NULL)
  }
  project <- list(title = project_title,
                  personnel = project_personnel,
                  award = award_information)
}
#' Add Project 
#' @param parent_element A list representing the EML project or dataset.
#' @param project_metadata see \code{\link{create_project}} 
#' @param personnel_metadata see \code{\link{create_project}}
#' @param funding_metadata see \code{\link{create_project}}
#' 
#' @export

add_project <- function(parent_element, project_metadata, personnel_metadata, funding_metadata) {
  parent_element$project <- create_project(project_metadata, personnel_metadata, funding_metadata)
  return(parent_element)
}