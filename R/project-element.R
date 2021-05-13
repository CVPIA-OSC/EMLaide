#' @title Create project element 
#' @description This function creates a project element from metadata inputs using  \code{add_personnel}
#' and \code{add_funding}.  
#' @param title_metadata A list or datatable describing the project title, this should 
#' include a \code{project_title}
#' @param project_personnel_metadata A named list or datatable describing the personnel for the project. See structure by looking at required inputs of \code{\link{create_person}}
#' @param funding_metadata A named list or datatable describing the project funding. See structure by looking at required inputs of \code{\link{create_funding}}
#' @return This function returns project element containing 
#' all project information required for an EML document. 
#' 
#' @export                                                                          

create_project <- function(title_metadata, project_personnel_metadata, funding_metadata) {
  project_title <- title_metadata$short_name
  award_information <- create_funding(funder_name = funding_metadata$funder_name, 
                                      award_title = funding_metadata$award_title, 
                                      funder_identifier = funding_metadata$funder_identifier, 
                                      award_number = funding_metadata$award_number, 
                                      award_url = funding_metadata$award_url, 
                                      funding_description = funding_metadata$funding_description)
  
  creator <- dplyr::filter(project_personnel_metadata, role == 'creator')
  project_personnel <- create_person(role = "Project Lead",
                                     first_name = creator$first_name,
                                     last_name = creator$last_name,
                                     email = creator$email,
                                     organization = creator$organization,
                                     orcid = NULL)

  project <- list(title = project_title,
                  personnel = project_personnel,
                  award = award_information)
}
#' Add Project 
#' @param parent_element A list representing the EML project or dataset.
#' @param title_metadata see \code{\link{create_project}} 
#' @param project_personnel_metadata see \code{\link{create_project}}
#' @param funding_metadata see \code{\link{create_project}}
#' 
#' @example 
#' title_metadata <- list("title" = "Salmonid Habitat monitoring in the Central Valley", 
#'                         "short_name" = "Salmonid Monitoring Project")
#' project_personnel_metadata <- list(first_name = "Stacy", last_name = "Banet", email = "Stacy@aol.com", 
#'                                    role = "creator", organization = "USBR")
#' funding_metadata <- list(funder_name = "USBR", funder_identifier = NA, award_number = "R14AC00096", 
#'                          award_title = "Salmonid Spawning and Rearing Habitat Restoration in the Sacramento River", 
#'                          award_url = NA, funding_description = NA)
#' 
#' dataset <- list() %>%
#'    add_project(title_metadata, project_personnel_metadata, funding_metadata)
#' 
#' @export

add_project <- function(parent_element, title_metadata, project_personnel_metadata, funding_metadata) {
  parent_element$project <- create_project(title_metadata, project_personnel_metadata, funding_metadata)
  return(parent_element)
}