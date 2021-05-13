#' @title Create project element 
#' @description This function creates a project element from metadata inputs using  \code{add_personnel}
#' and \code{add_funding}.  
#' @param project_title A project title as a string
#' @param project_lead A named list or datatable describing the project lead for the project. See structure by looking at required inputs of \code{\link{create_person}}
#' @param funding_metadata A named list or datatable describing the project funding. See structure by looking at required inputs of \code{\link{create_funding}}
#' @return This function returns project element containing 
#' all project information required for an EML document. 
#' 
#' @export                                                                          

create_project <- function(project_title, project_lead, funding_metadata) {
  
  award_information <- create_funding(funder_name = funding_metadata$funder_name, 
                                      award_title = funding_metadata$award_title, 
                                      funder_identifier = funding_metadata$funder_identifier, 
                                      award_number = funding_metadata$award_number, 
                                      award_url = funding_metadata$award_url, 
                                      funding_description = funding_metadata$funding_description)
  
  project_personnel <- create_person(role = "Project Lead",
                                     first_name = project_lead$first_name,
                                     last_name = project_lead$last_name,
                                     email = project_lead$email,
                                     organization = project_lead$organization,
                                     orcid = NULL)

  project <- list(title = project_title,
                  personnel = project_personnel,
                  award = award_information)
}
#' Add Project 
#' @param parent_element A list representing the EML project or dataset.
#' @param funding_metadata see \code{\link{create_project}}
#' @param project_title see \code{\link{create_project}} 
#' @param project_lead see \code{\link{create_project}}
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

add_project <- function(parent_element, funding_metadata, project_title = NULL, project_lead = NULL) {
  if(is.null(project_lead)) {
    if (is.null(parent_element$creator)) {
      stop('please supply information about the project lead or run add_personnel first and the dataset creator will be used')
    }
    project_lead <- parent_element$creator
  }
  if(is.null(project_title)) {
    if (is.null(parent_element$title)) {
      stop('please supply a project lead or run add_title first and the dataset title will be used')
    }
    project_title <- parent_element$title
  }
  
  parent_element$project <- create_project(project_title, project_lead, funding_metadata)
  return(parent_element)
}