#' @title Create project element 
#' @description This function creates a project element from metadata inputs using  \code{create_person}
#' and \code{create_funding}.  
#' @param project_title A project title as a string
#' @param project_lead A named list or datatable describing the project lead for the project. See structure by looking at required inputs of \code{\link{create_person}}
#' @param funding_metadata A named list or datatable describing the project funding. See structure by looking at required inputs of \code{\link{create_funding}}
#' @return This function returns project element containing 
#' all project information required for an EML document. 
#' @examples 
#' project_title <- "Salmonid Habitat Monitoring"
#' project_lead <- list(first_name = "Stacy", last_name = "Banet", email = "Stacy@aol.com", 
#'                                    role = "creator", organization = "USBR")
#' funding_metadata <- list(funder_name = "USBR", funder_identifier = NA, award_number = "R14AC00096", 
#'                          award_title = "Salmonid Spawning and Rearing Habitat Restoration in the Sacramento River", 
#'                          award_url = NA, funding_description = NA)
#' 
#' create_project(project_title, project_lead, funding_metadata)
#' @export                                                                          

create_project <- function(project_title, project_lead, funding_metadata) {
  
  award_information <- purrr::pmap(funding_metadata, create_funding)
  
  project_personnel <- create_person(role = "Project Lead",
                                     first_name = ifelse(is.null(project_lead$first_name), project_lead$individualName$givenName, project_lead$first_name),
                                     last_name = ifelse(is.null(project_lead$last_name), project_lead$individualName$surName, project_lead$last_name),
                                     email = project_lead$email,
                                     organization = project_lead$organization,
                                     orcid = NULL)

  project <- list(title = project_title,
                  personnel = project_personnel,
                  award = award_information)
  return(project)
}
#' Add Project 
#' @description Adds the project metadata elements according to EML standards.  
#' @param parent_element A list representing the EML project or dataset.
#' @param funding_metadata Add a named list or dataframe containing funding metadata: see \code{\link{create_project}}
#' @param project_title Optionally add a project_title that is different from dataset title: see \code{\link{create_project}} 
#' @param project_lead Optionally add metadata describing a specific project personnel: see \code{\link{create_project}}
#' @return The dataset list with project information appended.
#' 
#' @examples 
#' project_lead <- dplyr::tibble(first_name = "Stacy", last_name = "Banet", email = "Stacy@aol.com", 
#'                                    role = "creator", organization = "USBR")
#' funding_metadata <- list(funder_name = "USBR", funder_identifier = NA, award_number = "R14AC00096", 
#'                          award_title = "Salmonid Spawning and Rearing Habitat Restoration in the Sacramento River", 
#'                          award_url = NA, funding_description = NA)
#' 
#' dataset <- list() %>%
#'    add_title(list(title = "O.mykiss Habitat monitoring and contact point project", short_name = "O.mykiss monitoring")) %>%
#'    add_personnel(project_lead) %>%
#'    add_project(funding_metadata)
#' dataset
#'    
#' dataset <- list() %>%
#'    add_project(funding_metadata, project_title = "Salmonid Habitat monitoring in the Central Valley", project_lead = project_lead)   
#' dataset
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
      stop('please supply a project title or run add_title first and the dataset title will be used')
    }
    project_title <- parent_element$title
  }
  
  parent_element$project <- create_project(project_title, project_lead, funding_metadata)
  return(parent_element)
}