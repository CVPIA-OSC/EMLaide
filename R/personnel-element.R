#' Add Personnel Element
#' @description Adds personel according to EML standards
#' @param role Use "creator" if you are one of the primary originators of the data. 
#' Other possible roles "Data Manager", "Field Technician", or "Assistant Researcher". 
#' There can be multiple personnel on a project with the same role.
#' @param first_name Person's given name
#' @param last_name Person's surname
#' @param email Person's email address
#' @param organization Person's employer or the entity they are associated
#' with for this dataset or project
#' @param orcid (Optional) ORCID iD is a persistent digital identifier for researchers,
#' register at \url{http://orcid.org/}
#' @examples 
#' add_personnel(parent_element = list(),
#'               first_name = 'Katherine', 
#'               last_name = "Johnson", 
#'               email = 'kjohnson@nasa.gov', 
#'               role = 'creator', 
#'               organization = 'NASA',
#'               orcid = '12345')
#' 
#' add_personnel(parent_element = list(), 
#'               first_name = "Edith", 
#'               last_name = "Windsor", 
#'               email = 'ewindsor@ibm.com', 
#'               role = 'Data Manager', 
#'               organization = 'IBM')
#' @export
create_person <- function(role, first_name, last_name, email, organization, orcid = NULL) {
  
  required_arguments <- c("first_name", "last_name", "email", "role", "organization")
  missing_argument_index <- which(c(missing(first_name), missing(last_name), 
                                missing(email), missing(role), 
                                missing(organization)))
  
  if (length(missing_argument_index) > 0) {
    person_error <- required_arguments[missing_argument_index][1]
    person_error_message <- switch(person_error,
                                   first_name = "Please supply a first name.",
                                   last_name = "Please supply a last name.",
                                   email = "Please supply an email.",
                                   role = "Please supply a role. Use 'creator' if you are the main originator of the dataset or project",
                                   organization = "Please supply the name of the organization employing the personnel")
    stop(person_error_message, call. = FALSE)
  } 
  
  person <- list(individualName = list(givenName = first_name, 
                                       surName = last_name),
                 electronicMailAddress = email, 
                 organizationName = organization)
  
  if(!is.null(orcid)) {
    person$'@id' <- orcid 
  }
  
  if(role != 'creator') {
    person$role <- role
  }
  
  return(person)
  
}

#' Add personnel
#' @param parent_element A list representing the EML project or dataset
#' @param personnel_metadata A list or dataframe of personnel information see \code{\link{create_person}}
#' @export
add_personnel <- function(parent_element, personnel_metadata) {
  
  for (row in seq_along(personnel_metadata)) {
    if (personnel_metadata[row, 'role'] == 'creator') {
      parent_element$creator <- create_person(personnel_metadata[row, ])
      parent_element$contact <- create_person(personnel_metadata[row, ])
    }
    else {
      parent_element$associatedParty <- list(parent_element$associatedParty, create_person(personnel_metadata[row, ]))
    }
  }
  return(parent_element)
}
