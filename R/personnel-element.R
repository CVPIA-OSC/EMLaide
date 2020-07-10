#' Add Personnel Element
#' @description adds personel according to EML standards
#' @param parent_element a list representing the EML project or dataset
#' @param role Use "Creator" if you are one of the primary originators of the data. 
#' Other possible roles "Data Manager", "Field Technician", or "Assistant Researcher". 
#' There can be multiple personnel on a project with the same role.
#' @param first_name Person's given name
#' @param last_name Person's surname
#' @param email Person's email address
#' @param orcid ORCID iD is a persistent digital identifier for researchers, register at \url{http://orcid.org/}
#' @param organization Person's employer or the entity they are associated with for this dataset or project
#' @examples 
#' add_personnel(parent_element = list(), first_name = 'Katherine', 
#'               last_name = "Johnson", email = 'kjohnson@nasa.gov', role = 'Creator', organization = 'NASA')
#' 
#' add_personnel(parent_element = list(), first_name = "Edith", last_name = "Windsor", 
#'               email = 'ewindsor@ibm.com', role = 'Data Manager', organization = 'IBM')
#' @export
add_personnel <- function(parent_element, first_name, last_name, email, 
                          role, orcid = NULL, organization = NULL) {
  
  if (missing(first_name)) {stop("Please supply a first name.")}
  if (missing(last_name)) {stop("Please supply a last name.")}
  if (missing(email)) {stop("Please supply a email.")}
  if (missing(role)) {stop("Please supply a role. Use 'Creator' if you are the main originator of the dataset or project")}
  
  person <- list(individualName = 
                   list(givenName = first_name, 
                        surName = last_name),
                 electronicMailAddress = email)
  
  if (!is.null(orcid)) {
    person$userid = list(
      directory = 'https://orcid.org',
      paste0("https://orcid.org/", orcid))
  }
  
  if (!is.null(organization)) {
    person$organizationName = organization
  }
  
  if (role == "Creator") {
    if (is.null(parent_element$creator)) {
      parent_element$creator <- person
    } else {
      parent_element$creator <- list(parent_element$creator, person)
    }
  } else {
    person$role <- role
    if (is.null(parent_element$associatedParty)) {
      parent_element$associatedParty <- person
    } else {
      parent_element$associatedParty <- list(parent_element$associatedParty, person)
    }
  }
  
  return(parent_element)
  
}
