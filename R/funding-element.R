#' @title Add Funding Element
#' @description Creates the award information of a project based off of EML standards. 
#' This award element is then nested within a project node to complete a funding section. 
#' @param funder_name Organization or individual providing the funding.
#' @param funder_identifier This is where the funding organization is listed in 
#' the registry. The funder identifier must be registered. Follow the instructions at 
#' \url{https://ror.org/curation/} to get registered. 
#' @param award_number The identifier assigned by the funding agency to identify this funding award.
#' @param award_title Title of the dataset or project which recieved funding.
#' @param award_url Optional to include a link to information about the funding
#'  award on the funding organization's webpage.
#' @param funding_description Optional to provide a short description of the funding recieved.
#' @return An award list that is then added to the project element of an EML file.  
#' @examples 
#' add_funding(funder_name = "National Science Foundation",
#'             funder_identifier = "http://dx.doi.org/10.13039/100000001",
#'             award_number = "1656026",
#'             award_title = "LTER: Beaufort Sea Lagoons: An Arctic Coastal Ecosystem in Transition",
#'             award_url = "https://www.nsf.gov/awardsearch/showAward?AWD_ID=1656026",
#'             funding_description = "BLE LTER is supported by the National Science 
#'                                    Foundation under award #1656026 
#'                                    (2017-08-01 to 2022-07-31)." )
#' @export

add_funding <- function(funder_name, funder_identifier, award_number,
                        award_title, project_title, project_personnel, 
                        award_url = NULL, funding_description = NULL)  {
  
  award <- list()
  required_arguments <- c("funder_name", "funder_identifier", "award_number",
                          "award_title", "award_url", "funding_description")
  
  missing_argument_index <- which(c(missing(funder_name), missing(funder_identifier),
                              missing(award_number), missing(award_title), 
                              missing(award_url), missing(funding_description)))
  
  if (length(missing_argument_index) > 0) {
    fund_error <- required_arguments[missing_argument_index][1]
    fund_error_message <- switch(fund_error, funder_name = "Please provide funders name.",
                                 funder_identifier = "Please provide funder identifier link.",
                                 award_number = "Please provide your award number.", 
                                 award_title = "Please provide the title of your award.",
                                 award_url = "Please provide the award url.",
                                 funding_description = "Please provide the description of the funding recieved.")
    if (missing(funder_name) | missing(funder_identifier) |
        missing(award_number) | missing(award_title)) {
      stop(fund_error_message, call. = FALSE)
    } 
    
    if (missing(award_url) | missing(funding_description)) {
      warning(fund_error_message, call. = FALSE)
    }
  }
  award <- list(funderName = funder_name,
                funderIdentifier = funder_identifier,
                awardNumber = award_number, 
                title = award_title)
  
  if (!is.null(funding_description)) {
    award$description = funding_description

  }
  
  if (!is.null(award_url)) {
    award$awardUrl <- award_url
  }
  
  return(award)
}




