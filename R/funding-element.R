#' @title Add Funding Element
#' @description Adds the funding information of a dataset based off of EML standards 
#' @param parent_element A list representing the EML project or dataset
#' @param funder_name Organization or individual providing the funding 
#' @param funder_identifier This is where the funding organization is listed in the registry. The funder identifier must be registered. Follow the instructions at 
#' \url{https://ror.org/curation/} to get registered. 
#' @param award_number The identifier assigned by the funding agency to identify this funding award.
#' @param award_title Title of the dataset or project which recieved funding
#' @param award_url Optional to include a link to information about the funding award on the funding organization's webpage.
#' @param funding_description Optional to provide a short description of the funding recieved.
#' @return The dataset or project with funding information appended 
#' @examples 
#' add_funding(parent_element = list(), funder_name = "National Science Foundation",
#'             funder_identifier = "http://dx.doi.org/10.13039/100000001",
#'             award_number = "1656026",
#'             award_title = "LTER: Beaufort Sea Lagoons: An Arctic Coastal Ecosystem in Transition",
#'             award_url = "https://www.nsf.gov/awardsearch/showAward?AWD_ID=1656026",
#'             funding_description = "BLE LTER is supported by the National Science Foundation under award #1656026 (2017-08-01 to 2022-07-31)." )
#' @export

add_funding <- function(parent_element, funder_name, funder_identifier,
                        award_number, award_title, award_url = NULL, funding_description = NULL)  {

  if (missing(funder_name)) {stop("Please provide funders name.", call. = FALSE)}
  if (missing(funder_identifier)) {stop("Please provide funder identifier link.", call. = FALSE)}
  if (missing(award_number)) {stop("Please provide your award number.", call. = FALSE)}
  if (missing(award_title)) {stop("Please provide the title of your project.", call. = FALSE)}
  if (missing(award_url)) {warning("Please provide the award url.", call. = FALSE)}
  if (missing(funding_description)) {warning("Please provide the description of the funding recieved.", call. = FALSE)}
  
  
  parent_element$funding <- list(section = list(para = funding_description))
  parent_element$award <- list(funderName = funder_name,
                               funderIdentifier = funder_identifier,
                               awardNumber = award_number, 
                               title = award_title,
                               awardUrl = award_url)

  return(parent_element)
}
  
  
  
  
