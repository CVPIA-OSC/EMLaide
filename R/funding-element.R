#' @title Adds Funding Element
#' @description 
#' @param 
#' @param 
#' @param 
#' @param 
#' @param 
#' @param 
#' @param 
#' @details 
#' @return 
#' @examples 
#' @export

add_funding <- function(parent_element, funder_name, funder_identifier,
                        award_number, award_title, award_url, funding_description)  {
  
  
  if (missing(license_url)) {stop("Please provide a url for the license.")}
  
  if (missing(funder_name)) {warning("Please provide Funders name.")}
  if (missing(funder_identifier)) {warning("Please provide Funder identifier link.")}
  if (missing(award_number)) {warning("Please provide your award number.")}
  if (missing(award_title)) {warning("Please provide the title of your project.")}
  if (missing(award_url)) {warning("Please provide the award url.")}
  if (missing(funding_description)) {warning("Please provide the description of the funding recieved.")}
  
  
  parent_element$funding$section <- list(para = funding_description)
  parent_element$award <- list(funderName = funder_name,
                               funderIdentifier = funder_identifier,
                               awardNumber = award_number, 
                               title = award_title,
                               awardUrl = award_url)
  
  
  
}
  
  
  
  
  
#<funding>
#   <section>
#     <para>BLE LTER is supported by the National Science Foundation under award #1656026 (2017-08-01 to 2022-07-31).</para>
#   </section>
# </funding>
# <award>
#   <funderName>National Science Foundation</funderName>
#   <funderIdentifier>http://dx.doi.org/10.13039/100000001</funderIdentifier>
#   <awardNumber>1656026</awardNumber>
#   <title>LTER: Beaufort Sea Lagoons: An Arctic Coastal Ecosystem in Transition</title>
#   <awardUrl>https://www.nsf.gov/awardsearch/showAward?AWD_ID=1656026</awardUrl>
# </award>