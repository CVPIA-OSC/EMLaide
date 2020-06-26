#' @title Add Title Elements
#' @param parament_element dataset or project 
#' @param title title of the project/dataset TODO add EDI recommendations
#' @param short_name short name of the project/dataset TODO add EDI recommendations
#' @export 
add_title <- function(parent_element, title, short_name) {
  parent_element$title <- title 
  parent_element$shortName <- short_name
  
  return(parent_element)
}
