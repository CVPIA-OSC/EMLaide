#' @title Add Title Elements
#' @param parament_element dataset or project 
#' @param title title of the project/dataset TODO add EDI recommendations
#' @param short_name short name of the project/dataset TODO add EDI recommendations
#' @export 
add_title <- function(parent_element, title, short_name) {
  dataset$title <- title 
  dataset$shortName <- short_name
  
  return(dataset)
}
