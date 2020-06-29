#' @title Add Title Elements
#' @param parament_element dataset or project 
#' @param title The title of the project and/or dataset. A complete title is between 
#' 7 and 20 words long and includes: What, Where, and When (and Who, if relevant).
#' @param short_name Short name or nickname you use to refer to this dataset
#' @export 
add_title <- function(parent_element, title, short_name) {
  
  title_number_of_words <- length(unlist(strsplit(title, " ")))
  if (title_number_of_words < 7 | title_number_of_words > 20) {
    stop("Please make sure your title is between 7 and 20 words long.")
  }
    
  parent_element$title <- title 
  parent_element$shortName <- short_name
  
  return(parent_element)
}
