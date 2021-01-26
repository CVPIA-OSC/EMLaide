#' @title Add Title Elements
#' @description Adds title elements according to EML standards.
#' @param parent_element A list representing the EML project or dataset.
#' @param title The title of the project and/or dataset. A complete title is between 
#' 7 and 20 words long and includes: What, Where, and When (and Who, if relevant).
#' @param short_name Short name or nickname you use to refer to this dataset. 
#' @details
#'For a dataset, the title should be between 7 and 20 words long, with a short
#'name which is shorter than the title. If the title is too short, ensure that it
#'is covering the 'what', 'where', and 'when'. If the title is too long, attempt 
#'to make it as concise as possible.
#' @return The project or dataset list with title and short name appended. 
#' @examples 
#' add_title(parent_element = list(),
#'           title = "Eight Mile Lake Research Watershed, Carbon in Permafrost Experimental Heating Research.",
#'           short_name = "knb-lter-bnz.501.17") 
#' @export 
add_title <- function(parent_element, title, short_name) {
  
  title_number_of_words <- length(unlist(strsplit(title, " ")))
  short_name_number_of_words <- length(unlist(strsplit(short_name, " "))) 
  
  if (title_number_of_words < 7 | title_number_of_words > 20) {
    stop("Please make sure your title is between 7 and 20 words long.")
  }
  
  if (short_name_number_of_words >= title_number_of_words) {
    stop("Short name should not be longer than the dataset's title.")
  }
    
  parent_element$title <- title 
  parent_element$shortName <- short_name
  
  return(parent_element)
}
