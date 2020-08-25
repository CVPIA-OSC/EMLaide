#' @title Add Publication Date 
#' @description Adds publication date according to EML standards.
#' @param parent_element
#' @param date The publication date. If left null, it will be assigned the current 
#' date. This can be overriden by putting in a specific date. 
#' @return The dataset or project with the publication date appended. 
#' @examples 
#' add_pub_date(parent_element = list(),
#'              date = "2020-08-19")
#'              
#' # Appends current date:              
#' add_pub_date(parent_element = list()) 
#' @export 

add_pub_date <- function(parent_element, date = NULL) {
  if (is.null(date)) {
    parent_element$pubDate <- Sys.Date()
  } else {
    parent_element$pubDate <- date
  }
  return(parent_element)
}