#' @title Add Publication Date 
#' @description Adds publication date according to EML standards.
#' @param parent_element A list representing the EML project or dataset.
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

create_pub_date <- function(date = NULL) {
  if (is.null(date)) {
    date <- Sys.Date()
  } else {
    date
  }
  return(date)
}
#' Add Abstract
#' @param parent_element A list representing the EML project or dataset.
#' @param date The publication date. If left null, it will be assigned the current 
#' date. This can be overriden by putting in a specific date: see \code{\link{create_pub_date}} 
#' @example 
#' dataset <- list() %>%
#'     add_pub_date()
#' 
#' @export
add_pub_date <- function(parent_element, date = NULL) {
  if (is.null(date)){
    parent_element$pubDate <- create_pub_date()
  }
  else {
    parent_element$pubDate <- create_pub_date(date)
  }
  return(parent_element)
}