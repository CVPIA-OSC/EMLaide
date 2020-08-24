#' @title Add Data Collection Method Element 
#' @description Adds the data collection method information of a dataset based off of EML standards. 
#' @param parent_element A list representing the EML project or dataset. If being 
#' appended to a data table rather than the complete XML output, you can make the 
#' parent_element an empty list to be appended to the data table seperately. 
#' @param description How this method is being conducted. 
#' @param title Optional. What the particular method is accomplishing.
#' @param instrumentation Optional. What is being used to conduct the method. 
#' @return The dataset or project with methods information appended.
#' @examples 
#' add_method(parent_element = list(),
#'            title = "Climate Data",
#'            description = "Daily temperature (maximum/minimum) and
#'            precipitation data were obtained for each stand from 1996 to 2011
#'            from the online PRISM Gridded Climate database (PRISM Climate Group,
#'            Oregon State University, http://prism.oregonstate.edu, created 26 Mar 2015)
#'            by interpolating 4km2 resolution climate data at the centroid of each
#'            eastern hemlock stand using values from surrounding grid cell centers
#'            and inverse-distance squared weighting.", 
#'            instrumentation = "Thermometer")
#'                 
#' # If the method has a longer description than one paragraph:
#' description1 <- "This is the first paragraph" 
#' description2 <- "This is the second paragraph." 
#' description = list(description1, description2)
#' add_method(parent_element = list(),
#'            title = "The Data", 
#'            description = description,
#'            instrumentation = "The applicable instrument")
#' @export 
#' 
add_method <- function(parent_element, description, title = NULL, 
                       instrumentation = NULL) {
  
  required_arguments <- c("description", "title", "instrumentation")
  missing_argument_index <- which(c(missing(description), missing(title), 
                                    missing(instrumentation)))
  
  if (length(missing_argument_index) > 0) {
    methods_error <- required_arguments[missing_argument_index][1]
    methods_error_message <- switch(methods_error, 
                                    description = "Please provide the description of the method you are recording.",
                                    title = "No title inputed. Provide one for easier organization.",
                                    instrumentation = "Provide the insrumentation device used if beneficial to understanding the dataset.")
    if (missing(description)) {
      stop(methods_error_message, call. = FALSE)
    } 
  }
  
  if (missing(title) | missing(instrumentation)) {
    warning(methods_error_message, call. = FALSE)
  }
  
  Method <- list(methodStep = list(description = list(para = description)))
  
  if (!is.null(title)) {
    Method$methodStep$description$title <-  title
  }
  if (!is.null(instrumentation)) {
    Method$methodStep$instrumentation <- instrumentation
  } 

  if (is.null(parent_element$methods)) {
    parent_element$methods <- Method
  } else {
    parent_element$methods <- list(parent_element$methods, Method)
  }
  return(parent_element)
}

