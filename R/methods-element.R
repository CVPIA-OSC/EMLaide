#' @title Add Data Collection Method Element 
#' @description Adds the data collection method information of a dataset based off of EML standards. 
#' @param method_file Please provide a word document with proper formatting. The 
#' function will then append it properly. An example word document can be found in 
#' "~/cvpiaEDIutils/data-raw/template/methods-template.docx". This file can be opened
#' and edited to include your proper methods section. 
#' @param instrumentation Optional. What is being used to conduct the method. 
#' @return The methods information.
#' @examples 
#' add_method(method_file = "~/cvpiaEDIutils/data-raw/template/methods-template.docx",
#'            instrumentation = "Thermometer")
#'  
#' @export 
add_method <- function(methods_file, instrumentation = character(),
                       software = NULL,
                       sampling_file = NULL,
                       sampling_coverage = NULL,
                       sampling_citation = NULL,
                       qualityControl_file = NULL) {
  
  if (missing(methods_file)) {
    stop('Please provide the document of which your methods information resides.', call. = FALSE)
  }
  return(EML::set_methods(methods_file = methods_file,
                          instrumentation = instrumentation))
}

# add_method <- function(parent_element, description, title = NULL, 
#                        instrumentation = NULL) {
#   
#   required_arguments <- c("description", "title", "instrumentation")
#   missing_argument_index <- which(c(missing(description), missing(title), 
#                                     missing(instrumentation)))
#   
#   if (length(missing_argument_index) > 0) {
#     methods_error <- required_arguments[missing_argument_index][1]
#     methods_error_message <- switch(methods_error, 
#                                     description = "Please provide the description of the method you are recording.",
#                                     title = "No title inputed. Provide one for easier organization.",
#                                     instrumentation = "Provide the insrumentation device used if beneficial to understanding the dataset.")
#     if (missing(description)) {
#       stop(methods_error_message, call. = FALSE)
#     } 
#   }
#   
#   if (missing(title) | missing(instrumentation)) {
#     warning(methods_error_message, call. = FALSE)
#   }
#   
#   method <- list(methodStep = list(description = list(para = description)))
#   
#   if (!is.null(title)) {
#     method$methodStep$description$title <-  title
#   }
#   if (!is.null(instrumentation)) {
#     method$methodStep$instrumentation <- instrumentation
#   } 
# 
#   if (is.null(parent_element$methods)) {
#     parent_element$methods <- method
#   } else {
#     parent_element$methods <- list(parent_element$methods, method)
#   }
#   return(parent_element)
# }

