#' @title Add Data Collection Method Element 
#' @description Adds the data collection method information of a dataset based off of EML standards. 
#' @param method_file Please provide a word document with proper formatting. The 
#' function will then append it properly. An example word document can be found in 
#' "~/cvpiaEDIutils/inst/extdata/methods-template.docx". This file can be opened
#' and edited to include your proper methods section. 
#' @param instrumentation Optional. What is being used to conduct the method. 
#' @return The methods information.
#' @examples 
#' add_method(parent_element = list(), 
#'            methods_file = word_example("methods-template.docx"),
#'            instrumentation = "Thermometer")
#'  
#' @export 
add_method <- function(parent_element, 
                       methods_file, 
                       instrumentation = character(),
                       software = NULL,
                       sampling_file = NULL,
                       sampling_coverage = NULL,
                       sampling_citation = NULL,
                       qualityControl_file = NULL) {
  
  if (missing(methods_file)) {
    stop('Please provide the document of which your methods information resides.', call. = FALSE)
  }
  
  parent_element <- EML::set_methods(methods_file = methods_file,
                                             instrumentation = instrumentation)
  return(parent_element)
}