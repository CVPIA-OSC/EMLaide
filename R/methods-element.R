#' @title Add Data Collection Method Element 
#' @description Adds the data collection method information of a dataset based off of EML standards. 
#' @param parent_element Please provide a parent element to append methods to. 
#' @param methods_file Please provide a word document with proper formatting. The 
#' function will then append it properly. An example word document can be found in 
#' "~/EMLaide/inst/extdata/methods-template.docx". 
#' @param instrumentation Optional. What is being used to conduct the method. 
#' @param software Optional.
#' @param sampling_file Optional.
#' @param sampling_coverage Optional.
#' @param sampling_citation Optional.
#' @param qualityControl_file Optional.
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
  
  parent_element$methods <- EML::set_methods(methods_file = methods_file,
                                             instrumentation = instrumentation,
                                             software = software,
                                             sampling_file = sampling_file,
                                             sampling_coverage = sampling_coverage,
                                             sampling_citation = sampling_citation,
                                             qualityControl_file = qualityControl_file)
  return(parent_element)
}