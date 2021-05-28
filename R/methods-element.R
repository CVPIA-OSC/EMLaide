#' @title Create Data Collection Method Element 
#' @description Creates the data collection method information of a dataset based off of EML standards. 
#' @param methods_file Please provide a word or markdown document with proper formatting. The 
#' function will then append it properly. An example word document can be found in 
#' "~/EMLaide/inst/extdata/methods-template.docx". 
#' @param instrumentation Optional. What instruments are being used to conduct the method. 
#' @param software Optional.
#' @param sampling_file Optional.
#' @param sampling_coverage Optional.
#' @param sampling_citation Optional.
#' @param qualityControl_file Optional.
#' @return A list containing the methods information that can be appended to a dataset or project list.
#' @examples 
#' create_method(methods_file = word_example("methods-template.docx"),
#'               instrumentation = "Thermometer")
#'  
#' @export 
create_method <- function(methods_file, 
                          instrumentation = character(),
                          software = NULL,
                          sampling_file = NULL,
                          sampling_coverage = NULL,
                          sampling_citation = NULL,
                          qualityControl_file = NULL) {
  
  if (missing(methods_file)) {
    stop('Please provide the document of which your methods information resides.', call. = FALSE)
  }
  
  methods <- EML::set_methods(methods_file = methods_file,
                              instrumentation = instrumentation,
                              software = software,
                              sampling_file = sampling_file,
                              sampling_coverage = sampling_coverage,
                              sampling_citation = sampling_citation,
                              qualityControl_file = qualityControl_file)
  return(methods)
}

#' Add Methods
#' @description Adds the methods metadata elements according to EML standards. 
#' @param parent_element A list representing the EML project or dataset.
#' @param methods_file A file containing methods information (word or markdown): see \code{\link{create_method}}
#' @return The dataset list or project with methods information appended. 
#' @examples 
#' methods_file = word_example("methods-template.docx")
#' 
#' dataset <- list() %>%
#'     add_method(methods_file)
#' dataset
#' @export
add_method <- function(parent_element, methods_file) {
  parent_element$methods <- create_method(methods_file)
  return(parent_element)
}