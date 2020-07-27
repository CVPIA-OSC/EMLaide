#' @title Add Method Step Element 
#' @description Adds the method step information of a dataset based off of EML standards. 
#' @param method_step_title Optional. What the particular method is accomplishing. 
#' @param method_step_description How this method is being conducted. 
#' @param instrumentation Optional. What is being used to conduct the method. 
#' @reutn The dataset or project with methods information appended.
#' @examples 
#' add_method_step(method_step_title = "Climate Data",
#'                 method_step_description = "Daily temperature (maximum/minimum) and
#'                 precipitation data were obtained for each stand from 1996 to 2011
#'                 from the online PRISM Gridded Climate database (PRISM Climate Group,
#'                 Oregon State University, http://prism.oregonstate.edu, created 26 Mar 2015)
#'                 by interpolating 4km2 resolution climate data at the centroid of each
#'                 eastern hemlock stand using values from surrounding grid cell centers
#'                 and inverse-distance squared weighting.", instrumentation = "Thermometer")
#'                 
#' If the method has a longer description than one paragraph:
#' description1 <- "This is the first paragraph" 
#' description2 <- "This is the second paragraph." 
#' description = list(description1 description2)
#' add_method_step(method_step_title = "The Data", method_step_description = description,
#'                 instrumentation = "The applicable instrument")
#' @export 
#' 
add_method_step <- function(method_step_title = NULL, method_step_description, instrumentation = NULL) {
  if (missing(method_step_title)) {warning('No title inputed. Provide one for easier organization.', call. = FALSE)}
  if (missing(method_step_description)) {stop('Please provide the description of the method you are recording.', call. = FALSE)}
  if (missing(instrumentation))
  {warning('Provide the insrumentation device used if beneficial to understanding the dataset.', call. = FALSE)}
  
  methods <- list(methodStep = list(description = list(seciton = list(title = method_step_title, 
                                                       para = method_step_description)),
                                    instrumentation = instrumentation))
  
  return(methods)
}