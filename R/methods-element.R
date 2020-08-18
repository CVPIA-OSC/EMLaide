#' @title Add Data Collection Method Element 
#' @description Adds the data collection method information of a dataset based off of EML standards. 
#' @param description How this method is being conducted. 
#' @param title Optional. What the particular method is accomplishing.
#' @param instrumentation Optional. What is being used to conduct the method. 
#' @return The dataset or project with methods information appended.
#' @examples 
#' add_method(title = "Climate Data",
#'            description = "Daily temperature (maximum/minimum) and
#'            precipitation data were obtained for each stand from 1996 to 2011
#'            from the online PRISM Gridded Climate database (PRISM Climate Group,
#'            Oregon State University, http://prism.oregonstate.edu, created 26 Mar 2015)
#'            by interpolating 4km2 resolution climate data at the centroid of each
#'            eastern hemlock stand using values from surrounding grid cell centers
#'            and inverse-distance squared weighting.", 
#'            instrumentation = "Thermometer")
#'                 
#' #If the method has a longer description than one paragraph:
#' description1 <- "This is the first paragraph" 
#' description2 <- "This is the second paragraph." 
#' description = list(description1, description2)
#' add_method(title = "The Data", 
#'            description = description,
#'            instrumentation = "The applicable instrument")
#' @export 
#' 
add_method <- function(description, title = NULL, instrumentation = NULL) {
  if (missing(description)) {stop('Please provide the description of the method you are recording.', call. = FALSE)}
  if (missing(title)) {warning('No title inputed. Provide one for easier organization.', call. = FALSE)}
  if (missing(instrumentation))
  {warning('Provide the insrumentation device used if beneficial to understanding the dataset.', call. = FALSE)}
  
  methods <- list(methodStep = list(description = list(seciton = list(title = title, 
                                                       para = description)),
                                    instrumentation = instrumentation))
  
  return(methods)
}