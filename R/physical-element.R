#' @title Add Physical Element 
#' @description Adds the information of the physical format of the dataset based off of EML standards.
#' @param file_path The full name of the file of which your data is being read.
#' From this, the \code{object_name} and \code{object_size} will be calculated. 
#' @param number_of_headers Number of header lines preceeding the data. A default
#' of "1" is assigned if no input given as it is bad practice to provide anything 
#' greater than 1. 
#' @param number_of_footers Number of footer lines succeeding the data. 
#' @param record_delimiter Character used to delimit records. If no value is
#' inputted the default value of '\\r\\n' is assigned. 
#' @param attribute_orientation The orientation of the attributes. A default of 
#' "column" will be assigned if no input is given.  
#' @param field_delimiter Character used to delimit each field. The options that 
#' can be used are commas (","), space (" "), tab ("\t"), or colon (":"). 
#' @param authentication A value, typically a checksum, used to authenticate that 
#' the bitstream delivered to the user is identical to the original. This also 
#' descirbes authentication procedures or techniques. Can be used to describe
#' authentication procedures or techniques, typically by giving a checksum value
#' for the object.
#' @return A complete set of information on the physical format. 
#' @examples
#' add_physical(file_path = "/Users/lizzyshaw/FlowWest/cvpiaEDIutils/data-raw/Lowry et al 2013/all-database-records.csv", 
#'              authentication = "2317c6d849b8c5c8e80a39691d544fe2",
#'              number_of_headers = "1",
#'              number_of_footers = "1",
#'              record_delimiter = "\\r\\n", 
#'              attribute_orientation = "column", 
#'              field_delimiter = ",")
#' @export
add_physical <- function(file_path,
                         number_of_headers = "1", 
                         record_delimiter = "\\r\\n", 
                         attribute_orientation = "column", 
                         field_delimiter = c(",", " ", "\t", ":"),
                         authentication = NULL) {
 
  field_delimiter <- match.arg(field_delimiter)
  
  file_path_breaks <- unlist(strsplit(file_path, "/"))
  object_name <- paste(tail(file_path_breaks, n = 1))
  object_size <- paste(file.size(file_path), "bytes")
  
  required_arguments <- c("authentication", "number_of_headers",
                          "attribute_orientation", "field_delimiter")
  
  missing_argument_index <- which(c(missing(authentication),
                                    missing(number_of_headers),
                                    missing(attribute_orientation),
                                    missing(field_delimiter)))
  
  if (length(missing_argument_index) > 0) {
    physical_error <- required_arguments[missing_argument_index][1]
    physical_error_message <- switch(physical_error,
                                     number_of_headers = "Please provide the number of headers preceeding the data.",
                                     attribute_orientation = "Please provide the orientation of the attributes.",
                                     field_delimiter = "Please provide the character used to delimit the end of an attribute.",
                                     authentication = "Please provide the necessary inputs to authenticate that what is delivered to the user is identical to the original.")
    
    if (missing(authentication)) {
      warning(physical_error_message, call. = FALSE)
    } else {
      stop(physical_error_message, call. = FALSE)
    } 
  }
  
  if (record_delimiter == "\\r\\n") {
    warning("The default value of '\\r\\n' has been assigned. Please input a different value for record_delimiter if this is the wrong input.", call. = FALSE)
  }
  
  field_delimiter_characters <- length(unlist(strsplit(field_delimiter, " ")))
  
  if (field_delimiter_characters > 1) {
    stop('field_delimiter can only be one character long.', call. = FALSE)
  }
  
  physical <- list(objectName = object_name,
                   size = object_size,
                   dataFormat = list(textFormat = 
                                       list(numHeaderLines = number_of_headers,
                                            recordDelimiter = record_delimiter,
                                            attributeOrientation = attribute_orientation,
                                            simpleDelimited = list(fieldDelimiter = field_delimiter))))
  if (!is.null(authentication)) {
    physical$authentication <- authentication
  }
  
  return(physical)
}