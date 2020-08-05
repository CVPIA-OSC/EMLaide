#' @title Add Physical Element 
#' @description Adds the information of the physical format of the dataset based off of EML standards.
#' @param object_name The name of the data object.
#' @param object_size The physical size of the data object. 
#' @param authentication A value, typically a checksum, used to authenticate that 
#' the bitstream delivered to the user is identical to the original. This also 
#' descirbes authentication procedures or techniques. 
#' @param number_of_headers Number of header lines preceeding the data.
#' @param number_of_footers Number of footer lines succeeding the data. 
#' @param record_delimiter Character used to delimit records. If no value is
#' inputted the default value of '\ r\ n' is assigned. 
#' @param attribute_orientation The orientation of the attributes. 
#' @param field_delimiter Character used to delimit the end of an attribute. 
#' @return A complete set of information on the physical format. 
#' @examples
#' add_physical(object_name = "hf289-02-stand-year.csv",
#'              object_size = "36516 bytes", 
#'              authentication = "2317c6d849b8c5c8e80a39691d544fe2",
#'              number_of_headers = "1",
#'              number_of_footers = "1",
#'              record_delimiter = "\ r\ n", 
#'              attribute_orientation = "column", 
#'              field_delimiter = ",")
#' @export
add_physical <- function(object_name, object_size, authentication,
                         number_of_headers, number_of_footers, 
                         record_delimiter = "\r\n", attribute_orientation, 
                         field_delimiter) {
  
  required_arguments <- c("object_name", "object_size", "authentication", 
                          "number_of_headers", "number_of_footers",
                          "attribute_orientation", "field_delimiter")
  
  missing_argument_index <- which(c(missing(object_name), missing(object_size),
                                   missing(authentication), missing(number_of_headers),
                                   missing(number_of_footers), missing(attribute_orientation), 
                                   missing(field_delimiter)))
  
  if (length(missing_argument_index) > 0) {
    physical_error <- required_arguments[missing_argument_index][1]
    physical_error_message <- switch(physical_error,
                               object_name = "Please provide the name of the data object.",
                               object_size = "Please provide the physical size of the data object.",
                               authentication = "Please provide the necessary inputs to authenticate that what is delivered to the user is identical to the original.",
                               number_of_headers = "Please provide the number of headers preceeding the data.",
                               number_of_footers = "Please provide the number of footers concluding the data.",
                               attribute_orientation = "Please provide the orientation of the attributes.",
                               field_delimiter = "Please provide the character used to delimit the end of an attribute.")
    stop(physical_error_message, call. = FALSE)
  } 

  if (!is.null(record_delimiter)) {
    warning("The default value of '\ r\ n' has been assigned. Please input a different value for record_delimiter if this is the wrong input.", call. = FALSE)
  }
  
  field_delimiter_characters <- length(unlist(strsplit(field_delimiter, " ")))
  
  if (field_delimiter_characters > 1) {
    stop('field_delimiter can only be one character long.', call. = FALSE)
  }
  
  physical <- list(objectName = object_name,
                   size = object_size,
                   authentication = authentication,
                   dataFormat = list(textFormat = 
                                       list(numHeaderLines = number_of_headers,
                                            recordDelimiter = record_delimiter,
                                            attributeOrientation = attribute_orientation,
                                            simpleDelimited = list(fieldDelimiter = field_delimiter))))
  return(physical)
  
}