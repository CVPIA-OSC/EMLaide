#' @title Add Physical Element 
#' @description Adds the information of the physical format of the dataset based off of EML standards.
#' @param file_path The full name of the file of which your data is being read.
#' From this, the \code{object_name}, \code{object_size}, and \code{authentication}
#' will be extracted.  
#' @param number_of_headers Number of header lines preceeding the data. A default
#' of "1" is assigned if no input is given. Please refrain from inputting any other value
#' if possible as it is bad practice to do so. 
#' @param record_delimiter Character used to delimit records. If no value is
#' inputted the default value of '\\r\\n' is assigned. 
#' @param attribute_orientation The orientation of the attributes. A default of 
#' "column" will be assigned if no input is given.  
#' @param field_delimiter Character used to delimit each field. The options 
#' provided are commas (","), space (" "), tab ("\\t"), or colon (":"). The 
#' default is comma, as csv's are most common. 
#' @param data_url Optional. A url, if possible, of which the data file can be 
#' downloaded. 
#' @return A complete set of information on the physical format. 
#' @examples
#' add_physical(file_path = "/Users/lizzyshaw/FlowWest/cvpiaEDIutils/data-raw/Lowry et al 2013/all-database-records.csv", 
#'              number_of_headers = "1",
#'              record_delimiter = "\\r\\n", 
#'              attribute_orientation = "column", 
#'              field_delimiter = ",",
#'              data_url = "https://mydata.org/etc")
#'              
#' add_physical(file_path = "/Users/lizzyshaw/FlowWest/cvpiaEDIutils/data-raw/Lowry et al 2013/all-database-records.csv",
#'              data_url = "https://mydata.org/etc")
#' @export
add_physical <- function(file_path,
                         number_of_headers = "1", 
                         record_delimiter = "\\r\\n", 
                         attribute_orientation = "column", 
                         field_delimiter = c(",", " ", "\\t", ":"),
                         data_url = NULL) {
 
  field_delimiter <- match.arg(field_delimiter)
  
  file_path_breaks <- unlist(strsplit(file_path, "/"))
  object_name <- paste(tail(file_path_breaks, n = 1))
  object_size <- paste(file.size(file_path))
  authentication <- paste(tools::md5sum(file_path))
  
  #NOT SURE IF THIS IS NECESSARY bc of default values. 
  # required_arguments <- c("number_of_headers", "attribute_orientation", 
  #                         "field_delimiter", "data_url")
  # 
  # missing_argument_index <- which(c(missing(number_of_headers),
  #                                   missing(attribute_orientation),
  #                                   missing(field_delimiter),
  #                                   missing(data_url)))
  # 
  # if (length(missing_argument_index) > 0) {
  #   physical_error <- required_arguments[missing_argument_index][1]
  #   physical_error_message <- switch(physical_error,
  #                                    number_of_headers = "Please provide the number of headers preceeding the data.",
  #                                    attribute_orientation = "Please provide the orientation of the attributes.",
  #                                    field_delimiter = "Please provide the character used to delimit the end of an attribute.",
  #                                    data_url = "No url has been provided. Please input a url to which the data file can be downloaded if possible.")
  #   
  #   if (missing(data_url)) {
  #     warning(physical_error_message, call. = FALSE)
  #   } else {
  #     stop(physical_error_message, call. = FALSE)
  #   } 
  # }
  # 
  # if (record_delimiter == "\\r\\n") {
  #   warning("The default value of '\\r\\n' has been assigned. Please input a different value for record_delimiter if this is the wrong input.", call. = FALSE)
  # }
  # 
  # field_delimiter_characters <- length(unlist(strsplit(field_delimiter, " ")))
  # 
  # if (field_delimiter_characters > 1) {
  #   stop('field_delimiter can only be one character long.', call. = FALSE)
  # }
  # 
  
  if (missing(data_url)) {
    warning('No url has been provided. Please input a url to which the data file can be downloaded if possible.', call. = FALSE)
  } else {
    physical$distribution$online$url <- data_url
  }
  
 
  
  physical <- list(objectName = object_name,
                   size = list(unit = "bytes",
                               size = object_size),
                   authentication = list(method = "MD5", 
                                         authentication = authentication),
                   dataFormat = list(textFormat = 
                                       list(numHeaderLines = number_of_headers,
                                            recordDelimiter = record_delimiter,
                                            attributeOrientation = attribute_orientation,
                                            simpleDelimited = list(fieldDelimiter = field_delimiter))))
  
  return(physical)
}