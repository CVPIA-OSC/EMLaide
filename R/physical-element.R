#' @title Add Physical Element 
#' @description Adds the information of the physical format of the dataset based off of EML standards.
#' @param file_path The file path of the data set being documented. The file size 
#' and authentication checksums will be generated from this input.
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
#' add_physical(file_path = "User/data/example.csv", 
#'              number_of_headers = "1",
#'              record_delimiter = "\\r\\n", 
#'              attribute_orientation = "column", 
#'              field_delimiter = ",",
#'              data_url = "https://mydata.org/etc")
#'              
#' add_physical(file_path = "User/data/example.csv",
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
  
  if (missing(data_url)) {
    warning('No url has been provided. Please input a url to which the data file can be downloaded if possible.', call. = FALSE)
  } else {
    physical$distribution = list(online = list(url = list(url = data_url,
                                                          "function" = "download")))
  }
  
  return(physical)
}