#' @title Create Physical Element 
#' @description Create the information of the physical format of the dataset based off of EML standards.
#' @param file_path The file path of the data set being documented. The file size 
#' and authentication checksums will be generated from this input.
#' @param number_of_headers Number of header lines preceding the data. A default
#' of "1" is assigned if no input is given. Please refrain from inputting any other value
#' if possible as it is bad practice to do so. 
#' @param record_delimiter Character used to delimit records. If no value is
#' inputted the default value of '\\r\\n' is assigned. 
#' @param attribute_orientation The orientation of the attributes. A default of 
#' "column" will be assigned if no input is given.  
#' @param field_delimiter Character used to delimit each field. The options 
#' provided are commas (","), space (" "), tab ("\\t"), or colon (":"). The 
#' default is comma, as csv's are most common. 
#' @param data_url (Optional). A url, if possible, of which the data file can be 
#' downloaded. 
#' @return A complete set of information on the physical format. 
#' @examples
#' create_physical(file_path = "User/data/example.csv", 
#'              number_of_headers = "1",
#'              record_delimiter = "\\r\\n", 
#'              attribute_orientation = "column", 
#'              field_delimiter = ",",
#'              data_url = "https://mydata.org/etc")
#'              
#' create_physical(file_path = "User/data/example.csv",
#'              data_url = "https://mydata.org/etc")
#' @export
create_physical <- function(file_path,
                         number_of_headers = "1", 
                         record_delimiter = "\\r\\n", 
                         attribute_orientation = "column", 
                         field_delimiter = c(",", " ", "\\t", ":"),
                         data_url = NULL) {
 
  field_delimiter <- match.arg(field_delimiter)
  file_path_breaks <- unlist(strsplit(file_path, "/"))
  object_name <- paste(utils::tail(file_path_breaks, n = 1))
  authentication <- paste(tools::md5sum(file_path))  
  
  if (!is.null(data_url)){
    # td = tempdir()
    # tf = tempfile(tmpdir=td, fileext=".csv")
    # download.file(data_url, tf)
    # object_size <- paste(file.size(tf))
    github_info <- rvest::read_html("https://github.com/FlowWest/edi.935/blob/main/data/Environmentals.csv") %>% 
      rvest::html_element("body") %>%
      rvest::html_nodes(css = ".text-mono") %>%
      rvest::html_text()
    
    line_and_size <- github_info[[3]]
    line_and_size_split  <-  strsplit(line_and_size, split = '\n')[[1]]
    size_str <- line_and_size_split[length(line_and_size_split) - 1]
    size_val <- stringr::str_extract(size_str, pattern = '[0-9\\.]+')
    size_unit <- stringr::str_extract(size_str, pattern = '[A-Z]+$')
    
    object_size <- to_bytes(size_val, size_unit)

  } else {
    object_size <- paste(file.size(file_path))
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
  
  physical$distribution = list(online = list(url = list(url = data_url,
                                                          "function" = "download")))

  return(physical)
}

to_bytes = function(val, unit){
  unit_modifiers = c(
    'B'= 1,
    'KB'= 1000,
    'MB'= 1000000,
    'GB'= 1000000000
  )
  val = as.numeric(val)
  out = unname(val * unit_modifiers[unit])
  return(out)
}
