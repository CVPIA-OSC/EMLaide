#' @title Add Full Excel Document
#' @name read_excel_sheets
#' @description This function takes in an excel doc with multiple sheets and creates a list 
#' that contains a tibble for each sheet in the excel file.The individual sheets 
#' no longer need to be read in by separate read_excel calls but can instead by accessed
#' by name from the list generated. 
#' @param excel_path The path to the excel document that you wish to read in. 
#' @return A list that contains a tibble for each sheet in the excel file. 
#' @examples 
#' read_excel_sheets(excel_path = "data-raw/template/template.xlsx")
#' @export

library(readxl)

read_excel_sheets <- function(excel_path) {
  sheets <- readxl::excel_sheets(excel_path)
  all_sheets <- lapply(sheets, function(x) readxl::read_excel(excel_path, sheet = x))
  names(all_sheets) <- sheets
  all_sheets 
}
