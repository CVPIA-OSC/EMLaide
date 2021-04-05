# Data package identifier reservation ------------------------------------------
#' Reserve EDI ID 
#' @description This package returns a funder
#' @param user_id A string that contains the user ID for the EDI data portal. If you 
#' do not already have an EDI account you may create one on the EDI portal. \url {https://portal.edirepository.org/nis/login.jsp}
#' @param password A string that contains the user password for the EDI data portal. 
#' @return This function returns a edi identifier number. 
#' @examples 
#' reserve_edi_id(user_id = "samuelwright", 
#'                password = "340account")
#' @export                

reserve_edi_id <- function(user_id, password) {
  r <-httr::POST(
    url = "https://pasta-d.lternet.edu/package/reservations/eml/edi",
    config = httr::authenticate(paste('uid=', user_id, ",o=EDI", ',dc=edirepository,dc=org'), password)
  )
  if (r$status_code == "201") {
  edi_number <- httr::content(r, as = 'text', encoding = 'UTF-8')
  paste0("edi.", edi_number, ".1", sep = "")
} else {
  message("Your request to reserve an EDI number failed, 
          please check that you entered a valid username and password. 
          See more information on request status below")
  print(r)
}
}

# Evaluate EDI Data package -------------------------------------------------------
#' Evaluate EDI Package 
#' @description This function takes in authentication info for edi and an EML file to 
#' be evaluated using the EDI congruency checker. This package returns a data frame that contains the status of the 
#' package. 
#' @param user_id A string that contains the user ID for the EDI data portal. If you 
#' do not already have an EDI account you may create one on the EDI portal. \url {https://portal.edirepository.org/nis/login.jsp}
#' @param password A string that contains the user password for the EDI data portal. 
#' @param eml_file_path A string that contains the filepath to the EML document that 
#' contains all the data package information. A web link to the csv must be included 
#' in the dataset information in the EML in order for a data package to be evaluated. 
#' @return This package returns a data frame that contains the status of the 
#' package. The data frame contains a column with the status, a column with the descriotion 
#' of what each status is referring to, and a column that contains a suggestion of how 
#' to fix the status or improve the package. 
#' @examples 
#' reserve_edi_id(user_id = "samuelwright", 
#'                password = "340account",
#'                eml_file_path = "data/edi20.1.xml")
#' @export   
evaluate_edi_package <- function(user_id, password, eml_file_path) {
  r <- httr::POST(
    url = "https://pasta-d.lternet.edu/package/evaluate/eml",
    config = httr::authenticate(paste('uid=', user_id, ",o=EDI", ',dc=edirepository,dc=org'), password),
    body = httr::upload_file(eml_file_path)
  )
  if (r$status_code == "202") {
    transaction_id <- httr::content(r, as = 'text', encoding = 'UTF-8')
    r <- httr::GET(
      url = paste0("https://pasta-d.lternet.edu/package/evaluate/report/eml/",
                   transaction_id),
      config = httr::authenticate(paste('uid=', user_id, ",o=EDI", ',dc=edirepository,dc=org'), password)
    )
    report <- httr::content(r, as = 'text', encoding = 'UTF-8')
    name <- stringr::str_extract_all(report, "(?<=<name>)(.*)(?=</name>)")
    status <- stringr::str_extract_all(report, '[:alpha:]+(?=</status>)')
    suggestion <- stringr::str_extract_all(report, "(?<=<suggestion>)(.*)(?=</suggestion>)")
    
    report_df <- dplyr::tibble("Status" = as.vector(purrr::flatten(status)), 
                        "Element Checked" = as.vector(purrr::flatten(name)),
                        "Suggestion to fix/imporve" = as.vector(purrr::flatten(suggestion)))
    report_df
  } else {
    message("Your request to evaluate an EDI package failed,
           please check that you entered a valid username, password, and XML document.
           That XML document must link to a csv accessible online.
           See more information on request status below")
    print(r)
  }
  
}
