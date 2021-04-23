# Data package identifier reservation ------------------------------------------
#' Reserve EDI Data Package Identifier 
#' @description This package reserves and returns a unique EDI number. 
#' @param user_id EDI data portal user ID. Create an account an
#' EDI \href{https://portal.edirepository.org/nis/login.jsp}{here}
#' @param password EDI data portal user password
#' @return This function returns a edi identifier number. 
#' @examples 
#' reserve_edi_id(user_id = "samuelwright")
#' @export                

reserve_edi_id <- function(user_id, password) {
  response <-httr::POST(
    url = "https://pasta-d.lternet.edu/package/reservations/eml/edi",
    config = httr::authenticate(paste("uid=", user_id, ",o=EDI", ",dc=edirepository,dc=org"), password)
  )
  if (response$status_code == "201") {
  edi_number <- httr::content(response, as = "text", encoding = "UTF-8")
  paste0("edi.", edi_number, ".1", sep = "")
} else {
  message("Your request to reserve an EDI number failed, 
          please check that you entered a valid username and password. 
          See more information on request status below")
  print(response)
}
}

# Evaluate EDI Data package -------------------------------------------------------
#' Validate EDI Data Package 
#' @description This function takes in authentication info for EDI and an EML file to 
#' be evaluated using the EDI congruence checker. This package returns a data frame that contains the status of the 
#' package. 
#' @param user_id EDI data portal user ID. Create an account an
#' EDI \href{https://portal.edirepository.org/nis/login.jsp}{here}
#' @param password EDI data portal user password
#' @param eml_file_path The file path to the EML metadata document that you wish to evaluate. 
#' (A web link to the csv must be included in the dataset information in the EML in order for a data package to be evaluated.) 
#' @return This package returns a data frame that contains the status of the 
#' package. The data frame contains the following information:
#' * The status of each check - Can be valid, info, warn, or error. Errors must be fixed before package can be uploaded to EDI. 
#' * A description of what each status is referring to 
#' * A suggestion of how to fix the status or improve the package. 
#' @examples 
#' revaluate_edi_package(user_id = "samuelwright", 
#'                       eml_file_path = "data/edi20.1.xml")
#' @export   
evaluate_edi_package <- function(user_id, password, eml_file_path) {
  response <- httr::POST(
    url = "https://pasta-d.lternet.edu/package/evaluate/eml",
    config = httr::authenticate(paste('uid=', user_id, ",o=EDI", ',dc=edirepository,dc=org'), password),
    body = httr::upload_file(eml_file_path)
  )
  Sys.sleep(2)
  if (response$status_code == "202") {
    transaction_id <- httr::content(response, as = 'text', encoding = 'UTF-8')
    response<- httr::GET(
      url = paste0("https://pasta-d.lternet.edu/package/evaluate/report/eml/",
                   transaction_id),
      config = httr::authenticate(paste('uid=', user_id, ",o=EDI", ',dc=edirepository,dc=org'), password)
    )
    report <- httr::content(response, as = 'text', encoding = 'UTF-8')
    name <- stringr::str_extract_all(report, "(?<=<name>)(.*)(?=</name>)")[[1]]
    status <- stringr::str_extract_all(report, '[:alpha:]+(?=</status>)')[[1]]
    suggestion <- stringr::str_extract_all(report, "(?<=<suggestion>)(.*)(?=</suggestion>)")[[1]]
    
    report_df <- dplyr::tibble("Status" = as.vector(status), 
                               "Element Checked" = as.vector(name),
                               "Suggestion to fix/imporve" = as.vector(suggestion))
    return(report_df)
  } else {
    message("Your request to evaluate an EDI package failed,
           please check that you entered a valid username, password, and XML document.
           That XML document must link to a csv accessible online.
           See more information on request status below")
    print(r)
  }
}


# Need to test out once I have a package to upload 
# Upload EDI Data package -------------------------------------------------------
#' Upload EDI Data Package 
#' @description This function takes in authentication info for EDI and an EML file to 
#' be evaluated uploaded to EDI. 
#' @param user_id EDI data portal user ID. Create an account an
#' EDI \href{https://portal.edirepository.org/nis/login.jsp}{here}
#' @param password EDI data portal user password
#' @param eml_file_path The file path to the EML metadata document that you wish to evaluate. 
#' (A web link to the csv must be included in the dataset information in the EML in order for a data package to be evaluated.) 
#' @return Message describing if your package was successfully updated or not. 
#' @examples 
#' upload_edi_package(user_id = "samuelwright", 
#'                eml_file_path = "data/edi20.1.xml")
#' @export   

upload_edi_package <- function(user_id, password, eml_file_path) {
  response <- httr::POST(
    url = "https://pasta-d.lternet.edu/package/eml",
    config = httr::authenticate(paste('uid=', user_id, ",o=EDI", ',dc=edirepository,dc=org'), password),
    body = httr::upload_file(eml_file_path)
  )
  Sys.sleep(2)
  if (response$status_code == "202") {
    transaction_id <- httr::content(response, as = 'text', encoding = 'UTF-8')
    response<- httr::GET(
      url = paste0("https://pasta-d.lternet.edu/package/report/eml/",
                   transaction_id),
      config = httr::authenticate(paste('uid=', user_id, ",o=EDI", ',dc=edirepository,dc=org'), password)
    )
    report <- httr::content(response, as = 'text', encoding = 'UTF-8')
    name <- stringr::str_extract_all(report, "(?<=<name>)(.*)(?=</name>)")[[1]]
    status <- stringr::str_extract_all(report, '[:alpha:]+(?=</status>)')[[1]]
    suggestion <- stringr::str_extract_all(report, "(?<=<suggestion>)(.*)(?=</suggestion>)")[[1]]
    
    report_df <- dplyr::tibble("Status" = as.vector(status), 
                               "Element Checked" = as.vector(name),
                               "Suggestion to fix/imporve" = as.vector(suggestion))
    return(report_df)
  } else {
    message("Your request to evaluate an EDI package failed,
           please check that you entered a valid username, password, and XML document.
           That XML document must link to a csv accessible online.
           See more information on request status below")
    print(r)
  }
}

# Need to test out once I have a package to upload 
# Upload EDI Data package -------------------------------------------------------
#' Upload EDI Data Package 
#' @description This function takes in authentication info for EDI and an EML file to 
#' be uploaded to EDI. 
#' @param user_id EDI data portal user ID. Create an account an
#' EDI \href{https://portal.edirepository.org/nis/login.jsp}{here}
#' @param password EDI data portal user password
#' @param eml_file_path The file path to the EML metadata document that you wish to upload. 
#' (A web link to the csv must be included in the dataset information in the EML in order for a data package to be evaluated.) 
#' @return Message describing if your package was successfully updated or not. 
#' @examples 
#' upload_edi_package(user_id = "samuelwright", 
#'                eml_file_path = "data/edi20.1.xml")
#' @export   

upload_edi_package <- function(user_id, password, eml_file_path) {
  response <- httr::POST(
    url = "https://pasta-d.lternet.edu/package/eml",
    config = httr::authenticate(paste('uid=', user_id, ",o=EDI", ',dc=edirepository,dc=org'), password),
    body = httr::upload_file(eml_file_path)
  )
  Sys.sleep(2)
  if (response$status_code == "202") {
    transaction_id <- httr::content(response, as = 'text', encoding = 'UTF-8')
    response<- httr::GET(
      url = paste0("https://pasta-d.lternet.edu/package/report/eml/",
                   transaction_id),
      config = httr::authenticate(paste('uid=', user_id, ",o=EDI", ',dc=edirepository,dc=org'), password)
    )
    report <- httr::content(response, as = 'text', encoding = 'UTF-8')
    name <- stringr::str_extract_all(report, "(?<=<name>)(.*)(?=</name>)")[[1]]
    status <- stringr::str_extract_all(report, '[:alpha:]+(?=</status>)')[[1]]
    suggestion <- stringr::str_extract_all(report, "(?<=<suggestion>)(.*)(?=</suggestion>)")[[1]]
    
    report_df <- dplyr::tibble("Status" = as.vector(status), 
                               "Element Checked" = as.vector(name),
                               "Suggestion to fix/imporve" = as.vector(suggestion))
    message("Your package has been succesfully uploaded")
    return(report_df)
  } else {
    message("Your request to evaluate an EDI package failed,
           please check that you entered a valid username, password, and XML document.
           That XML document must link to a csv accessible online.
           See more information on request status below")
    print(r)
  }
}
# Need to test out once I have a package to upload 
# Update EDI Data package -------------------------------------------------------
#' Update EDI Data Package 
#' @description This function takes in authentication info for EDI, a packge number, and an updated EML file to 
#' updated an existing package on EDI. 
#' @param user_id EDI data portal user ID. Create an account an
#' EDI \href{https://portal.edirepository.org/nis/login.jsp}{here}
#' @param password EDI data portal user password
#' @param existing_package_identifier The current edi number of the package that you are trying to update. 
#' Do not include the "edi" prefix, just put the number. (ex: For "edi.101.1" put 101.1)
#' @param eml_file_path The file path to the EML metadata document that you wish to use to update. 
#' (A web link to the csv must be included in the dataset information in the EML in order for a data package to be evaluated.) 
#' @return Message describing if your package was successfully updated or not. 
#' @examples 
#' upload_edi_package(user_id = "samuelwright", existing_packge_identifier = "740.1",
#'                    eml_file_path = "data/edi20.1.xml")
#' @export   

upload_edi_package <- function(user_id, password, existing_package_identifier, eml_file_path) {
  id <- stringr::str_replace_all(existing_package_identifier, "\\.", "/")
  url_for_update <- paste0("https://pasta-d.lternet.edu/package/eml/", id)
  response <- httr::PUT(
    url = url_for_update,
    config = httr::authenticate(paste('uid=', user_id, ",o=EDI", ',dc=edirepository,dc=org'), password),
    body = httr::upload_file(eml_file_path)
  )
  Sys.sleep(2)
  if (response$status_code == "202") {
    transaction_id <- httr::content(response, as = 'text', encoding = 'UTF-8')
    response<- httr::GET(
      url = paste0("https://pasta-d.lternet.edu/package/report/eml/",
                   transaction_id),
      config = httr::authenticate(paste('uid=', user_id, ",o=EDI", ',dc=edirepository,dc=org'), password)
    )
    report <- httr::content(response, as = 'text', encoding = 'UTF-8')
    name <- stringr::str_extract_all(report, "(?<=<name>)(.*)(?=</name>)")[[1]]
    status <- stringr::str_extract_all(report, '[:alpha:]+(?=</status>)')[[1]]
    suggestion <- stringr::str_extract_all(report, "(?<=<suggestion>)(.*)(?=</suggestion>)")[[1]]
    
    report_df <- dplyr::tibble("Status" = as.vector(status), 
                               "Element Checked" = as.vector(name),
                               "Suggestion to fix/imporve" = as.vector(suggestion))
    message("Your package has been succesfully updated")
    return(report_df)
  } else {
    message("Your request to evaluate an EDI package failed,
           please check that you entered a valid username, password, and XML document.
           That XML document must link to a csv accessible online.
           See more information on request status below")
    print(r)
  }
}
