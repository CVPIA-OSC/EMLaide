# Data package identifier reservation ------------------------------------------
#' Reserve EDI Data Package Identifier 
#' @description This package reserves and returns a unique EDI number. 
#' @param user_id EDI data portal user ID. Create an account an
#' EDI \href{https://portal.edirepository.org/nis/login.jsp}{here}
#' @param password EDI data portal user password
#' @param environment EDI portal environment to run command in. Can be: "production" - environment for publishing to EDI , 
#' "staging" - environment to test upload and rendering of new environment, "development"
#' @details For more information about the identifier reservation services see \href{https://pastaplus-core.readthedocs.io/en/latest/doc_tree/pasta_api/data_package_manager_api.html#reservations}{the PASTAplus docs}
#' @return This function returns a edi identifier number. 
#' @examples 
#' \dontrun{
#' reserve_edi_id(user_id = "samuelwright")}
#' @export                

reserve_edi_id <- function(user_id, password, environment = "production") {
  base_url <- dplyr::case_when(environment == "staging" ~ "https://pasta-s.lternet.edu/package/reservations/eml/edi",
                               environment == "development" ~ "https://pasta-d.lternet.edu/package/reservations/eml/edi",
                               environment == "production" ~ "https://pasta.lternet.edu/package/reservations/eml/edi")
  response <-httr::POST(
    url = base_url,
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
#' @param environment EDI portal environment to run command in. Can be: "production" - environment for publishing to EDI , 
#' "staging" - environment to test upload and rendering of new environment, "development"
#' @param eml_file_path The file path to the EML metadata document that you wish to evaluate. 
#' (A web link to the csv must be included in the dataset information in the EML in order for a data package to be evaluated.) 
#' @details For more information about the validation services see \href{https://pastaplus-core.readthedocs.io/en/latest/doc_tree/pasta_api/data_package_manager_api.html#upload-and-evaluation}{the PASTAplus docs}
#' @return This package returns a data frame that contains the status of the 
#' package. The data frame contains the following information:
#' * The status of each check - Can be valid, info, warn, or error. Errors must be fixed before package can be uploaded to EDI. 
#' * A description of what each status is referring to 
#' * A suggestion of how to fix the status or improve the package. 
#' @examples 
#' \dontrun{evaluate_edi_package(user_id = "samuelwright", 
#'                               eml_file_path = "data/edi20.1.xml")}
#' @export   
evaluate_edi_package <- function(user_id, password, eml_file_path, environment = "staging") {
  # Select package environment 
  base_url <- dplyr::case_when(environment == "staging" ~ "https://pasta-s.lternet.edu/package/",
                               environment == "development" ~ "https://pasta-d.lternet.edu/package/",
                               environment == "production" ~ "https://pasta.lternet.edu/package/")
  
  # post package to EDI for evaluation 
  response <- httr::POST(
    url = paste0(base_url, "evaluate/eml"),
    config = httr::authenticate(paste('uid=', user_id, ",o=EDI", ',dc=edirepository,dc=org'), password),
    body = httr::upload_file(eml_file_path)
  )
  if (response$status_code == "202") {
    # pull transaction id from response content 
    transaction_id <- httr::content(response, as = 'text', encoding = 'UTF-8')
    iter <- 0
    max_iter <- 5
    while(TRUE){ # Loop through a few times to give EDI time to evaluate package 
      Sys.sleep(2) 
      # use transaction id to read evaluation report
      response<- httr::GET(
        url = paste0(base_url, "evaluate/report/eml/", transaction_id),
        config = httr::authenticate(paste('uid=', user_id, ",o=EDI", ',dc=edirepository,dc=org'), password)
      )
      iter <- iter + 1
      if (response$status_code == "200") {
        # use generate_report_df() function defined above to parse transaction_response 
        # content into a report_df table 
        report_df <- generate_report_df(response)
        print("Please check for errors in the report dataframe")
        return(report_df)
        break
      }
      if (max_iter > iter) {
        print("Request timed out, check that you inputs are all valid and try again")
        break 
      }
    }
  } else {
    message("Your request to evaluate an EDI package failed,
           please check that you entered a valid username, password, and XML document.
           That XML document must link to a csv accessible online.
           See more information on request status below")
    print(response)
  }
}


# TODO add test api funcitons
# Upload EDI Data package -------------------------------------------------------
#' Upload EDI Data Package 
#' @description This function takes in authentication info for EDI and an EML file to 
#' be evaluated uploaded to EDI. 
#' @param user_id EDI data portal user ID. Create an account an
#' EDI \href{https://portal.edirepository.org/nis/login.jsp}{here}
#' @param password EDI data portal user password
#' @param environment EDI portal environment to run command in. Can be: "production" - environment for publishing to EDI , 
#' "staging" - environment to test upload and rendering of new environment, "development"
#' @param eml_file_path The file path to the EML metadata document that you wish to evaluate. 
#' (A web link to the csv must be included in the dataset information in the EML in order for a data package to be evaluated.) 
#' @details For more information about the validation services see \href{https://pastaplus-core.readthedocs.io/en/latest/doc_tree/pasta_api/data_package_manager_api.html#upload-and-evaluation}{the PASTAplus docs}
#' @return Message describing if your package was successfully updated or not. 
#' @examples 
#' \dontrun{upload_edi_package(user_id = "samuelwright", 
#'                             eml_file_path = "data/edi20.1.xml")}
#' @export   

upload_edi_package <- function(user_id, password, eml_file_path, environment = "production") {
  # Select package environment& define 
  base_url <- dplyr::case_when(environment == "staging" ~ "https://pasta-s.lternet.edu/package/",
                               environment == "development" ~ "https://pasta-d.lternet.edu/package/",
                               environment == "production" ~ "https://pasta.lternet.edu/package/")
  # Define scope (edi) and identifier (package number) and revision
  scope <- unlist(strsplit(eml_file_path, "\\."))[1]
  identifier <- unlist(strsplit(eml_file_path, "\\."))[2]
  revision <- unlist(strsplit(eml_file_path, "\\."))[3]
  # post package to EDI for upload 
  response <- httr::POST(
    url = paste0(base_url, "eml/"),
    config = httr::authenticate(paste('uid=', user_id, ",o=EDI", ',dc=edirepository,dc=org'), password),
    body = httr::upload_file(eml_file_path)
  )
  
  if (response$status_code == "202") {
    Sys.sleep(2) 
    transaction_id <- httr::content(response, as = 'text', encoding = 'UTF-8')
    check_error <- httr::GET(url = paste0(base_url, "error/eml/", transaction_id), 
                             config = httr::authenticate(paste('uid=', user_id, ",o=EDI", ',dc=edirepository,dc=org'), password))
    # If check error = 200 it means the package did not post, use the message to understand why 
    message <- substr(httr::content(check_error, as = 'text', encoding = 'UTF-8'), 1, 64)
    # the data package already exists in the staging area - first if statement
    if (message == "Attempting to insert a data package that already exists in PASTA") {
      print("Attempting to insert a data package that already exists in PASTA. Please reserve a different identifier or update the existing package using update_edi_package()")
    }
    # the EML is not valid - we must view errors in error report dataframe
    else if (check_error$status_code == "200" & 
             message != "Attempting to insert a data package that already exists in PASTA") { 
      report_df <- generate_report_df(check_error)
      print("EML not valid. Please fix errors in report dataframe or if report dataframe comes back empty please try to evaluate_edi_package().")
      return(report_df)
      break
    } else {
      iter <- 0
      max_iter <- 5
      while(TRUE){ # Loop through a few times to give EDI time to upload package 
        Sys.sleep(2)
        # If check_error does not equal 200, run the check upload lines below to view upload
        check_upload <- httr::GET(url = paste0(base_url, 
                                               "report/eml/", 
                                               scope, "/", identifier, "/", revision), 
                                  config = httr::authenticate(paste('uid=', user_id, ",o=EDI", ',dc=edirepository,dc=org'), password))
        iter <- iter + 1
        if (check_upload$status_code == "200") {
          print("Your data package posted to EDI. Please check EDI portal to confirm")
          break
        }
        # Stop loop if iterating through more than 5 times 
        else if(max_iter > iter) {
          print("Request timed out, check that you inputs are all valid, rerun evalutate_edi_package(), and try again")
          break 
        }
      }
    }
    # Adds error handling message for 505, 405 & other errors that come from bad initial response 
  } else {
    message("Your request to upload an EDI package failed,
           please check that you entered a valid username, password, and XML document.
           That XML document must link to a csv accessible online.
           See more information on request status below")
    print(response)
  }
}

# TODO add test api functions
# Update Data package on EDI ---------------------------------------------------
#' Update EDI Data Package 
#' @description This function takes in authentication info for EDI, a package number, and an updated EML file to 
#' updated an existing package on EDI. 
#' @param user_id EDI data portal user ID. Create an account an
#' EDI \href{https://portal.edirepository.org/nis/login.jsp}{here}
#' @param password EDI data portal user password
#' @param environment EDI portal environment to run command in. Can be: "production" - environment for publishing to EDI , 
#' "staging" - environment to test upload and rendering of new environment, "development"
#' @param existing_package_identifier The current edi number of the package that you are trying to update.(ex: "edi.101.1")
#' @param eml_file_path The file path to the EML metadata document that you wish to use to update. 
#' (A web link to the csv must be included in the dataset information in the EML in order for a data package to be evaluated.) 
#' @return Message describing if your package was successfully updated or not. 
#' @examples 
#' \dontrun{update_edi_package(user_id = "samuelwright", 
#'                             existing_package_identifier = "edi.740.1",
#'                             eml_file_path = "data/edi20.1.xml")}
#' @export   

update_edi_package <- function(user_id, password, existing_package_identifier, eml_file_path, environment = "production") {
  # Define scope (edi) and identifier (package number)
  scope <- unlist(strsplit(existing_package_identifier, "\\."))[1]
  identifier <- unlist(strsplit(existing_package_identifier, "\\."))[2]
  revision <- unlist(strsplit(basename(eml_file_path), "\\."))[3]
  
  # Select package environment 
  base_url <- dplyr::case_when(environment == "staging" ~ "https://pasta-s.lternet.edu/package/",
                               environment == "development" ~ "https://pasta-d.lternet.edu/package/",
                               environment == "production" ~ "https://pasta.lternet.edu/package/")
  # post package to EDI for update
  response <- httr::PUT(
    url = paste0(base_url, "eml/", scope, "/", identifier),
    config = httr::authenticate(paste('uid=', user_id, ",o=EDI", ',dc=edirepository,dc=org'), password),
    body = httr::upload_file(eml_file_path)
  )
  if (response$status_code == "202") {
    Sys.sleep(2)
    transaction_id <- httr::content(response, as = 'text', encoding = 'UTF-8')
    check_error <- httr::GET(url = paste0(base_url, "error/eml/", transaction_id), 
                             config = httr::authenticate(paste('uid=', user_id, ",o=EDI", ',dc=edirepository,dc=org'), password))
    # If check error = 200 it means the package did not post, use the message to understand why 
    message <- substr(httr::content(check_error, as = 'text', encoding = 'UTF-8'), 1, 64)
    revision_number <- unlist(strsplit(eml_file_path, "\\."))[3]
    # the data package revision already exists in the staging area - first if statement
    if (message == paste0("Attempting to update a data package to revision ", "'", revision_number, "' ", "but an equal")) {
      print("Attempting to insert a version that already exists in PASTA. Please reserve a different identifier or update to the next revision")
    }
    # the EML is not valid - we must view errors in error report dataframe
    else if (check_error$status_code == "200" & 
             message != paste0("Attempting to update a data package to revision ", "'", revision_number, "' ", "but an equal")) { 
      report_df <- generate_report_df(check_error)
      print("EML not valid. Please fix errors in report dataframe or if report dataframe comes back empty please try to evaluate_edi_package().")
      return(report_df)
      break
    } else {
      iter <- 0
      max_iter <- 5
      while(TRUE){ # Loop through a few times to give EDI time to upload updated package 
        Sys.sleep(2)
        # If check_error does not equal 200, run the check upload lines below to view upload
        check_upload <- httr::GET(url = paste0(base_url, "report/eml/", 
                                               scope, "/", identifier, "/", revision), 
                                  config = httr::authenticate(paste('uid=', user_id, ",o=EDI", ',dc=edirepository,dc=org'), password))
        iter <- iter + 1
        if (check_upload$status_code == "200") {
          print(paste("Your data package posted to EDI. Please check EDI", environment, "portal to confirm"))
          break
        }
        # Stop loop if iterating through more than 5 times 
        else if(max_iter > iter) {
          print("Request timed out, check that you inputs are all valid, rerun evalutate_edi_package(), and try again")
          break 
        }
      }
    }
    # Adds error handling message for 505, 405 & other errors that come from bad initial response 
  } else {
    message("Your request to update an EDI package failed,
           please check that you entered a valid username, password, and XML document.
           That XML document must link to a csv accessible online.
           See more information on request status below")
    print(response)
  }
}

# Helper functions 
#' Generate Report Dataframe 
#' @description Generates Package Report in Data Frame 
#' @export   
generate_report_df <- function(response) {
  report <- httr::content(response, as = 'text', encoding = 'UTF-8')
  name <- stringr::str_extract_all(report, "(?<=<name>)(.*)(?=</name>)")[[1]]
  status <- stringr::str_extract_all(report, '[:alpha:]+(?=</status>)')[[1]]
  suggestion <- stringr::str_extract_all(report, "(?<=<suggestion>)(.*)(?=</suggestion>)")[[1]]
  
  report_df <- dplyr::tibble("Status" = as.vector(status), 
                             "Element Checked" = as.vector(name),
                             "Suggestion to fix/imporve" = as.vector(suggestion))
  if (nchar(report) <= 500){
    print(report)
  }
  return(report_df)
}