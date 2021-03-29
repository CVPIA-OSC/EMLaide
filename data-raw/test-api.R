library(httr)
library(jsonlite)
library(EDIutils)
library(xml2)
library(tidyr)

# Define variables -------------------------------------------------------------
file_path <- "vignettes/edi.678.1.xml" 
user_id <- "ecain"
# Data package identifier reservation ------------------------------------------
reserve_edi_id <- function(user_id, password) {
  r <-httr::POST(
    url = "https://pasta-d.lternet.edu/package/reservations/eml/edi",
    config = httr::authenticate(paste('uid=', user_id, ",o=EDI", ',dc=edirepository,dc=org'), password)
  )
  edi_number <- httr::content(r, as = 'text', encoding = 'UTF-8')
  paste0("edi.", edi_number, ".1", sep = "")
}

# Play around with evaluation online...try with dataset that has URL------------

evaluate_edi_package <- function(user_id, password, file_path) {
  r <- httr::POST(
    url = "https://pasta-d.lternet.edu/package/evaluate/eml",
    config = httr::authenticate(paste('uid=', user_id, ",o=EDI", ',dc=edirepository,dc=org'), password),
    body = httr::upload_file(file_path)
  )
  if (r$status_code == "201") {
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
   
  report_df <- tibble("Status" = as_vector(flatten(status)), 
                      "Element Checked" = as_vector(flatten(name)),
                      "Suggestion to fix/imporve" = as_vector(flatten(suggestion)))
  return(report_df)
  
  } else {
    message("Your request to evaluate an EDI package failed,
           please check that you entered a valid username, password, and XML document.
           That XML document must link to a csv accessible online.
           See more information on request status below")
    print(r)
  }
  
}