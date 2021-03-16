library(httr)
library(jsonlite)
library(EDIutils)

# Define variables -------------------------------------------------------------
file_path <- "vignettes/edi.678.1.xml" 
user_id <- "ecain"
# password <- Sys.getenv('edi_password')
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


r <- httr::POST(
  url = "https://pasta-d.lternet.edu/package/evaluate/eml",
  config = httr::authenticate(paste('uid=', user_id, ",o=EDI", ',dc=edirepository,dc=org'), password),
  body = httr::upload_file(file_path)
)
r
transaction_id <- httr::content(r, as = 'text', encoding = 'UTF-8')


r <- httr::GET(
  url = paste0("https://pasta-d.lternet.edu/package/evaluate/report/eml/",
               transaction_id),
  config = httr::authenticate(paste('uid=', user_id, ",o=EDI", ',dc=edirepository,dc=org'), password)
)
report <- httr::content(r, as = 'text', encoding = 'UTF-8')
report




