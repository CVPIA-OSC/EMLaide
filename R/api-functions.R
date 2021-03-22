# Data package identifier reservation ------------------------------------------
#' Reserve EDI ID 
#' @description This package returns a funder
#' @param user_id A string that contains the user ID for the EDI data portal. If you 
#' do not already have an EDI account you may create one on the EDI portal.
#' \url {https://portal.edirepository.org/nis/login.jsp}
#' @param password A string that contains the user password for the EDI data portal. 
#'
#' @return This function returns a edi identifier number. 
#'
#' @examples 
#' reserve_edi_id(user_id = "samuelwright", 
#'                password = "340account")
#'                
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
