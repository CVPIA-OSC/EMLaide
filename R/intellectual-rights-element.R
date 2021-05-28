#' Create License and Intellectual Rights
#' @description Creates the intellectual rights information of a dataset based off of EML standards.
#' @param default_license Use "CCO" or "CCBY" as argument value to use one of the 
#' CVPIA default licenses. Supply NULL if using another license.
#' @param license_name (Optional) if using default_license = "CCO" or "CCBY" as 
#' default values are provided. Otherwise, please provide the appropriate license 
#' name. Other possible license examples include "MIT License" and "Creative Commons 
#' Attribution Non Commercial Share Alike 4.0 International". 
#' @param license_url (Optional) if using default_license = "CCO" or "CCBY" as default
#' values are provided. Otherwise, please provide the correct license url 
#' to view further license information.
#' @param license_identifier (Optional) if using default_license = "CCO" or "CCBY" 
#' as default values are provided. Otherwise, please provide the appropriate 
#' identifier. Other possible identifiers include "MIT" and "CC-BY-NC-SA". 
#' @param intellectual_rights_descripiton (Optional) if using default_license = "CCO" 
#' or "CCBY" as default values are provided. Otherwise, please provide a short 
#' description of the license being used and its regulations. 
#' @details 
#' Learn more about the default CVPIA licese options from Creative Commons:
#' \itemize{
#'  \item \href{https://creativecommons.org/publicdomain/zero/1.0/}{CCO} - 
#'  The most premissive license, appropriate for data in the public domain.
#'  \item \href{https://creativecommons.org/licenses/by/4.0/}{CC BY} - Attribution required
#' }
#' 
#' To understand the difference between license choice or to use another Creative Commons license
#' \href{https://en.wikipedia.org/wiki/Creative_Commons_license}{this} wikipedia page is helpful.
#' 
#' To view more license choices and find the appropriate url and identifier visit
#' \href{https://spdx.org/licenses/CC0-1.0.html}{this} link. 
#' 
#' @return A intellectual rights element that can be appended to a dataset or project list. 
#' 
#' @examples 
#' create_license() # defaults to CCO
#' 
#' create_license(default_license = "CCBY")
#' 
#' create_license(default_license = NULL, 
#'            license_name = "Creative Commons Attribution Non Commercial Share Alike 4.0 International", 
#'            license_url = "https://spdx.org/licenses/CC-BY-NC-SA-4.0.html", 
#'            license_identifier = "CC-BY-NC-SA-4.0", 
#'            intellectual_rights_descripiton = "This information is released under 
#'            the Creative Commons license - Attribution - CC BY-NC-SA 
#'            (https://creativecommons.org/licenses/by-nc-sa/4.0/). The consumer 
#'            of these data (\"Data User\" herein) is required to cite it 
#'            appropriately in any publication that results from its use. 
#'            The Data User should realize that these data may be actively 
#'            used by others for ongoing research and that coordination may be 
#'            necessary to prevent duplicate publication. The Data User is urged 
#'            to contact the authors of these data if any questions about 
#'            methodology or results occur. Where appropriate, the Data User is 
#'            encouraged to consider collaboration or co-authorship with the authors. 
#'            The Data User should realize that misinterpretation of data may occur 
#'            if used out of context of the original study. While substantial efforts 
#'            are made to ensure the accuracy of data and associated documentation, 
#'            complete accuracy of data sets cannot be guaranteed. All data are made 
#'            available \"as is.\" The Data User should be aware, however, that 
#'            data are updated periodically and it is the responsibility of the
#'            Data User to check for new versions of the data. The data authors and
#'            the repository where these data were obtained shall not be liable for 
#'            damages resulting from any use or misinterpretation of the data. 
#'            You may not use the material for commercial purposes and you must 
#'            distribute your contributions on this same license. Thank you.&#13;")
#' 
#' @export

create_license <- function(default_license = c("CCO", "CCBY"), 
                           license_name = NULL, license_url = NULL, 
                           license_identifier = NULL, intellectual_rights_descripiton = NULL) {
  
  default_license <- match.arg(default_license)
  
  if (!is.null(license_name) && !is.na(license_name)) {
    
      required_arguments <- c("license_url", "license_identifier", "intellectual_rights_descripiton")
      missing_argument_index <- which(c(missing(license_url), missing(license_identifier), 
                                missing(intellectual_rights_descripiton)))
      
      if (length(missing_argument_index) > 0) {
        ir_error <- required_arguments[missing_argument_index][1]
        ir_error_message <- switch(ir_error,
                                   license_url = "Please provide a url for the license.",
                                   license_identifier = "Please provide the license identifier.",
                                   intellectual_rights_descripiton = 
                                     "Please provide a simplified description of the license.")
        stop(ir_error_message, call. = FALSE)
    }
    
    intellectualRights <- list(para = intellectual_rights_descripiton)
    licensed <- list(licenseName = license_name, 
                                    url = license_url,
                                    identifier = license_identifier)
  } else {
    if (default_license == "CCO") {
      intellectualRights <- list(para = "This data package is released to the \"public domain\" under Creative Commons CC0 1.0 \"No Rights Reserved\" (see: https://creativecommons.org/publicdomain/zero/1.0/). It is considered professional etiquette to provide attribution of the original work if this data package is shared in whole or by individual components. A generic citation is provided for this data package on the website https://portal.edirepository.org (herein \"website\") in the summary metadata page. Communication (and collaboration) with the creators of this data package is recommended to prevent duplicate research or publication. This data package (and its components) is made available \"as is\" and with no warranty of accuracy or fitness for use. The creators of this data package and the website shall not be liable for any damages resulting from misinterpretation or misuse of the data package or its components. Periodic updates of this data package may be available from the website. Thank you.")
      licensed <- list(licenseName = "Creative Commons Zero v1.0 Universal", 
                                      url = "https://spdx.org/licenses/CC0-1.0.html",
                                      identifier = "CC0-1.0")
    }
    
    if (default_license == "CCBY") {
      intellectualRights <- list(para = "This information is released under the Creative Commons license - Attribution - CC BY (https://creativecommons.org/licenses/by/4.0/). The consumer of these data (\"Data User\" herein) is required to cite it appropriately in any publication that results from its use. The Data User should realize that these data may be actively used by others for ongoing research and that coordination may be necessary to prevent duplicate publication. The Data User is urged to contact the authors of these data if any questions about methodology or results occur. Where appropriate, the Data User is encouraged to consider collaboration or co-authorship with the authors. The Data User should realize that misinterpretation of data may occur if used out of context of the original study. While substantial efforts are made to ensure the accuracy of data and associated documentation, complete accuracy of data sets cannot be guaranteed. All data are made available \"as is.\" The Data User should be aware, however, that data are updated periodically and it is the responsibility of the Data User to check for new versions of the data. The data authors and the repository where these data were obtained shall not be liable for damages resulting from any use or misinterpretation of the data. Thank you.")
      licensed <- list(licenseName = "Creative Commons Attribution 4.0 International", 
                                      url = "https://spdx.org/licenses/CC-BY-4.0.html",
                                      identifier = "CC-BY-4.0")
    }
  }
  
  return(list(intellectualRights = intellectualRights, licensed = licensed))
}

#' Add License
#' @description Adds the license element to a dataset list according to EML standards. 
#' @param parent_element A list representing the EML project or dataset.
#' @param license_metadata A table or list containing license metadata: see \code{\link{create_license}} 
#' @return The dataset list or project with intellectual rights information appended.
#' @examples 
#' license_metadata = list(default_license = "CCBY")
#' dataset <- list() %>%
#'     add_license(license_metadata)
#' dataset
#' @export

add_license <- function(parent_element, license_metadata) {
  license_list <- create_license(default_license = license_metadata$default_license,
                                 license_name = license_metadata$license_name, 
                                 license_url = license_metadata$license_url, 
                                 license_identifier = license_metadata$license_identifier, 
                                 intellectual_rights_descripiton = license_metadata$intellectual_rights_description)
  parent_element$intellectualRights <- license_list$intellectualRights
  parent_element$licensed <- license_list$licensed

  return(parent_element)
}