#' @title Add Access Element
#' @description The add access element creates a list that includes all the required
#' elements for the access section of an EML. Calling add_access() will output the 
#' default public access permissions. To create a non default access add principal 
#' and permission values into allow_principal and add_permission. 
#' @param allow_principal The principal argument refers to who will have access
#' to this dataset. The allow_principal argument takes in a string. 
#' With no input the allow_principal will default to "public". To change
#' to a different principal please input a new principal. 
#' @param allow_permission The permission argument refers to the type of access 
#' that is allowed to the principal. The allow_permission argument takes in a string. 
#' With no input the allow_permission will default to "read". This default permission
#' allows the principal to read the dataset but not edit. To change to a different 
#' permission please input a new permission. 
#' @return Returns an list with all the information required for the access section 
#' of an EML 
#' @examples
#' add_access()
#' add_access(allow_principal = "private",
#'            allow_permission = "none")
#' @export
 
add_access <- function(allow_principal = NULL, allow_permission = NULL){
  if (!is.null(allow_principal)) {
    principal = allow_principal
  } else {
    principal = "public"
  }
  if (!is.null(allow_permission)) {
    permission = allow_permission
  } else {
    permission = "read"
  }
  allow = list(principal = principal, 
               permission = permission)
  
  access <- list(scope = "document",
                 order = "allowFirst", 
                 authSystem = "https://pasta.edirepository.org/authentication",
                 allow = allow)
  return(access)
}


