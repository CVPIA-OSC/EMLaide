#' @title Add Access Element
#' @description The add access element creates a list that includes all the required
#' elements for the access section of an EML document. Calling add_access() will output the 
#' default public access permissions and cvpia write access permissions. To customize the access 
#' level provide values for the allow_principal, add_permission, and cvpia_access arguments. 
#' @param allow_principal The principal argument refers to who will have access
#' to this dataset. The allow_principal argument takes in a string. 
#' With no input the allow_principal will default to "public". To change
#' to a different principal please input a new principal. 
#' 
#' The valid options for the principal are:
#'   * 'public' - special principal indicate that any user or group has a particular access permission 
#'   * other individual users or groups can be specified if they are defined in the PASTA EDI authentication 
#'   system. Read the PASTA documentation for more information <https://pastaplus-core.readthedocs.io/_/downloads/en/latest/pdf/>  
#' 
#' @param allow_permission The permission argument refers to the type of access 
#' that is allowed to the principal. The allow_permission argument takes in a string. 
#' With no input the allow_permission will default to "read". This default permission
#' allows the principal to read the dataset but not edit. To change to a different 
#' permission please input a new permission. 
#' 
#' The valid options for permission are: 
#'   * 'read' - allow viewing of the resource, 
#'   * 'write' - allow modification of the resource (except for access rules),  
#'   * 'changePermission' - modifications including access rules, and  
#'   * 'all' - all of the above.  
#'   
#' @param cvpia_access The cvpia_access argument allows for an additional set of permissions 
#' for a cvpia user group. 
#' The valid options for cvpia_access are:
#'   * TRUE - default. Gives public "read" access and a CVPIA user group "write" access.
#'   * FALSE - Inputs for allow_permission and allow_principal are used as the single set of permissions.
#'   
#' @return Returns an list with all the information required for the access section 
#' of an EML document.
#' @examples
#' add_access()
#' 
#' add_access(allow_principal = "private",
#'            allow_permission = "none",
#'            cvpia_access = FALSE)
#' @export
 
add_access <- function(allow_principal = NULL, allow_permission = NULL, cvpia_access = TRUE){
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
  if(cvpia_access){
    access <- list(scope = "document",
                   order = "allowFirst",
                   authSystem = "https://pasta.edirepository.org/authentication",
                   allow = list(list(principal = principal,
                                     permission = permission),
                                list(principal = "cvpia",
                                     permission = "write")))
  }else{
    access <- list(scope = "document",
                   order = "allowFirst", 
                   authSystem = "https://pasta.edirepository.org/authentication",
                   allow = list(principal = principal, 
                                permission = permission))
  }
  return(access)
}


