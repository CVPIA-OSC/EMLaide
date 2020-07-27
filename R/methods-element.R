#' @title TODO
#' @description TODO
#' @param method_step_title TODO
#' @param method_step_description TODO
#' @param instrumentation TODO
#' @reutn TODO
#' @examples TODO
#' @export 
#' 
add_method_step <- function(method_step_title, method_step_description, instrumentation) {
  
  methods <- list(methodStep = list(description = list(seciton = list(title = method_step_title, 
                                                       para = method_step_description)),
                                    instrumentation = instrumentation))
  
  return(methods)
}