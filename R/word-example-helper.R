#' @title Word Document Example Reader 
#' @description Helps read documents which are present in the examples of the
#'  \code{EDIutils} package. 
#' @param path Name of file. If `NULL`, the example files will be listed.
#' @export
#' @examples
#' word_example()
#' word_example("abstract_template.docx")
word_example <- function(path = NULL) {
  if (is.null(path)) {
    dir(system.file("extdata", package = "EDIutils"))
  } else {
    system.file("extdata", path, package = "EDIutils", mustWork = TRUE)
  }
}