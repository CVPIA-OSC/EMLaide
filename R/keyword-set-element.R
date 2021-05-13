#' Creates Keyword Set Element
#' @description Creates keyword set according to EML standards.
#' @param keyword_set A keyword set or list of keyword sets.
#' @details A keyword set is a list with two elements: 
#' \enumerate{
#'   \item keyword - A list of keywords
#'   \item keywordThesauraus - (optional) A string identifying the controlled 
#'   vocabulary the keywords originate from, do not include if keywords are not
#'   from a controlled vocabulary
#' }
#' 
#' @section Controlled Vocabularies:
#' 
#' In order to promote consistency, please search the following resources for keywords:
#' 
#' \href{https://vocab.lternet.edu/vocab/vocab/index.php}{LTER} - Long Term Ecological Research.
#' 
#' \href{http://aims.fao.org/standards/agrovoc/functionalities/search}{AGROVOC} - 
#' A controlled vocabulary covering all areas of interest of the Food and Agriculture 
#' Organization (FAO) of the United Nations, including food, nutrition, agriculture, 
#' fisheries, forestry, environment.
#' 
#' \href{https://geonames.usgs.gov/apex/f?p=138:1:5668294677959}{U.S. Board on Geographic Names} - 
#' USGS place names dictionary.
#' 
#' @return The keyword list.
#' @examples 
#' create_keyword_set(keyword_set = list(keyword = list('stream discharge', 'discharge'), 
#'                                    keywordThesaurus = 'LTER Controlled Vocabulary'))
#'                                    
#' create_keyword_set(keyword_set = list(list(keyword = list('stream discharge', 'discharge'), 
#'                                         keywordThesaurus = 'LTER Controlled Vocabulary'),
#'                                    list(keyword = list('Sacramento River'))))                                    
#' @export
create_keyword_set <- function(keyword_set) {
  keywords <- list()
  if (length(keyword_set) < 1) {
    warning("Please supply at least one keyword")
  }
  keywords <- keyword_set
  
  return(keywords)
}
#' Add Keywords
#' @param parent_element A list representing the EML project or dataset.
#' @param keyword_metadata A named list or dataframe containing keyword elements: see \code{\link{create_keywords}} 
#' 
#' @export
#' 
add_keyword_set <- function(parent_element, keyword_metadata) {
  unique_thesaurus <- unique(keyword_metadata$keywordThesaurus)
  if (is.na(unique_thesaurus)) {
    keywords <- create_keyword_set(keyword_metadata$keyword)
    parent_element$keywordSet <- keywords
  } else {
    keywords <- list()
    add_keywords = function(unique_thesaurus){
      keywords <- add_keyword_set(keywords, 
                                  keyword_set = list(keyword = keyword_metadata$keyword[keyword_metadata$keywordThesaurus == unique_thesaurus],
                                                     keywordThesaurus = unique_thesaurus))
    }
    parent_element$keywordSet <- purrr::map(unique_thesaurus, add_keywords) %>% flatten()
  }
  return(parent_element)
  
}
