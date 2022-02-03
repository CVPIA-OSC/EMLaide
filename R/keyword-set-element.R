#' Creates Keyword Set Element
#' @description Takes in keywords and associated keyword thesaurus and creates a keyword set according to EML standards.
#' @param keyword_metadata A  dataframe containing `keyword` and `keywordThesuraus` (if keyword is from a controlled vocabulary). 
#' The dataframe must have a `keyword` column and a `keywordThesaurus` column. If keywords are not from a controlled vocabulary please leave `keywordThesaurus` blank or NA.
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
#' @return The keyword list that can be appended to a dataset or project list.
#' @examples 
#' keyword_metadata <- dplyr::tibble(keyword = c("CVPIA", "dog", "shark", "cat"), 
#'                                   keywordThesaurus = c(NA, "pet", "ocean", "pet"))
#' create_keyword_set(keyword_metadata)
#' 
#' keyword_metadata <- dplyr::tibble(keyword = c("CVPIA", "dog", "shark", "cat"), 
#'                                   keywordThesaurus = c(NA, NA, NA, NA))                                   
#' create_keyword_set(keyword_metadata)
#' @importFrom magrittr %>%                                
#' @export
create_keyword_set <- function(keyword_metadata) {
  
  if (length(keyword_metadata$keyword) < 1) {
    warning("Please supply at least one keyword")
  }
  unique_thesaurus <- unique(keyword_metadata$keywordThesaurus)
  keywords = list()
  for (i in 1:length(unique_thesaurus)){
    rel_thesaurus = unique_thesaurus[i]
    if (is.na(rel_thesaurus)) {
      filtered_keywords = keyword_metadata %>%
        dplyr::filter(is.na(keywordThesaurus)) %>%
        dplyr::pull(keyword)
      keywords[[i]] = list(keyword = filtered_keywords)
    } else {
      filtered_keywords = keyword_metadata %>%
        dplyr::filter((keywordThesaurus == rel_thesaurus)) %>%
        dplyr::pull(keyword)
      keywords[[i]] = list(keyword = filtered_keywords, keywordThesaurus = rel_thesaurus)
    } 
  }
  return(keywords)
}


#' Add Keywords
#' @description Adds the keyword metadata elements to a dataset list according to EML standards. 
#' @param parent_element A list representing the EML project or dataset.
#' @param keyword_metadata A named list or dataframe containing keyword elements: see \code{\link{create_keyword_set}}
#' @return The dataset list or project with keyword information appended.
#' @examples
#' keyword_metadata <- dplyr::tibble(keyword = c("Sacramento River", 
#'                                               "Salmonid Habitat Restoration Projects", 
#'                                               "Effectiveness Monitoring", 
#'                                               "Pacific Salmon", 
#'                                               "CVPIA"), 
#'                                   keywordThesaurus = c(NA, NA, NA, NA, NA))
#' 
#' dataset <- list() %>%
#'      add_keyword_set(keyword_metadata)
#' dataset
#' @export
#' 
add_keyword_set <- function(parent_element, keyword_metadata) {
  parent_element$keywordSet <- create_keyword_set(keyword_metadata = keyword_metadata) 
  return(parent_element)
}





