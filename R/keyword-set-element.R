#' Creates Keyword Set Element
#' @description Creates keyword set according to EML standards.
#' @param keyword_metadata A named list or dataframe containing keywords and keywordThesuraus (if keyword is from a controlled vocabulary)
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
#' keyword_metadata <- list("keyword" = c("CVPIA", "dog", "shark", "cat"), 
#'                          keywordThesaurus = c(NA, "pet", "ocean", "pet"))
#' create_keyword_set(keyword_metadata)
#' 
#' keyword_metadata <- tibble("keyword" = c("CVPIA", "dog", "shark", "cat"), 
#'                            "keywordThesaurus" = c(NA, NA, NA, NA))                                   
#' create_keyword_set(keyword_metadata)
#'                                 
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
      rel_keywords = keyword_metadata %>%
        filter(is.na(keywordThesaurus)) %>%
        pull(keyword)
      keywords[[i]] = list(keyword = rel_keywords)
    } else {
      rel_keywords = keyword_metadata %>%
        filter((keywordThesaurus == rel_thesaurus)) %>%
        pull(keyword)
      keywords[[i]] = list(keyword = rel_keywords, keywordThesaurus = rel_thesaurus)
    } 
  }
  return(keywords)
}


#' Add Keywords
#' @param parent_element A list representing the EML project or dataset.
#' @param keyword_metadata A named list or dataframe containing keyword elements: see \code{\link{create_keywords}} 
#' 
#' @export
#' 
add_keyword_set <- function(parent_element, keyword_metadata) {
  parent_element$keywordSet <- create_keyword_set(keyword_metadata = keyword_metadata) 
  return(parent_element)
}





