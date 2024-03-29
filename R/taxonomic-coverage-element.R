#' @title Create Taxonomic Coverage 
#' @description Create the taxonomic coverage information of a dataset based off of 
#' EML standards. The addition of taxonomic coverage is (Optional), however defaults 
#' of chinook, sturgeon, smelt, and steelhead are provided. While single or 
#' multiple taxonomies can be applied, the full set of information must be 
#' provided if chosen to be included. 
#' @param CVPIA_common_species Use one of the following from the helper data 
#' \code{\link{CVPIA_common_species}}: "chinook", "delta_smelt", 
#' "white_sturgeon", "green_sturgeon", or "steelhead" to get pre-selected 
#' information from ITIS.
#' @param kingdom Kingdom level present.
#' @param kingdom_value The kingdom name. 
#' @param phylum Phylum level present.
#' @param phylum_value The phylum name.
#' @param class Class level present. 
#' @param class_value The class level name.
#' @param order Order level present.
#' @param order_value The order level name.
#' @param family Family level present.
#' @param family_value The family level name.
#' @param genus Genus level present. 
#' @param genus_value The genus level name. 
#' @param species Species level present.
#' @param species_value The species level name.
#' @param common_name The common name of the organism. 
#' @param taxon_id (Optional). The taxonomic serial number provided by ITIS.
#' @section CVPIA Common Species: 
#' The following frequently cited species are available for convenience: 
#'  * EMLaide::CVPIA_common_species$chinook - Oncorhynchus tshawytscha (ITIS: 161980)
#'  * EMLaide::CVPIA_common_species$steelhead - Oncorhynchus mykiss (ITIS: 161989)
#'  * EMLaide::CVPIA_common_species$delta_smelt - Hypomesus transpacificus (ITIS: 162032)
#'  * EMLaide::CVPIA_common_species$white_sturgeon - Acipenser transmontanus (ITIS:161068)
#'  * EMLaide::CVPIA_common_species$green_sturgeon - Acipenser medirostris (ITIS: 161067)
#' 
#' For further taxonomic coverage (i.e subkingdom, infrakingdom, etc.) on any of 
#' these species, you can visit: \href{https://www.itis.gov/}{ITIS}'s webpage for 
#' full coverage information.
#' 
#' @return Taxonomic coverage information. The function should be assigned to the 
#' name taxonomic_coverage to append it to the dataset or project. Example of how 
#' to incorporate it into the add_coverage function is seen below. 
#' @examples 
#' \dontrun{taxonomic_coverage <- create_taxonomic_coverage(CVPIA_common_species = "chinook")}
#' 
#' \dontrun{taxonomic_coverage <- create_taxonomic_coverage(CVPIA_common_species = "delta_smelt")}
#' 
#' \dontrun{taxonomic_coverage <- create_taxonomic_coverage(CVPIA_common_species = "white_sturgeon")}
#' 
#' \dontrun{taxonomic_coverage <- create_taxonomic_coverage(CVPIA_common_species = "green_sturgeon")}
#' 
#' \dontrun{taxonomic_coverage <- create_taxonomic_coverage(CVPIA_common_species = "steelhead")}
#' 
#' \dontrun{taxonomic_coverage <- create_taxonomic_coverage(kingdom_value = "Animalia",
#'                                                          phylum_value = "Chordata",
#'                                                          class_value = "Mammalia",
#'                                                          order_value = "Carnivora",
#'                                                          family_value = "Felidae",
#'                                                          genus_value = "Panthera", 
#'                                                          species_value = "Panthera Leo",
#'                                                          common_name = "Lion", 
#'                                                          taxon_id = "183803")}
#'                                             
#' # To append this information to the dataset or project:                        
#' \dontrun{create_coverage(geographic_description = "Description",
#'                          west_bounding_coordinate = "-160.594000", 
#'                          east_bounding_coordinate = "-134.104800",
#'                          north_bounding_coordinate = "71.238300", 
#'                          south_bounding_coordinate = "67.865000",
#'                          begin_date = "1980-01-01", 
#'                          end_date = "2010-12-31", 
#'                          taxonomic_coverage = taxonomic_coverage)}
#' @export
create_taxonomic_coverage <- function(CVPIA_common_species = NULL,
                                   kingdom = "kingdom", kingdom_value = NULL,
                                   phylum = "phylum", phylum_value = NULL,
                                   class = "class", class_value = NULL,
                                   order = "order", order_value = NULL,
                                   family = "family", family_value = NULL,
                                   genus = "genus", genus_value = NULL , 
                                   species = "species", species_value = NULL,
                                   common_name = NULL, taxon_id = NULL) {  
  
  if (is.null(CVPIA_common_species)) {
    
    kingdom_value <- kingdom_value
    phylum_value <- phylum_value
    class_value <- class_value
    order_value <- order_value
    family_value <- family_value
    genus_value <- genus_value
    species_value <- species_value
    common_name <- common_name
  } 
  common_species_index <- which(c(CVPIA_common_species == "chinook", 
                                  CVPIA_common_species == "steelhead",
                                  CVPIA_common_species == "delta_smelt", 
                                  CVPIA_common_species == "white_sturgeon",
                                  CVPIA_common_species == "green_sturgeon"))
  
  if (length(common_species_index) > 0) {
    taxonomicCoverage <- EMLaide::CVPIA_common_species[[common_species_index]]
    
  } else {
    kingdom_value <- "Animalia"
    phylum_value <- "Chordata"
  
    taxonomicCoverage <-
      list(taxonomicClassification = 
             list(taxonRankName = kingdom,
                  taxonRankValue = kingdom_value,
                  taxonomicClassification =
                    list(taxonRankName = phylum,
                         taxonRankValue = phylum_value,
                         taxonomicClassification =
                           list(taxonRankName = class,
                                taxonRankValue = class_value,
                                taxonomicClassification =
                                  list(taxonRankName = order,
                                       taxonRankValue = order_value,
                                       taxonomicClassification =
                                         list(taxonRankName = family,
                                              taxonRankValue = family_value,
                                              taxonomicClassification =
                                                list(taxonRankName = genus,
                                                     taxonRankValue = genus_value,
                                                     taxonomicClassification =
                                                       list(taxonRankName = species,
                                                            taxonRankValue = species_value,
                                                            commonName = common_name))))))))
  if (is.null(taxon_id)) {
    message("No taxon id has been provided. This number can be found at ITIS.gov if you wish to append it.")
  } else {
    taxonomicCoverage$taxonomicClassification$taxonomicClassification$taxonomicClassification$taxonomicClassification$taxonomicClassification$taxonomicClassification$taxonomicClassification$taxonId <-
      list("provider" = "https://itis.gov",
            taxonId = taxon_id)
  }
  }
  return(taxonomicCoverage)
}

#' Add Taxonomic Coverage
#' @description Formats the taxonomic coverage elements in a nested list to easily be added as a parameter to the `add_coverage()` function. 
#' @param taxonomic_metadata see \code{\link{create_taxonomic_coverage}} 
#' @return A complete list describing taxonomic coverage that can be added to the coverage list using `add_coverage()`
#' @examples 
#' \dontrun{
#' taxonomic_metadata <- list(CVPIA_common_species = c("chinook", "steelhead"), common_name = c(NA, NA), 
#'                            kingdom = c(NA, NA), phylum = c(NA, NA), class = c(NA, NA), 
#'                            order = c(NA, NA), family = c(NA, NA), genus = c(NA, NA), species = c(NA, NA), 
#'                            taxon_id = c(NA, NA))}
#' \dontrun{                           
#' add_taxonomic_coverage(taxonomic_metadata)
#' }
#' @export
add_taxonomic_coverage <- function(taxonomic_metadata){
  if(is.null(taxonomic_metadata)) {
    return(NULL)
  }
  purrr::pmap(taxonomic_metadata, create_taxonomic_coverage)
  
}
