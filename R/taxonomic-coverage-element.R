#' @title Add Taxonomic Coverage 
#' @description Adds the taxonomic coverage information of a dataset based off of 
#' EML standards. The addition of taxonomic coverage is optional, however defaults 
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
#' @param taxon_id Optional. The taxonomic serial number provided by ITIS.
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
#' taxonomic_coverage <- add_taxonomic_coverage(CVPIA_common_species = "chinook")
#' 
#' taxonomic_coverage <- add_taxonomic_coverage(CVPIA_common_species = "delta_smelt")
#' 
#' taxonomic_coverage <- add_taxonomic_coverage(CVPIA_common_species = "white_sturgeon")
#' 
#' taxonomic_coverage <- add_taxonomic_coverage(CVPIA_common_species = "green_sturgeon")
#' 
#' taxonomic_coverage <- add_taxonomic_coverage(CVPIA_common_species = "steelhead")
#' 
#' taxonomic_coverage <- add_taxonomic_coverage(kingdom_value = "Animalia",
#'                        phylum_value = "Chordata",
#'                        class_value = "Mammalia",
#'                        order_value = "Carnivora",
#'                        family_value = "Felidae",
#'                        genus_value = "Panthera", 
#'                        species_value = "Panthera Leo",
#'                        common_name = "Lion", 
#'                        taxon_id = "183803")   
#'                                             
#' # To append this information to the dataset or project:                        
#'     add_coverage(parent_element = list(), geographic_description = "Description",
#'                  west_bounding_coordinate = "-160.594000", 
#'                  east_bounding_coordinate = "-134.104800",
#'                  north_bounding_coordinate = "71.238300", 
#'                  south_bounding_coordinate = "67.865000",
#'                  begin_date = "1980-01-01", 
#'                  end_date = "2010-12-31", 
#'                  taxonomic_coverage = taxonomic_coverage)
#' @export

add_taxonomic_coverage <- function(CVPIA_common_species = NULL,
                                   kingdom = "kingdom", kingdom_value,
                                   phylum = "phylum", phylum_value,
                                   class = "class", class_value,
                                   order = "order", order_value,
                                   family = "family", family_value,
                                   genus = "genus", genus_value, 
                                   species = "species", species_value,
                                   common_name, taxon_id = NULL) {
  
  if (is.null(CVPIA_common_species)) {
    required_arguments <- c("kingdom_value", "phylum_value", "class_value", "order_value",
                       "family_value", "genus_value", "species_value", "common_name")
    missing_argument_index <- which(c(missing(kingdom_value), missing(phylum_value),
                               missing(class_value), missing(order_value),
                               missing(family_value), missing(genus_value),
                               missing(species_value), missing(common_name)))
    
    if (length(missing_argument_index) > 0) {
      tax_error <- required_arguments[missing_argument_index][1]
      tax_error_message <- switch(tax_error,
                                 kingdom_value = "Please provide a kingdom.",
                                 phylum_value = "Please provide a phylum.",
                                 class_value = "Please provide a class.",
                                 order_value = "Please provide an order.", 
                                 family_value = "Please provide a family.",
                                 genus_value = "Please provide a genus.",
                                 species_value = "Please provide a species.",
                                 common_name = "Please provide a common name.")
      stop(tax_error_message, call. = FALSE)
    } 
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
  }
  if (is.null(taxon_id)) {
    message("No taxon id has been provided. This number can be found at ITIS.gov if you wish to append it.")
  } else {
    taxonomicCoverage$taxonomicClassification$taxonomicClassification$taxonomicClassification$taxonomicClassification$taxonomicClassification$taxonomicClassification$taxonomicClassification$taxonId <-
      list("provider" = "https://itis.gov",
            taxonId = taxon_id)
  }
  return(taxonomicCoverage)
}