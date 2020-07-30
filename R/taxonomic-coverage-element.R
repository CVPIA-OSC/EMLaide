#' @title Add Taxonomic Coverage 
#' @description Adds the taxonomic coverage information of a dataset based off of EML standards. The addition of taxonomic coverage is optional, however defaults of chinook, sturgeon, smelt, and steelhead are provided. While single or multiple taxonomies can be applied, the full set of information must be provided if chosen to be included. 
#' @param CVPIA_common_species Use one of the following: "chinook", "delta_smelt", "white_sturgeon", "green_sturgeon", or "steelhead" to get pre-selected information from ITIS
#' @param kingdom Kingdom level present
#' @param kingdom_value The kingdom name. 
#' @param phylum Phylum level present
#' @param phylum_value The phylum name.
#' @param class Class level present 
#' @param class_value The class level name.
#' @param order Order level present
#' @param order_value The order level name.
#' @param family Family level present
#' @param family_value The family level name.
#' @param genus Genus level present 
#' @param genus_value The genus level name. 
#' @param species Species level present
#' @param species_value The species level name.
#' @param common_name The common name of the organism 
#' @section CVPIA Common Species: 
#' By using a CVPIA common species, the appropriate taxonomy is appended based off of the Integrated Taxonomic Information System (ITIS). 
#' 
#' Select "chinook" to append the taxonomy of Oncorhynchus tshawytscha (Also known as Chinook Salmon or King Salmon) from the ITIS database. The correspoinding ITIS taxonomic id number is 161980.
#' 
#' Select "delta_smelt" to append the taxonomy of Hypomesus transpacificus (Also known as Delta Smelt) from the ITIS database. The correspoinding ITIS taxonomic id number is 162032. 
#' 
#' Select "white_sturgeon" to append the taxonomy of Acipenser transmontanus (Also known as White Sturgeon) from the ITIS database. The correspoinding ITIS taxonomic id number is 161068.
#' 
#' Select "green_sturgeon" to append the taxonomy of Acipenser medirostris (Also known as Green Sturgeon) from the ITIS database. The correspoinding ITIS taxonomic id number is 161067.
#' 
#' Select "steelhead" to append the taxonomy of Oncorhynchus mykiss (Also known as Steelhead, Rainbow Trout, or Redband Trout) from the ITIS database. The correspoinding ITIS taxonomic id number is 161989.
#' 
#' For further taxonomic coverage (i.e subkingdom, infrakingdom, etc.) on any of these species, you can visit: \href{https://www.itis.gov/}{ITIS}'s webpage for full coverage information.
#' 
#' @return Taxonomic coverage information. The function should be assigned to the name taxonomic_coverage to append it to the dataset or project. Example of how to incorporate it into the add_coverage function is seen below. 
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
#'                        common_name = "Lion")   
#'                                             
#' To append this information to the dataset or project:                        
#'     add_coverage(parent_element = list(), geographic_description = "Description",
#                   west_bounding_coordinate = "-160.594000", east_bounding_coordinate = "-134.104800",
#                   north_bounding_coordinate = "71.238300", south_bounding_coordinate = "67.865000",
#                   begin_date = "1980-01-01", end_date = "2010-12-31", taxonomic_coverage = taxonomic_coverage)
#' @export

add_taxonomic_coverage <- function(CVPIA_common_species = NULL,
                                   kingdom = "kingdom", kingdom_value,
                                   phylum = "phylum", phylum_value,
                                   class = "class", class_value,
                                   order = "order", order_value,
                                   family = "family", family_value,
                                   genus = "genus", genus_value, 
                                   species = "species", species_value,
                                   common_name) {
  
  if (is.null(CVPIA_common_species)) {
    if (missing(kingdom_value)) {stop("Please provide a kingdom.", call. = FALSE)}
    kingdom_value <- kingdom_value
    if (missing(phylum_value)) {stop("Please provide a phylum.", call. = FALSE)}
    phylum_value <- phylum_value
    if (missing(class_value)) {stop("Please provide a class.", call. = FALSE)}
    class_value <- class_value
    if (missing(order_value)) {stop("Please provide an order.", call. = FALSE)}
    order_value <- order_value
    if (missing(family_value)) {stop("Please provide a family.", call. = FALSE)}
    family_value <- family_value
    if (missing(genus_value)) {stop("Please provide a genus.", call. = FALSE)}
    genus_value <- genus_value
    if (missing(species_value)) {stop("Please provide a species.", call. = FALSE)}
    species_value <- species_value
    if (missing(common_name)) {stop("Please provide a common name.", call. = FALSE)}
    common_name <- common_name
    
  } else {
    
    kingdom_value <- "Animalia"
    phylum_value <- "Chordata"
    
    if (CVPIA_common_species == "chinook") {
      class_value <- "Teleostei"
      order_value <- "Salmoniformes"
      family_value <- "Salmonidae"
      genus_value <- "Oncorhynchus"
      species_value <- "Oncorhynchus tshawytscha"
      common_name <- "Chinook Salmon"

    }
    
    if (CVPIA_common_species == "steelhead") {
      class_value <- "Teleostei"
      order_value <- "Salmoniformes"
      family_value <- "Salmonidae"
      genus_value <- "Oncorhynchus"
      species_value <- "Oncorhynchus mykiss"
      common_name <- "Steelhead Trout"
    }
    
    if (CVPIA_common_species == "delta_smelt") {
      class_value <- "Teleostei"
      order_value <- "Osmeriformes"
      family_value <- "Osmeridae"
      genus_value <- "Hypomesus"
      species_value <- "Hypomesus transpacificus"
      common_name <- "Delta Smelt"
    }
    
    if (CVPIA_common_species == "white_sturgeon") {
      class_value <- "Chondrostei"
      order_value <- "Acipenseriformes"
      family_value <- "Acipenseridae"
      genus_value <- "Acipenser"
      species_value <- "Acipenser transmontanus"
      common_name <- "White Sturgeon"
    }
    
    if (CVPIA_common_species == "green_sturgeon") {
      class_value <- "Chondrostei"
      order_value <- "Acipenseriformes"
      family_value <- "Acipenseridae"
      genus_value <- "Acipenser"
      species_value <- "Acipenser medirostris"
      common_name <- "Green Sturgeon"
    }
  } 
  
  taxonomicCoverage <-
    list(TaxonomicClassification = 
           list(TaxonRankName = kingdom,
                TaxonRankValue = kingdom_value,
                TaxonomicClassification =
                  list(TaxonRankName = phylum,
                       TaxonRankValue = phylum_value,
                       TaxonomicClassification =
                         list(TaxonRankName = class,
                              TaxonRankValue = class_value,
                              TaxonomicClassification =
                                list(TaxonRankName = order,
                                     TaxonRankValue = order_value,
                                     TaxonomicClassification =
                                       list(TaxonRankName = family,
                                            TaxonRankValue = family_value,
                                            TaxonomicClassification =
                                              list(TaxonRankName = genus,
                                                   TaxonRankValue = genus_value,
                                                   TaxonomicClassification =
                                                     list(TaxonRankName = species,
                                                          TaxonRankValue = species_value,
                                                          commonName = common_name))))))))


}