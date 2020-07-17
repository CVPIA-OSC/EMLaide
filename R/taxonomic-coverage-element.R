#' @title Add Taxonomic Coverage 
#' @description Adds the taxonomic coverage information of a dataset based off of EML standards. The addition of taxonomic coverage is optional, however defaults of chinook, sturgeon, smelt, and steelhead are provided. While single or multiple taxonomies can be applied, the full set of information must be provided if chosen to be included. 
#' @param parent_element A list representing the EML project or dataset
#' @param CVPIA_common_species A list of common CVPIA species can be set as the default species so that taxonomic coverage does not need to be inputted in manually. These species include: chinook, steelhead, smelt, and sturgeon. 
#' @param kingdom Kingdom level present
#' @param kingdom_value The kingdom level of the taxonomy. 
#' @param phylum Phylum level present
#' @param phylum_value The phylum level of the taxonomy.
#' @param class Class level present 
#' @param class_value The class level of the taxonomy.
#' @param order Order level present
#' @param order_value The order level of the taxonomy.
#' @param family Family level present
#' @param family_value The family level of the taxonomy.
#' @param genus Genus level present 
#' @param genus_value The genus level of the taxonomy. 
#' @param species Species level present
#' @param species_value The species level of the taxonomy.
#' @param common_name The common name of the organism 
#' @return The dataset or project with taxonomic coverage information appended
#' @examples 
#' add_taxonomic_coverage(parent_element = list(), CVPIA_common_species = "chinook")
#' 
#' add_taxonomic_coverage(parent_element = list(), CVPIA_common_species = "smelt")
#' 
#' add_taxonomic_coverage(parent_element = list(), CVPIA_common_species = "sturgeon")
#' 
#' add_taxonomic_coverage(parent_element = list(), CVPIA_common_species = "steelhead")
#' 
#' add_taxonomic_coverage(parent_element = list(),
#'                        kingdom = "KINGDOM", kingdom_value = "Animalia",
#'                        phylum = "PHYLUM", phylum_value = "Chordata",
#'                        class = "CLASS", class_value = "Mammalia",
#'                        order = "ORDER", order_value = "Carnivora",
#'                        family = "FAMILY", family_value = "Felidae",
#'                        genus = "GENUS", genus_value = "Panthera", 
#'                        species = "SPECIES", species_value = "Panthera Leo",
#'                        common_name = "Lion")
#' @export

#taxon id provider??
#general taxonomic coverage?? 
#taxonomic system??
#references?? 
add_taxonomic_coverage <- function(parent_element, CVPIA_common_species = NULL,
                                   kingdom = "KINGDOM", kingdom_value,
                                   phylum = "PHYLUM", phylum_value,
                                   class = "CLASS", class_value,
                                   order = "ORDER", order_value,
                                   family = "FAMILY", family_value,
                                   genus = "GENUS", genus_value, 
                                   species = "SPECIES", species_value,
                                   common_name) {
  
  if (is.null(CVPIA_common_species)) {
    kingdom_value <- kingdom_value
    phylum_value <- phylum_value
    class_value <- class_value
    order_value <- order_value
    family_value <- family_value
    genus_value <- genus_value
    species_value <- species_value
    commonName <- common_name
    
  }else{
  
  if (CVPIA_common_species == "chinook") {
    kingdom_value <- "Animalia"
    phylum_value <- "Chordata"
    class_value <- "Teleostei"
    order_value <- "Salmoniformes"
    family_value <- "Salmonidae"
    genus_value <- "Oncorhynchus"
    species_value <- "Oncorhynchus tshawytscha"
    commonName <- "Chinook Salmon"
  }
  
  if (CVPIA_common_species == "steelhead") {
    kingdom_value <- "Animalia"
    phylum_value <- "Chordata"
    class_value <- "Teleostei"
    order_value <- "Salmoniformes"
    family_value <- "Salmonidae"
    genus_value <- "Oncorhynchus"
    species_value <- "Oncorhynchus mykiss"
    commonName <- "Steelhead Trout"
  }
  
  if (CVPIA_common_species == "delta_smelt") {
    kingdom_value <- "Animalia"
    phylum_value <- "Chordata"
    class_value <- "Teleostei"
    order_value <- "Osmeriformes"
    family_value <- "Osmeridae"
    genus_value <- "Hypomesus"
    species_value <- "Hypomesus transpacificus"
    commonName <- "Delta Smelt"
  }
  
  if (CVPIA_common_species == "white_sturgeon") {
    kingdom_value <- "Animalia"
    phylum_value <- "Chordata"
    class_value <- "Chondrostei"
    order_value <- "Acipenseriformes"
    family_value <- "Acipenseridae"
    genus_value <- "Acipenser"
    species_value <- "Acipenser transmontanus"
    commonName <- "White Sturgeon"
  }
  
  if (CVPIA_common_species == "green_sturgeon") {
    kingdom_value <- "Animalia"
    phylum_value <- "Chordata"
    class_value <- "Chondrostei"
    order_value <- "Acipenseriformes"
    family_value <- "Acipenseridae"
    genus_value <- "Acipenser"
    species_value <- "Acipenser medirostris"
    commonName <- "Green Sturgeon"
  }
} 
 
    parent_element$taxonomicCoverage <-
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
    
  return(parent_element)
}