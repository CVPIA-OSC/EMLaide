#' @title Add Taxonomic Coverage 
#' @description Adds the taxonomic coverage information of a dataset based off of EML standards. The addition of taxonomic coverage is optional, however defaults of chinook, sturgeon, smelt, and steelhead are provided. Multiple Taxonomies can be applied. 
#' @param parent_element A list representing the EML project or dataset
#' @param default_species A list of common CVPIA species can be set as the default species so that taxonomic coverage does not need to be inputted in manually. These species include: chinook, steelhead, smelt, and sturgeon. 
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
#' @examples TODO
#' @export

#taxon id provider??
add_taxonomic_coverage <- function(parent_element, default_species = NULL,
                                   kingdom = "KINGDOM", kingdom_value = NULL,
                                   phylum = "PHYLUM", phylum_value = NULL,
                                   class = "CLASS", class_value = NULL,
                                   order = "ORDER", order_value = NULL,
                                   family = "FAMILY", family_value = NULL,
                                   genus = "GENUS", genus_value = NULL, 
                                   species = "SPECIES", species_value = NULL,
                                   common_name = NULL ) {
if(is.null(default_species)){
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
  
  
}else {
  if (default_species == "chinook"){
    parent_element$taxonomicCoverage <-
      list(TaxonomicClassification = 
             list(TaxonRankName = kingdom,
                  TaxonRankValue = "Animalia",
                  TaxonomicClassification =
                    list(TaxonRankName = phylum,
                         TaxonRankValue = "Chordata",
                         TaxonomicClassification =
                           list(TaxonRankName = class,
                                TaxonRankValue = "Actinopterygii",
                                TaxonomicClassification =
                                  list(TaxonRankName = order,
                                       TaxonRankValue = "Salmoniformes",
                                       TaxonomicClassification =
                                         list(TaxonRankName = family,
                                              TaxonRankValue = "Salmonidae",
                                              TaxonomicClassification =
                                                list(TaxonRankName = genus,
                                                     TaxonRankValue = "Oncorhynchus",
                                                     TaxonomicClassification =
                                                       list(TaxonRankName = species,
                                                            TaxonRankValue = "Oncorhynchus tshawytscha",
                                                            commonName = "Chinook Salmon"))))))))
    
  }
  if (default_species == "steelhead"){
    parent_element$taxonomicCoverage <-
      list(TaxonomicClassification = 
             list(TaxonRankName = kingdom,
                  TaxonRankValue = "Animalia",
                  TaxonomicClassification =
                    list(TaxonRankName = phylum,
                         TaxonRankValue = "Chordata",
                         TaxonomicClassification =
                           list(TaxonRankName = class,
                                TaxonRankValue = "Osteichthyes",
                                TaxonomicClassification =
                                  list(TaxonRankName = order,
                                       TaxonRankValue = "Salmoniformes",
                                       TaxonomicClassification =
                                         list(TaxonRankName = family,
                                              TaxonRankValue = "Salmonidae",
                                              TaxonomicClassification =
                                                list(TaxonRankName = genus,
                                                     TaxonRankValue = "Oncorhynchus",
                                                     TaxonomicClassification =
                                                       list(TaxonRankName = species,
                                                            TaxonRankValue = "Oncorhynchus mykiss",
                                                            commonName = "Steelhead Trout"))))))))
  }
  if (default_species == "sturgeon") {
    parent_element$taxonomicCoverage <-
      list(TaxonomicClassification = 
             list(TaxonRankName = kingdom,
                  TaxonRankValue = "Animalia",
                  TaxonomicClassification =
                    list(TaxonRankName = phylum,
                         TaxonRankValue = "Chordata",
                         TaxonomicClassification =
                           list(TaxonRankName = class,
                                TaxonRankValue = "Actinopterygii",
                                TaxonomicClassification =
                                  list(TaxonRankName = order,
                                       TaxonRankValue = "Acipenseriformes",
                                       TaxonomicClassification =
                                         list(TaxonRankName = family,
                                              TaxonRankValue = "Acipenseridae",
                                              TaxonomicClassification =
                                                list(TaxonRankName = genus,
                                                     TaxonRankValue = "Acipenser",
                                                     TaxonomicClassification =
                                                       list(TaxonRankName = species,
                                                            TaxonRankValue = "TODO!!",
                                                            commonName = "Sturgeon"))))))))
  }
  if (default_species == "smelt") {
    parent_element$taxonomicCoverage <-
      list(TaxonomicClassification = 
             list(TaxonRankName = kingdom,
                  TaxonRankValue = "Animalia",
                  TaxonomicClassification =
                    list(TaxonRankName = phylum,
                         TaxonRankValue = "Chordata",
                         TaxonomicClassification =
                           list(TaxonRankName = class,
                                TaxonRankValue = "Actinopterygii",
                                TaxonomicClassification =
                                  list(TaxonRankName = order,
                                       TaxonRankValue = "Osmeriformes",
                                       TaxonomicClassification =
                                         list(TaxonRankName = family,
                                              TaxonRankValue = "Osmeridae",
                                              TaxonomicClassification =
                                                list(TaxonRankName = genus,
                                                     TaxonRankValue = "TODO!!",
                                                     TaxonomicClassification =
                                                       list(TaxonRankName = species,
                                                            TaxonRankValue = "TODO!!",
                                                            commonName = "Smelt"))))))))
    
  }

  
}
  
  
  
  
  
  
  
  
  
}