#' @title Add Taxonomic coverage 
#' @description TODO
#' @param parent_element
#' @param default_species TODO
#' @param kingdom TODO
#' @param kingdom_value TODO
#' @param phylum TODO
#' @param phylum_value TODO
#' @param class TODO
#' @param class_value TODO
#' @param order TODO
#' @param order_value TODO
#' @param family TODO
#' @param family_value TODO
#' @param genus TODO
#' @param genus_value TODO 
#' @param species TODO
#' @param species_value TODO
#' @param common_name TODO
#' @return TODO
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