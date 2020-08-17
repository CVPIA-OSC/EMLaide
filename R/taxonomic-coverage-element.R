#' @title Add Taxonomic Coverage 
#' @description Adds the taxonomic coverage information of a dataset based off of 
#' EML standards. The addition of taxonomic coverage is optional, however defaults 
#' of chinook, sturgeon, smelt, and steelhead are provided. While single or 
#' multiple taxonomies can be applied, the full set of information must be 
#' provided if chosen to be included. 
#' @param CVPIA_common_species Use one of the following: "chinook", "delta_smelt", 
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
#' By using a CVPIA common species, the appropriate taxonomy is appended based off 
#' of the Integrated Taxonomic Information System (ITIS). 
#' 
#' Select "chinook" to append the taxonomy of Oncorhynchus tshawytscha 
#' (Also known as Chinook Salmon or King Salmon) from the ITIS database. 
#' The correspoinding ITIS taxonomic id number is 161980.
#' 
#' Select "delta_smelt" to append the taxonomy of Hypomesus transpacificus 
#' (Also known as Delta Smelt) from the ITIS database. The correspoinding ITIS 
#' taxonomic id number is 162032. 
#' 
#' Select "white_sturgeon" to append the taxonomy of Acipenser transmontanus 
#' (Also known as White Sturgeon) from the ITIS database. The correspoinding ITIS 
#' taxonomic id number is 161068.
#' 
#' Select "green_sturgeon" to append the taxonomy of Acipenser medirostris (Also
#' known as Green Sturgeon) from the ITIS database. The correspoinding ITIS 
#' taxonomic id number is 161067.
#' 
#' Select "steelhead" to append the taxonomy of Oncorhynchus mykiss 
#' (Also known as Steelhead, Rainbow Trout, or Redband Trout) from the ITIS database. 
#' The correspoinding ITIS taxonomic id number is 161989.
#' 
#' For further taxonomic coverage (i.e subkingdom, infrakingdom, etc.) on any of 
#' these species, you can visit: \href{https://www.itis.gov/}{ITIS}'s webpage for 
#' full coverage information.
#' 
#' @return Taxonomic coverage information. The function should be assigned to the 
#' name taxonomic_coverage to append it to the dataset or project. Example of how 
#' to incorporate it into the add_coverage function is seen below. 
#' @examples 
#' taxonomic_coverage <- add_taxonomic_coverage(CVPIA_common_species = cvpiaEDIutils::CVPIA_common_species$chinook)
#' 
#' taxonomic_coverage <- add_taxonomic_coverage(CVPIA_common_species = cvpiaEDIutils::CVPIA_common_species$delta_smelt)
#' 
#' taxonomic_coverage <- add_taxonomic_coverage(CVPIA_common_species = cvpiaEDIutils::CVPIA_common_species$white_sturgeon)
#' 
#' taxonomic_coverage <- add_taxonomic_coverage(CVPIA_common_species = cvpiaEDIutils::CVPIA_common_species$green_sturgeon)
#' 
#' taxonomic_coverage <- add_taxonomic_coverage(CVPIA_common_species = cvpiaEDIutils::CVPIA_common_species$steelhead)
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
#' To append this information to the dataset or project:                        
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
      taxon_id <- "161980"
    }
    
    if (CVPIA_common_species == "steelhead") {
      class_value <- "Teleostei"
      order_value <- "Salmoniformes"
      family_value <- "Salmonidae"
      genus_value <- "Oncorhynchus"
      species_value <- "Oncorhynchus mykiss"
      common_name <- "Steelhead Trout"
      taxon_id <- "161989"
    }
    
    if (CVPIA_common_species == "delta_smelt") {
      class_value <- "Teleostei"
      order_value <- "Osmeriformes"
      family_value <- "Osmeridae"
      genus_value <- "Hypomesus"
      species_value <- "Hypomesus transpacificus"
      common_name <- "Delta Smelt"
      taxon_id <- "162032"
    }
    
    if (CVPIA_common_species == "white_sturgeon") {
      class_value <- "Chondrostei"
      order_value <- "Acipenseriformes"
      family_value <- "Acipenseridae"
      genus_value <- "Acipenser"
      species_value <- "Acipenser transmontanus"
      common_name <- "White Sturgeon"
      taxon_id <- "161068"
    }
    
    if (CVPIA_common_species == "green_sturgeon") {
      class_value <- "Chondrostei"
      order_value <- "Acipenseriformes"
      family_value <- "Acipenseridae"
      genus_value <- "Acipenser"
      species_value <- "Acipenser medirostris"
      common_name <- "Green Sturgeon"
      taxon_id <- 161067
    }
  } 
  
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
    message("No taxon id has been provided. This number can be found at ITIS.gov if you wish to append it.", call. = FALSE )
  } else {
    taxonomicCoverage$taxonomicClassification$taxonomicClassification$taxonomicClassification$taxonomicClassification$taxonomicClassification$taxonomicClassification$taxonomicClassification$taxonId <-
      list("provider" = "https://itis.gov",
            taxonId = taxon_id)
  }
  return(taxonomicCoverage)
}