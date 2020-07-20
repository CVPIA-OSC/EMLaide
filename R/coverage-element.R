#' @title Add Coverage Element
#' @description Adds the coverage information of a dataset based off of EML standards.
#' @param parent_element A list representing the EML project or dataset
#' @param geographic_description A description of the locations	of research sites and areas related to the data.
#' @param west_bounding_coordinate The west cardinality limit 
#' @param east_bounding_coordinate The east cardinality limit.
#' @param north_bounding_coordinate The north cardinality limit.
#' @param south_bounding_coordinate The south cardinality limit.
#' @param begin_date The starting date for the dataset or project. Dates must be provided in ISO 8601 format, YYYY-MM-DD.
#' @param end_date The projected or actual end date for the dataset or project. Dates must be provided in ISO 8601 format, YYYY-MM-DD.
#' @param taxonomic_coverage Addition of taxonomic coverage is optional. Can be appended using the add_taxonomic_coverage function. Assign this function a name and then set it as the taxonomic_coverage parameter input. An example is given down below.  
#' @return The dataset or project with coverage information appended
#' @examples 
#' add_coverage(parent_element = list(), geographic_description = "North Slope drainage basin:
#'              Bounding box encompasses 42 drainage basins totaling the North Slope drainage basin, Alaska, USA.",
#'              west_bounding_coordinate = "-160.594000", east_bounding_coordinate = "-134.104800",
#'              north_bounding_coordinate = "71.238300", south_bounding_coordinate = "67.865000", 
#'              begin_date = "1980-01-01", end_date = "2010-12-31")
#'              
#' taxonomic_coverage <- add_taxonomic_coverage(CVPIA_common_species = "chinook")
#' add_coverage(parent_element = list(), geographic_description = "Description",
#               west_bounding_coordinate = "-160.594000", east_bounding_coordinate = "-134.104800",
#               north_bounding_coordinate = "71.238300", south_bounding_coordinate = "67.865000",
#               begin_date = "1980-01-01", end_date = "2010-12-31", taxonomic_coverage = taxonomic_coverage) 
#' @export
add_coverage <- function(parent_element, geographic_description, west_bounding_coordinate,
                         east_bounding_coordinate, north_bounding_coordinate,
                         south_bounding_coordinate, begin_date, end_date, taxonomic_coverage = NULL) {
  
  if (missing(geographic_description)) {stop("Please supply a brief description of the locations of research sites and areas related to this dataset.", call. = FALSE)}
  if (missing(west_bounding_coordinate)) {stop("Please supply the west cardinality limit if applicable.", call. = FALSE)}
  if (missing(east_bounding_coordinate)) {stop("Please supply the east cardinality limit if applicable.", call. = FALSE)}
  if (missing(north_bounding_coordinate)) {stop("Please supply the north cardinality limit if applicable.", call. = FALSE)}
  if (missing(south_bounding_coordinate)) {stop("Please supply the south cardinality limit if applicable.", call. = FALSE)}
  if (missing(begin_date)) {stop("Please suppply the starting date of this project.", call. = FALSE)}
  if (missing(end_date)) {stop("Please supply the end or projected end date for this project.", call. = FALSE)}

  
  parent_element$coverage <- list(geographicCoverage = 
                                    list(geographicDescription = geographic_description,
                                         boundingCoordinates = 
                                           list(westBoundingCoordinate = west_bounding_coordinate,
                                                eastBoundingCoordinate = east_bounding_coordinate,
                                                northBoundingCoordinate = north_bounding_coordinate,
                                                southBoundingCoordinate = south_bounding_coordinate)),
                                  temporalCoverage = list(rangeOfDates = list(beginDate = list(calendarDate = begin_date),
                                                                         endDate = list(calendarDate = end_date))))
                                  
  if (!is.null(taxonomic_coverage)) {
    parent_element$coverage$taxonomicCoverage = taxonomic_coverage
  }
  return(parent_element)
}




