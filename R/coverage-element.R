#' @title Add Coverage Element
#' @description Adds coverage element #TODO
#' @param parent_element A list representing the EML project or dataset
#' @param geographic_description #TODO
#' @param west_bounding_coordinate #TODO
#' @param east_bounding_coordinate #TODO
#' @param north_bounding_coordinate #TODO
#' @param south_bounding_coordinate #TODO
#' @param begin_date #TODO
#' @param end_date #TODO
#' @return The dataset or project with coverage information appended
#' @examples #TODO
#' @export
add_coverage <- function(parent_element, geographic_description, west_bounding_coordinate,
                         east_bounding_coordinate, north_bounding_coordinate,
                         south_bounding_coordinate, begin_date, end_date) {
  
  parent_element$coverage <- list(geographicCoverage = 
                                    list(geographicDescription = geographic_description,
                                         boundingCoordinates = 
                                           list(westBoundingCoordinate = west_bounding_coordinate,
                                                eastBoundingCoordinate = east_bounding_coordinate,
                                                northBoundingCoordinate = north_bounding_coordinate,
                                                southBoundingCoordinate = south_bounding_coordinate)),
                                  temporalCoverage = list(rangeOfDates = list(beginDate = list(calendarDate = begin_date),
                                                                         endDate = list(calendarDate = end_date)))
                                  )
}




# <coverage>
#   <geographicCoverage>
#     <geographicDescription>North Slope drainage basin: Bounding box encompasses 42 drainage basins totaling the North Slope drainage basin, Alaska, USA.</geographicDescription>
#     <boundingCoordinates>
#       <westBoundingCoordinate>-160.594000</westBoundingCoordinate>
#       <eastBoundingCoordinate>-134.104800</eastBoundingCoordinate>
#       <northBoundingCoordinate>71.238300</northBoundingCoordinate>
#       <southBoundingCoordinate>67.865000</southBoundingCoordinate>
#     </boundingCoordinates>
#   </geographicCoverage>
#   <temporalCoverage>
#     <rangeOfDates>
#       <beginDate>
#         <calendarDate>1980-01-01</calendarDate>
#       </beginDate>
#       <endDate>
#         <calendarDate>2010-12-31</calendarDate>
#       </endDate>
#     </rangeOfDates>
#   </temporalCoverage>
# </coverage>