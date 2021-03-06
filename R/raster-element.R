#' Create Raster Element 
#' @title Create Raster Element
#' @description This function creates all the required elements for a \code{spatialRaster} section of
#' an ELM document. 
#' @param parent_element A list to append \code{spatialRaster} to 
#' @param file_name The name of your raster file
#' @param file_description A short description of your file
#' @param attribute_list A list of all the columns on your datatable. 
#' Use \code{\link{create_attribute}} to generate this attribute list and see the 
#' documentation for \code{\link{create_attribute}} for more information. 
#' @param physical A list of the physical descriptions of your file. Use 
#' \code{\link{create_physical}} to generate this physical list and see the 
#' documentation for \code{\link{create_physical}} for more information. 
#' @param spatial_reference This is the name of a predefined coordinate system. 
#' The acceptable values for \code{spatial_reference} can be found at 
#' \href{https://eml.ecoinformatics.org/schema/index.html}{EDI Schema}.
#' @param horizontal_accuracy  The accuracy of horizontal locational measurements within the data.
#' @param vertical_accuracy The accuracy of vertical locational measurements within the data. 
#' @param cell_size_x The width of the cell in the x direction.
#' @param cell_size_y The width of the cell in the y direction. 
#' @param number_of_bands The number of bands in the image. 
#' @param raster_origin  The corner location of the pixel it should have the 
#' minimum x and y values
#' @param rows Max number of raster object along the y-axis
#' @param columns Max number of raster objects along the x-axis
#' @param verticals Max number of raster objects along the z-axis
#' @param cell_geometry Geometric representation of the cell's content
#'
#' @return A list that contains all the required elements of the spatialRaster 
#' section of an EML document. 
#'
#' @examples
#' \dontrun{
#' create_raster(file_name = "Rasterfiles.tif" ,
#'               file_description = "A Raster File",
#'               attribute_list =  create_attribute(attribute_name = "Yrs", attribute_label = "Years", 
#'                                               attribute_definition = "Calendar year of the observation from years 1990 - 2010.", 
#'                                               storage_type = EMLaide::storage_type$date, 
#'                                               measurement_scale = EMLaide::measurement_scale$dateTime, 
#'                                               date_time_format = "YYYY",
#'                                               date_time_precision = "1", minimum = "1993", maximum = "2003"),
#'               physical = create_physical("Rasterfiles.tif"),
#'               spatial_reference = "NAD_1983_StatePlane_California_I_FIPS_0401",
#'               horizontal_accuracy = "No Information",
#'               vertical_accuracy =  "No Information",
#'               cell_size_x = "25",
#'               cell_size_y = "25",
#'               number_of_bands = "10",
#'               raster_origin = "Upper Left",
#'               rows = "200",
#'               columns = "6",
#'               verticals = "1",
#'               cell_geometry = "pixel")}
#'
#' @export            
create_raster <- function(file_name, file_description, attribute_list, physical,
                          spatial_reference, horizontal_accuracy, vertical_accuracy, 
                          cell_size_x, cell_size_y, number_of_bands, raster_origin, 
                          rows, columns, verticals, cell_geometry) {
  
  required_arguments <- c("file_name", "file_description", "attribute_list", "physical",
                          "spatial_reference", "horizontal_accuracy", "vertical_accuracy",
                          "cell_size_x", "cell_size_y", "number_of_bands", "raster_origin",
                          "rows", "columns", "verticals", "cell_geometry")

  missing_argument_index <- which(c(missing(file_name), missing(file_description), 
                                    missing(attribute_list), missing(physical), missing(spatial_reference), 
                                    missing(horizontal_accuracy), missing(vertical_accuracy),
                                    missing(cell_size_x), missing(cell_size_y), missing(number_of_bands),
                                    missing(raster_origin), missing(rows), missing(columns),
                                    missing(verticals), missing(cell_geometry)))
  
  if (length(missing_argument_index) > 0) {
    raster_error <- required_arguments[missing_argument_index][1]
    raster_error_message <- paste("Please supply the", raster_error)
    stop(raster_error_message, call. = FALSE)
  }

  spatialRaster <- list(entityName = file_name, 
                        entityDescription = file_description,
                        attributeList = attribute_list,
                        physical = physical,
                        spatialReference = list(horizCoordSysName = spatial_reference),
                        horizontalAccuracy = list(accuracyReport = horizontal_accuracy), 
                        verticalAccuracy = list(accuracyReport = vertical_accuracy), 
                        cellSizeXdirection = cell_size_x, 
                        cellSizeYDirection = cell_size_y,
                        numberOfBands = number_of_bands, 
                        rasterOrigin = raster_origin, 
                        rows = rows, 
                        columns = columns, 
                        verticals = verticals, 
                        cellGeometry = cell_geometry)
  return(spatialRaster)
}
#' Add Raster File
#' @description Adds the raster elements to a dataset list according to EML standards. 
#' @param parent_element A list representing the EML project or dataset.
#' @param raster_metadata A named list or dataframe containing raster metadata elements
#' (file_name, file_description, attribute_list, physical, spatial_reference, horizontal_accuracy, vertical_accuracy, 
#' cell_size_x, cell_size_y, number_of_bands, raster_origin, rows, columns, verticals, cell_geometry): see \code{\link{create_raster}} 
#' @return The dataset list or project with raster file information appended.
#' @examples  
#' raster_metadata <- list("file_name" = "Rasterfiles.tif",
#'                         "file_description" = "A Raster File",
#'                         "attribute_list" =  create_attribute("attribute_name" = "Yrs", attribute_label = "Years", 
#'                                                         "attribute_definition" = "Calendar year of the observation from years 1990 - 2010.", 
#'                                                         "storage_type" = EMLaide::storage_type$date, 
#'                                                         "measurement_scale" = EMLaide::measurement_scale$dateTime, 
#'                                                         "date_time_format" = "YYYY",
#'                                                         "date_time_precision" = "1", minimum = "1993", maximum = "2003"),
#'                         "physical" = create_physical("Rasterfiles.tif"),
#'                         "spatial_reference" = "NAD_1983_StatePlane_California_I_FIPS_0401",
#'                         "horizontal_accuracy" = "No Information",
#'                         "vertical_accuracy" =  "No Information",
#'                         "cell_size_x" = "25",
#'                         "cell_size_y" = "25",
#'                         "number_of_bands" = "10",
#'                         "raster_origin" = "Upper Left",
#'                         "rows" = "200",
#'                         "columns" = "6",
#'                         "verticals" = "1",
#'                         "cell_geometry" = "pixel")
#' \dontrun{
#' dataset <- list() %>%
#'     add_raster(raster_metadata)
#' dataset
#' }
#' @export
add_raster <- function(parent_element, raster_metadata) {
  parent_element$spatialRaster <- create_raster(file_name = raster_metadata$file_name, 
                                                file_description = raster_metadata$file_description, 
                                                attribute_list = raster_metadata$attribute_list, 
                                                physical = raster_metadata$physical,
                                                spatial_reference = raster_metadata$spatial_reference, 
                                                horizontal_accuracy = raster_metadata$horizontal_accuracy, 
                                                vertical_accuracy = raster_metadata$vertical_accuracy, 
                                                cell_size_x = raster_metadata$cell_size_x, 
                                                cell_size_y = raster_metadata$cell_size_y, 
                                                number_of_bands = raster_metadata$number_of_bands, 
                                                raster_origin = raster_metadata$raster_origin, 
                                                rows = raster_metadata$rows, 
                                                columns = raster_metadata$columns, 
                                                verticals = raster_metadata$verticals, 
                                                cell_geometry = raster_metadata$cell_geometry)
  return(parent_element)
}