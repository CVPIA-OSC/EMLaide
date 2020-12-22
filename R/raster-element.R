add_raster <- function(file_name, file_description, attribute_list, 
                       spatial_reference_name, spacial_reference_def, 
                       horizontal_accuracy, vertical_accuracy, 
                       cell_size_x, cell_size_y, number_of_bands, raster_origin, 
                       rows, columns, verticals, cell_geometry) {
  
  SpatialRaster <- list(entityName = file_name, 
                        entityDescription = file_description,
                        attributeList = attribute_list,
                        spatialReference = list(spatial_reference_name, 
                                                spatial_reference_def),
                        horizontalAccuracy = horizontal_accuracy, 
                        verticalAccuracy = vertical_accuracy, 
                        cellSizeXdirection = cell_size_x, 
                        cellSizeYDirection = cell_size_y,
                        numberOfBands = number_of_bands, 
                        rasterOrigin = raster_origin, 
                        rows = rows, 
                        columns = columns, 
                        verticals = verticals, 
                        cellGeometry = cell_geometry)
}