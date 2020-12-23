# Tests for spatial raster 
test_that('add_raster errors when not provided the mandatory input elements', {
  file_name = "Rasterfiles.zip" 
  file_description = "A Raster File"
  attribute_list =  "attribute list"
  physical = "physical list"
  spatial_reference = "NAD_1983_StatePlane_California_I_FIPS_0401"
  horizontal_accuracy = "No Information"
  vertical_accuracy =  "No Information"
  cell_size_x = "25"
  cell_size_y = "25"
  number_of_bands = "10"
  raster_origin = "Upper Left"
  rows = "200"
  columns = "6"
  verticals = "1"
  cell_geometry = "pixel"
  
  expect_error(add_raster(file_description = file_description,
                          attribute_list = attribute_list,
                          physical = physical,
                          spatial_reference = spatial_reference,
                          horizontal_accuracy = horizontal_accuracy,
                          vertical_accuracy = vertical_accuracy,
                          cell_size_x = cell_size_x,
                          cell_size_y = cell_size_y,
                          number_of_bands = number_of_bands,
                          raster_origin = raster_origin,
                          rows = rows,
                          columns = columns,
                          verticals = verticals,
                          cell_geometry = cell_geometry), "Please supply the file_name")
  
  expect_error(add_raster(file_name = file_name,
                          attribute_list = attribute_list,
                          physical = physical,
                          spatial_reference = spatial_reference,
                          horizontal_accuracy = horizontal_accuracy,
                          vertical_accuracy = vertical_accuracy,
                          cell_size_x = cell_size_x,
                          cell_size_y = cell_size_y,
                          number_of_bands = number_of_bands,
                          raster_origin = raster_origin,
                          rows = rows,
                          columns = columns,
                          verticals = verticals,
                          cell_geometry = cell_geometry), "Please supply the file_description")
  
  expect_error(add_raster(file_name = file_name, 
                          file_description = file_description,
                          physical = physical,
                          spatial_reference = spatial_reference,
                          horizontal_accuracy = horizontal_accuracy,
                          vertical_accuracy = vertical_accuracy,
                          cell_size_x = cell_size_x,
                          cell_size_y = cell_size_y,
                          number_of_bands = number_of_bands,
                          raster_origin = raster_origin,
                          rows = rows,
                          columns = columns,
                          verticals = verticals,
                          cell_geometry = cell_geometry), "Please supply the attribute_list")
  
  expect_error(add_raster(file_name = file_name, 
                          file_description = file_description,
                          attribute_list = attribute_list,
                          spatial_reference = spatial_reference,
                          horizontal_accuracy = horizontal_accuracy,
                          vertical_accuracy = vertical_accuracy,
                          cell_size_x = cell_size_x,
                          cell_size_y = cell_size_y,
                          number_of_bands = number_of_bands,
                          raster_origin = raster_origin,
                          rows = rows,
                          columns = columns,
                          verticals = verticals,
                          cell_geometry = cell_geometry), "Please supply the physical")
  
  expect_error(add_raster(file_name = file_name, 
                          file_description = file_description,
                          attribute_list = attribute_list,
                          physical = physical,
                          horizontal_accuracy = horizontal_accuracy,
                          vertical_accuracy = vertical_accuracy,
                          cell_size_x = cell_size_x,
                          cell_size_y = cell_size_y,
                          number_of_bands = number_of_bands,
                          raster_origin = raster_origin,
                          rows = rows,
                          columns = columns,
                          verticals = verticals,
                          cell_geometry = cell_geometry), "Please supply the spatial_reference")
  
  expect_error(add_raster(file_name = file_name, 
                          file_description = file_description,
                          attribute_list = attribute_list,
                          physical = physical,
                          spatial_reference = spatial_reference,
                          vertical_accuracy = vertical_accuracy,
                          cell_size_x = cell_size_x,
                          cell_size_y = cell_size_y,
                          number_of_bands = number_of_bands,
                          raster_origin = raster_origin,
                          rows = rows,
                          columns = columns,
                          verticals = verticals,
                          cell_geometry = cell_geometry), "Please supply the horizontal_accuracy")
  
  expect_error(add_raster(file_name = file_name, 
                          file_description = file_description,
                          attribute_list = attribute_list,
                          physical = physical,
                          spatial_reference = spatial_reference,
                          horizontal_accuracy = horizontal_accuracy,
                          cell_size_x = cell_size_x,
                          cell_size_y = cell_size_y,
                          number_of_bands = number_of_bands,
                          raster_origin = raster_origin,
                          rows = rows,
                          columns = columns,
                          verticals = verticals,
                          cell_geometry = cell_geometry), "Please supply the vertical_accuracy")
  
  expect_error(add_raster(file_name = file_name, 
                          file_description = file_description,
                          attribute_list = attribute_list,
                          physical = physical,
                          spatial_reference = spatial_reference,
                          horizontal_accuracy = horizontal_accuracy,
                          vertical_accuracy = vertical_accuracy,
                          cell_size_y = cell_size_y,
                          number_of_bands = number_of_bands,
                          raster_origin = raster_origin,
                          rows = rows,
                          columns = columns,
                          verticals = verticals,
                          cell_geometry = cell_geometry), "Please supply the cell_size_x")
  
  expect_error(add_raster(file_name = file_name, 
                          file_description = file_description,
                          attribute_list = attribute_list,
                          physical = physical,
                          spatial_reference = spatial_reference,
                          horizontal_accuracy = horizontal_accuracy,
                          vertical_accuracy = vertical_accuracy,
                          cell_size_x = cell_size_x,
                          number_of_bands = number_of_bands,
                          raster_origin = raster_origin,
                          rows = rows,
                          columns = columns,
                          verticals = verticals,
                          cell_geometry = cell_geometry), "Please supply the cell_size_y")
  
  expect_error(add_raster(file_name = file_name, 
                          file_description = file_description,
                          attribute_list = attribute_list,
                          physical = physical,
                          spatial_reference = spatial_reference,
                          horizontal_accuracy = horizontal_accuracy,
                          vertical_accuracy = vertical_accuracy,
                          cell_size_x = cell_size_x,
                          cell_size_y = cell_size_y,
                          raster_origin = raster_origin,
                          rows = rows,
                          columns = columns,
                          verticals = verticals,
                          cell_geometry = cell_geometry), "Please supply the number_of_bands")
  
  expect_error(add_raster(file_name = file_name, 
                          file_description = file_description,
                          attribute_list = attribute_list,
                          physical = physical,
                          spatial_reference = spatial_reference,
                          horizontal_accuracy = horizontal_accuracy,
                          vertical_accuracy = vertical_accuracy,
                          cell_size_x = cell_size_x,
                          cell_size_y = cell_size_y,
                          number_of_bands = number_of_bands,
                          rows = rows,
                          columns = columns,
                          verticals = verticals,
                          cell_geometry = cell_geometry), "Please supply the raster_origin")
  
  expect_error(add_raster(file_name = file_name, 
                          file_description = file_description,
                          attribute_list = attribute_list,
                          physical = physical,
                          spatial_reference = spatial_reference,
                          horizontal_accuracy = horizontal_accuracy,
                          vertical_accuracy = vertical_accuracy,
                          cell_size_x = cell_size_x,
                          cell_size_y = cell_size_y,
                          number_of_bands = number_of_bands,
                          raster_origin = raster_origin,
                          columns = columns,
                          verticals = verticals,
                          cell_geometry = cell_geometry), "Please supply the rows")
  
  expect_error(add_raster(file_name = file_name, 
                          file_description = file_description,
                          attribute_list = attribute_list,
                          physical = physical,
                          spatial_reference = spatial_reference,
                          horizontal_accuracy = horizontal_accuracy,
                          vertical_accuracy = vertical_accuracy,
                          cell_size_x = cell_size_x,
                          cell_size_y = cell_size_y,
                          number_of_bands = number_of_bands,
                          raster_origin = raster_origin,
                          rows = rows,
                          verticals = verticals,
                          cell_geometry = cell_geometry), "Please supply the columns")
  
  expect_error(add_raster(file_name = file_name, 
                          file_description = file_description,
                          attribute_list = attribute_list,
                          physical = physical,
                          spatial_reference = spatial_reference,
                          horizontal_accuracy = horizontal_accuracy,
                          vertical_accuracy = vertical_accuracy,
                          cell_size_x = cell_size_x,
                          cell_size_y = cell_size_y,
                          number_of_bands = number_of_bands,
                          raster_origin = raster_origin,
                          rows = rows,
                          columns = columns,
                          cell_geometry = cell_geometry), "Please supply the verticals")
  
  expect_error(add_raster(file_name = file_name, 
                          file_description = file_description,
                          attribute_list = attribute_list,
                          physical = physical,
                          spatial_reference = spatial_reference,
                          horizontal_accuracy = horizontal_accuracy,
                          vertical_accuracy = vertical_accuracy,
                          cell_size_x = cell_size_x,
                          cell_size_y = cell_size_y,
                          number_of_bands = number_of_bands,
                          raster_origin = raster_origin,
                          rows = rows,
                          columns = columns,
                          verticals = verticals), "Please supply the cell_geometry")
  
  
  
})

test_that('The add raster function returns a valid list to turn into a EML file', {
  parent_element = list()
  file_name = "Rasterfiles.zip" 
  file_description = "A Raster File"
  attribute_list =  "attribute list"
  physical = "physical list"
  spatial_reference = "NAD_1983_StatePlane_California_I_FIPS_0401"
  horizontal_accuracy = "No Information"
  vertical_accuracy =  "No Information"
  cell_size_x = "25"
  cell_size_y = "25"
  number_of_bands = "10"
  raster_origin = "Upper Left"
  rows = "200"
  columns = "6"
  verticals = "1"
  cell_geometry = "pixel"
  
  expected = list(spatialRaster = list(entityName = "Rasterfiles.zip", 
                                       entityDescription = "A Raster File",
                                       attributeList = "attribute list",
                                       physical = "physical list",
                                       spatialReference = list(horizCoordSysName = "NAD_1983_StatePlane_California_I_FIPS_0401"),
                                       horizontalAccuracy = list(accuracyReport = "No Information"), 
                                       verticalAccuracy = list(accuracyReport = "No Information"), 
                                       cellSizeXdirection = "25", 
                                       cellSizeYDirection = "25",
                                       numberOfBands = "10", 
                                       rasterOrigin = "Upper Left", 
                                       rows = "200", 
                                       columns = "6", 
                                       verticals = "1", 
                                       cellGeometry = "pixel"))
  
  expect_equal(add_raster(parent_element = parent_element,
                          file_name = file_name, 
                          file_description = file_description,
                          attribute_list = attribute_list,
                          physical = physical, 
                          spatial_reference = spatial_reference,
                          horizontal_accuracy = horizontal_accuracy,
                          vertical_accuracy = vertical_accuracy,
                          cell_size_x = cell_size_x,
                          cell_size_y = cell_size_y,
                          number_of_bands = number_of_bands,
                          raster_origin = raster_origin,
                          rows = rows,
                          columns = columns,
                          verticals = verticals,
                          cell_geometry = cell_geometry), expected)
})