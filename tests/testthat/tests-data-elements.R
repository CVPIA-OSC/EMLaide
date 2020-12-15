# Tests for excel element 
test_that('Excel function errors when excel file is not inputed', { 
  expect_error(read_excel_sheets("read_me"))
})

test_that('Excel function outputs a list', {
  expect_type(read_excel_sheets("test_data.xlsx"),
              "list")
})

test_that('Excel function outputs a list of the expected length', {
  expect_length(read_excel_sheets("test_data.xlsx"),
  length(readxl::excel_sheets("test_data.xlsx")))
})

test_that('Excel function outputs the proper names of sheets', {
  expect_equal(names(read_excel_sheets("test_data.xlsx")),
               readxl::excel_sheets("test_data.xlsx"))
})