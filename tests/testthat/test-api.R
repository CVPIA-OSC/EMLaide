library(EMLaide)
library(vcr)
library(testthat)
# Define variables -------------------------------------------------------------
eml_file_path <- "vignettes/edi.678.1.xml" 
user_id <- "ecain"
# Test data packge id reservation ----------------------------------------------
test_that("reserve_edi_id() works", {
  vcr::use_cassette("reserve_edi_id", {
    id <- reserve_edi_id(user_id, "<<<my_api_key>>>")
  })
  testthat::expect_type(id, "character")
})
# Test data package evaluation function ----------------------------------------
