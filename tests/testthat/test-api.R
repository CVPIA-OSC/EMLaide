library(EMLaide)
library(vcr)
library(testthat)
# Define variables -------------------------------------------------------------
eml_file_path <- "../../vignettes/edi.678.1.xml" 
user_id <- "ecain"

# Test data package id reservation ----------------------------------------------
test_that("reserve_edi_id() errors when arguments are not supplied", {
  expect_error(reserve_edi_id(user_id = user_id), "argument \"password\" is missing, with no default")
  expect_error(reserve_edi_id(password = Sys.getenv("APIKEY")), "argument \"user_id\" is missing, with no default")
})

test_that("reserve_edi_id() returns an edi number", {
  vcr::use_cassette("reserve_edi_id_responses", {
    id <- reserve_edi_id(user_id, Sys.getenv("APIKEY"))
  })
  expect_type(id, "character")
})

# Test data package evaluation function ----------------------------------------
test_that("evaluate_edi_package() errors when arguments are not supplied", {
  expect_error(evaluate_edi_package(user_id = user_id, password = Sys.getenv("APIKEY")), "argument \"eml_file_path\" is missing, with no default")
  expect_error(evaluate_edi_package(password = Sys.getenv("APIKEY"), eml_file_path = eml_file_path), "argument \"user_id\" is missing, with no default")
  expect_error(evaluate_edi_package(user_id = user_id, eml_file_path = eml_file_path), "argument \"password\" is missing, with no default")
})

