passing_dataset <- list() %>% 
  add_title(title = "This is 5 words or more", 
            short_name = "This is a short name for this dataset")

failing_dataset <- list() %>% 
  add_title(title = "This is 4 words", 
            short_name = "This is a short name for this dataset")

test_that("dataset title length is at least 5 words.", {
  expect_gte(length(unlist(strsplit(passing_dataset$title, split = " "))), 5)
})


