test_that('dataset title length is between 7 and 20 words long', {
  
  passing_dataset <- list() %>% 
    add_title(title = "This title is at least 7 words and not over 20 words.", 
              short_name = "This is a short name for this dataset")
  
  expect_gte(length(unlist(strsplit(passing_dataset$title, split = " "))), 7)
  
  expect_error(
    add_title(list(), title = "Too long title lorem ipsum dolor sit amet, consectetur adipiscing elit. Aliquam ante ipsum, consectetur quis iaculis eget, rhoncus rutrum velit. Donec cursus massa non.",
              short_name = "This is a short name for this dataset"),
    "Please make sure your title is between 7 and 20 words long."
  )
  
  expect_error(
    add_title(list(), title = "Too short title.",
              short_name = "This is a short name for this dataset"),
    "Please make sure your title is between 7 and 20 words long."
  )
  
  expect_error(
    add_title(list(), title = "This title is at least 7 words and not over 20 words.",
              short_name = "This is a short name and it is way way way way way way way way way way way way way way way way way way too long."),
    "Short name should not be longer than the dataset's title."
  )
  
 
})

test_that('dataset title function is producing the expected values',{
  
  parent_element <- list()
  title <- "This title will work because it is of a sufficient length"
  short_name <- "This is the short name"
  
  title_1 <- add_title(parent_element = parent_element, title = title, short_name = short_name)
  
  expect_equal(add_title(parent_element = parent_element, title = title, short_name = short_name),
               list(title = "This title will work because it is of a sufficient length", 
                    shortName = "This is the short name")
  )
  
})

test_that('dataset abstract warns if abstract is too short',  {
  expect_warning(add_abstract(list(), abstract = "A not very specific abstract"))
})


test_that('the dataset abstract function is producing the expected values', {
  parent_element <- list()
  abstract <- "This is the abstract for my test. It needs to have twenty or more words for it to pass. It informs the users if this dataset relates to what they are studying or not."
  abstract_1 <- add_abstract(parent_element = parent_element, abstract = abstract)
  
  expect_equal(add_abstract(parent_element = parent_element, abstract = abstract),
               list(abstract = list(para = "This is the abstract for my test. It needs to have twenty or more words for it to pass. It informs the users if this dataset relates to what they are studying or not.")))
})


test_that('warn when there is at less than one keyword within the keywordSets', {
  expect_warning(add_keyword_set(list(), keyword_set = c()))
})

