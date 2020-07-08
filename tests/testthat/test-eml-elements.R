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


test_that('dataset abstract warns if abstract is too short',  {
  expect_warning(add_abstract(list(), abstract = "A not very specific abstract"))
})

test_that('warn when there is at less than one keyword within the keywordSets', {
  expect_warning(add_keyword_set(list(), keyword_set = c()))
})

test_that('personnel function errors when missing mandatory identifier inputs',  {
  parent_element <- list()
  role1 <- "Creator"
  role2 <- "Data Manager"
  first_name <- "Susan"
  last_name <- "Susanton"
  email <- "susanton@fake.com"
  orcid <- "00110011"
  organization <- "USFWS"
  
  expect_error(add_personnel(parent_element = parent_element, role = role1, 
                             first_name = first_name, last_name = last_name),
               "Please supply a email.")
  expect_error(add_personnel(parent_element = parent_element, role = role1, 
                             first_name = first_name, email = email),
               "Please supply a last name.")
  expect_error(add_personnel(parent_element = parent_element, role = role1, 
                             last_name = last_name, email = email), 
               "Please supply a first name.")
  expect_error(add_personnel(parent_element = parent_element, first_name = first_name, 
                             last_name = last_name, email = email), 
               "Please supply a role. Use 'Creator' if you are the main originator of the dataset or project")
  
  expect_equal(add_personnel(parent_element = parent_element, first_name = first_name, 
                             last_name = last_name, email = email, role = role1),
               list(creator = list(individualName = list(givenName = "Susan", 
                                                         surName = "Susanton"), 
                                   electronicMailAddress = "susanton@fake.com")))
  
  expect_equal(add_personnel(parent_element = parent_element, first_name = first_name, 
                             last_name = last_name, email = email, role = role2, orcid = orcid, 
                             organization = organization),
               list(associatedParty = list(individualName = 
                                             list(givenName = "Susan", surName = "Susanton"),
                                           electronicMailAddress = "susanton@fake.com", 
                                           userid = list(directory = "https://orcid.org", "https://orcid.org/00110011"), 
                                           organizationName = "USFWS", 
                                           role = "Data Manager")))
  
  creator_1 <- add_personnel(parent_element = parent_element, first_name = first_name, 
                             last_name = last_name, email = email, role = role1)
  
  expect_equal(add_personnel(parent_element = creator_1, first_name = "Not Susan", 
                             last_name = "Smith", email = "free_cats@aol.com", role = role1),
               list(creator = list(list(individualName = list(givenName = "Susan", 
                                                              surName = "Susanton"), 
                                        electronicMailAddress = "susanton@fake.com"), 
                                   list(individualName = list(givenName = "Not Susan", surName = "Smith"), 
                                        electronicMailAddress = "free_cats@aol.com"))))
  
  data_manager_1 <- add_personnel(parent_element = parent_element, first_name = first_name, 
                                  last_name = last_name, email = email, role = role2)
  
  expect_equal(add_personnel(parent_element = data_manager_1, first_name = "Not Susan", 
                             last_name = "Smith", email = "free_cats@aol.com", role = role2),
               list(associatedParty = list(list(individualName = list(givenName = "Susan", 
                                                                      surName = "Susanton"), 
                                                electronicMailAddress = "susanton@fake.com", 
                                                role = "Data Manager"), 
                                           list(individualName = list(givenName = "Not Susan", surName = "Smith"), 
                                                electronicMailAddress = "free_cats@aol.com", 
                                                role = "Data Manager"))))
  
  
})