parent_element <- list()

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

test_that('dataset title function adds title and short name.',{
  
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


test_that('the dataset add_abstract function adds abstract', {
  abstract <- "This is the abstract for my test. It needs to have twenty or more words for it to pass. It informs the users if this dataset relates to what they are studying or not."
  #abstract <- add_abstract(parent_element = parent_element, abstract = abstract)
  
  expect_equal(add_abstract(parent_element = parent_element, abstract = abstract),
               list(abstract = list(para = "This is the abstract for my test. It needs to have twenty or more words for it to pass. It informs the users if this dataset relates to what they are studying or not.")))
})


test_that('warn when there is less than one keyword within the keywordSets', {
  expect_warning(add_keyword_set(list(), keyword_set = c()))
})


test_that('the dataset add_keyword_set function adds the keyword set',{
          keyword_set_1 <- list(keyword = list("dog", "cat", "cow", "pig"),
                              keywordThesaurus = "LTER Controlled Vocabulary")
          
          first <- add_keyword_set(parent_element = parent_element, keyword_set = keyword_set_1)
          
          expect_equal(add_keyword_set(parent_element, keyword_set_1),
                       list(KeywordSet = list(keyword = list("dog", "cat", "cow", "pig"), 
                                              keywordThesaurus = "LTER Controlled Vocabulary")))
          
          keyword_set_2<- list(keyword = list("bear", "lion"))
          #keyword_set_2 <- add_keyword_set(parent_element = parent_element, keyword_set = keyword_set_2)
          
          
          expect_equal(add_keyword_set(parent_element, keyword_set_2), 
                       list(KeywordSet = list(keyword = list("bear", "lion"))))
          
          first <- add_keyword_set(parent_element, keyword_set_1)
          second <- add_keyword_set(first, keyword_set_2)
          
          expect_equal(second, list(KeywordSet = list(list(keyword = list("dog", "cat", "cow", "pig"), 
                                                 keywordThesaurus = "LTER Controlled Vocabulary"), 
                                                 list(keyword = list("bear", "lion"))))
          )

})

test_that('personnel function errors when missing mandatory identifier inputs',  {
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

test_that('Intellectual rights function errors when missing mandatory identifier inputs', {
  
  expect_error(add_license(parent_element = list(), default_license = NULL, license_name = "Creative Commons",
                           license_identifier = "CC-BY-NC-SA-4.0",
                           intellectual_rights_descripiton = "The description goes here"), 
               "Please provide a url for the license.")
  
  expect_error(add_license(parent_element = list(), default_license = NULL, license_name = "Creative Commons",
                           license_url = "https://spdx.org/licenses/CC-BY-NC-SA-4.0.html",
                           intellectual_rights_descripiton = "The description goes here"), 
               "Please provide the license identifier.")
  
  expect_error(add_license(parent_element = list(), default_license = NULL, license_name = "Creative Commons",
                           license_identifier = "CC-BY-NC-SA-4.0",
                           license_url = "https://spdx.org/licenses/CC-BY-NC-SA-4.0.html"), 
               "Please provide a simplified description of the license.")
})

test_that('Intellectual rights function outputs the correct values when given the default inputs of "CCO" and "CCBY', {
  
  #CCO <- add_license(parent_element = parent_element)
  #CCBY <- add_license(list(), "CCBY")
  expect_equal(add_license(list(), "CCO"), 
               list(intellectualRights = list(para = "This data package is released to the \"public domain\" under Creative Commons CC0 1.0 \"No Rights Reserved\" (see: https://creativecommons.org/publicdomain/zero/1.0/). It is considered professional etiquette to provide attribution of the original work if this data package is shared in whole or by individual components. A generic citation is provided for this data package on the website https://portal.edirepository.org (herein \"website\") in the summary metadata page. Communication (and collaboration) with the creators of this data package is recommended to prevent duplicate research or publication. This data package (and its components) is made available \"as is\" and with no warranty of accuracy or fitness for use. The creators of this data package and the website shall not be liable for any damages resulting from misinterpretation or misuse of the data package or its components. Periodic updates of this data package may be available from the website. Thank you.&#13;"), 
                    licensed = list(licensedName = "Creative Commons Zero v1.0 Universal", 
                                    url = "https://spdx.org/licenses/CC0-1.0.html", identifier = "CC0-1.0")))

  
  expect_equal(add_license(list(), "CCBY"),
               list(intellectualRights = list(para = "This information is released under the Creative Commons license - Attribution - CC BY (https://creativecommons.org/licenses/by/4.0/). The consumer of these data (\"Data User\" herein) is required to cite it appropriately in any publication that results from its use. The Data User should realize that these data may be actively used by others for ongoing research and that coordination may be necessary to prevent duplicate publication. The Data User is urged to contact the authors of these data if any questions about methodology or results occur. Where appropriate, the Data User is encouraged to consider collaboration or co-authorship with the authors. The Data User should realize that misinterpretation of data may occur if used out of context of the original study. While substantial efforts are made to ensure the accuracy of data and associated documentation, complete accuracy of data sets cannot be guaranteed. All data are made available \"as is.\" The Data User should be aware, however, that data are updated periodically and it is the responsibility of the Data User to check for new versions of the data. The data authors and the repository where these data were obtained shall not be liable for damages resulting from any use or misinterpretation of the data. Thank you.&#13;"), 
                    licensed = list(licensedName = "Creative Commons Attribution 4.0 International", 
                                    url = "https://spdx.org/licenses/CC-BY-4.0.html", identifier = "CC-BY-4.0")))
})