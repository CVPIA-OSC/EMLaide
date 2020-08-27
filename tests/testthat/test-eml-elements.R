parent_element <- list()

#Tests for add_title function 

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

#Tests for add_abstract function 
setwd("~/FlowWest/cvpiaEDIutils")

test_that('the dataset add_abstract function adds abstract', {
  
  expect_equal(add_abstract(parent_element = list(), abstract = "tests/testthat/abstract_test.docx"),
               list(abstract = list(section = list(), para = list("\n  This is a test for the abstract. This abstract is of sufficient\n  length. Please make sure your abstract meets the word limit of twenty\n  words or more.\n"))))
})

#Tests for add_keyword_set function 

test_that('warn when there is less than one keyword within the keywordSets', {
  expect_warning(add_keyword_set(list(), keyword_set = c()))
})


test_that('the dataset add_keyword_set function adds the keyword set',{
  
  keyword_set_1 <- list(keyword = list("dog", "cat", "cow", "pig"),
                        keywordThesaurus = "LTER Controlled Vocabulary")
  
  keyword_set_2 <- list(keyword = list("bear", "lion"))
  
  first <- add_keyword_set(parent_element, keyword_set_1)
  second <- add_keyword_set(first, keyword_set_2)
  
  expect_equal(first,
               list(keywordSet = list(keyword = list("dog", "cat", "cow", "pig"), 
                                      keywordThesaurus = "LTER Controlled Vocabulary")))
  
  expect_equal(second, list(keywordSet = list(list(keyword = list("dog", "cat", "cow", "pig"), 
                                                   keywordThesaurus = "LTER Controlled Vocabulary"), 
                                              list(keyword = list("bear", "lion"))))
  )
  
})

#Tests for add_personnel function 

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
               "Please supply an email.")
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
                             last_name = last_name, email = email, role = role1, orcid = orcid),
               list(contact = list(individualName = list(givenName = "Susan", 
                                                         surName = "Susanton"), electronicMailAddress = "susanton@fake.com"), 
                    creator = list(individualName = list(givenName = "Susan", 
                                                         surName = "Susanton"), electronicMailAddress = "susanton@fake.com", 
                                   `@id` = "00110011")))
  
  expect_equal(add_personnel(parent_element = parent_element, first_name = first_name,
                             last_name = last_name, email = email, role = role2, orcid = orcid,
                             organization = organization),
               list(associatedParty = list(individualName = list(givenName = "Susan", 
                                                                 surName = "Susanton"), electronicMailAddress = "susanton@fake.com", 
                                           organizationName = "USFWS", role = "Data Manager")))
  
  creator_1 <- add_personnel(parent_element = parent_element, first_name = first_name, 
                             last_name = last_name, email = email, role = role1)
  
  expect_equal(add_personnel(parent_element = creator_1, first_name = "Not Susan", 
                             last_name = "Smith", email = "free_cats@aol.com", role = role1),
               list(contact = list(individualName = list(givenName = "Not Susan", 
                                                         surName = "Smith"), electronicMailAddress = "free_cats@aol.com"), 
                    creator = list(list(individualName = list(givenName = "Susan", 
                                                              surName = "Susanton"), electronicMailAddress = "susanton@fake.com"), 
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

#Tests for add_funding function 

test_that('funding function errors when missing mandatory identifier inputs', {
  
  expect_error(add_funding(parent_element = list(), 
                           funder_identifier = "http://dx.doi.org/10.13039/100000001",
                           award_number = "1656026",
                           award_title = "LTER: Beaufort Sea Lagoons: An Arctic Coastal Ecosystem in Transition",
                           award_url = "https://www.nsf.gov/awardsearch/showAward?AWD_ID=1656026",
                           funding_description = "BLE LTER is supported by the National Science Foundation under award #1656026 (2017-08-01 to 2022-07-31)." ), 
               "Please provide funders name.")
  
  expect_error(add_funding(parent_element = list(), funder_name = "National Science Foundation",
                           award_number = "1656026",
                           award_title = "LTER: Beaufort Sea Lagoons: An Arctic Coastal Ecosystem in Transition",
                           award_url = "https://www.nsf.gov/awardsearch/showAward?AWD_ID=1656026",
                           funding_description = "BLE LTER is supported by the National Science Foundation under award #1656026 (2017-08-01 to 2022-07-31)." ),
               "Please provide funder identifier link.")
  
  expect_error(add_funding(parent_element = list(), funder_name = "National Science Foundation",
                           funder_identifier = "http://dx.doi.org/10.13039/100000001",
                           award_title = "LTER: Beaufort Sea Lagoons: An Arctic Coastal Ecosystem in Transition",
                           award_url = "https://www.nsf.gov/awardsearch/showAward?AWD_ID=1656026",
                           funding_description = "BLE LTER is supported by the National Science Foundation under award #1656026 (2017-08-01 to 2022-07-31)." ),
               "Please provide your award number.")
  
  expect_error(add_funding(parent_element = list(), funder_name = "National Science Foundation",
                           funder_identifier = "http://dx.doi.org/10.13039/100000001",
                           award_number = "1656026",
                           award_url = "https://www.nsf.gov/awardsearch/showAward?AWD_ID=1656026",
                           funding_description = "BLE LTER is supported by the National Science Foundation under award #1656026 (2017-08-01 to 2022-07-31)." ),
               "Please provide the title of your project.")
  
  expect_warning(add_funding(parent_element = list(), funder_name = "National Science Foundation",
                             funder_identifier = "http://dx.doi.org/10.13039/100000001",
                             award_number = "1656026",
                             award_title = "LTER: Beaufort Sea Lagoons: An Arctic Coastal Ecosystem in Transition",
                             funding_description = "BLE LTER is supported by the National Science Foundation under award #1656026 (2017-08-01 to 2022-07-31)." ),
                 "Please provide the award url.")
  
  expect_warning(add_funding(parent_element = list(), funder_name = "National Science Foundation",
                             funder_identifier = "http://dx.doi.org/10.13039/100000001",
                             award_number = "1656026",
                             award_title = "LTER: Beaufort Sea Lagoons: An Arctic Coastal Ecosystem in Transition",
                             award_url = "https://www.nsf.gov/awardsearch/showAward?AWD_ID=1656026"),
                 "Please provide the description of the funding recieved.")
  
  
})

test_that('The add_funding function adds the funding elements', {
  
  expect_equal(add_funding(parent_element = list(), funder_name = "National Science Foundation",
                           funder_identifier = "http://dx.doi.org/10.13039/100000001",
                           award_number = "1656026",
                           award_title = "LTER: Beaufort Sea Lagoons: An Arctic Coastal Ecosystem in Transition",
                           award_url = "https://www.nsf.gov/awardsearch/showAward?AWD_ID=1656026",
                           funding_description = "BLE LTER is supported by the National Science Foundation under award #1656026 (2017-08-01 to 2022-07-31)."),
               list(funding = list(para = "BLE LTER is supported by the National Science Foundation under award #1656026 (2017-08-01 to 2022-07-31)."), 
                    award = list(funderName = "National Science Foundation", 
                                 funderIdentifier = "http://dx.doi.org/10.13039/100000001", 
                                 awardNumber = "1656026", title = "LTER: Beaufort Sea Lagoons: An Arctic Coastal Ecosystem in Transition", 
                                 awardUrl = "https://www.nsf.gov/awardsearch/showAward?AWD_ID=1656026"))
  )
  
})

#Tests for add_license function 

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
  
  
  expect_equal(add_license(list(), "CCO"), 
               list(intellectualRights = list(para = "This data package is released to the \"public domain\" under Creative Commons CC0 1.0 \"No Rights Reserved\" (see: https://creativecommons.org/publicdomain/zero/1.0/). It is considered professional etiquette to provide attribution of the original work if this data package is shared in whole or by individual components. A generic citation is provided for this data package on the website https://portal.edirepository.org (herein \"website\") in the summary metadata page. Communication (and collaboration) with the creators of this data package is recommended to prevent duplicate research or publication. This data package (and its components) is made available \"as is\" and with no warranty of accuracy or fitness for use. The creators of this data package and the website shall not be liable for any damages resulting from misinterpretation or misuse of the data package or its components. Periodic updates of this data package may be available from the website. Thank you.&#13;"), 
                    licensed = list(licenseName = "Creative Commons Zero v1.0 Universal", 
                                    url = "https://spdx.org/licenses/CC0-1.0.html", identifier = "CC0-1.0")))
  
  
  expect_equal(add_license(list(), "CCBY"),
               list(intellectualRights = list(para = "This information is released under the Creative Commons license - Attribution - CC BY (https://creativecommons.org/licenses/by/4.0/). The consumer of these data (\"Data User\" herein) is required to cite it appropriately in any publication that results from its use. The Data User should realize that these data may be actively used by others for ongoing research and that coordination may be necessary to prevent duplicate publication. The Data User is urged to contact the authors of these data if any questions about methodology or results occur. Where appropriate, the Data User is encouraged to consider collaboration or co-authorship with the authors. The Data User should realize that misinterpretation of data may occur if used out of context of the original study. While substantial efforts are made to ensure the accuracy of data and associated documentation, complete accuracy of data sets cannot be guaranteed. All data are made available \"as is.\" The Data User should be aware, however, that data are updated periodically and it is the responsibility of the Data User to check for new versions of the data. The data authors and the repository where these data were obtained shall not be liable for damages resulting from any use or misinterpretation of the data. Thank you.&#13;"), 
                    licensed = list(licenseName = "Creative Commons Attribution 4.0 International", 
                                    url = "https://spdx.org/licenses/CC-BY-4.0.html", identifier = "CC-BY-4.0")))
})



#Tests for add_maintenance function 

test_that('The maintenance function errors when missing mandatory identifier inputs.', {
  
  expect_error(add_maintenance(parent_element = parent_element), 
               "Please provide the status of your project or dataset.")
  
  expect_error(add_maintenance(parent_element = parent_element, status = "ongoing"),
               "Please provide the frequency of when this project or dataset is updated.")
  
})

test_that('The maintenance function adds the maintenance elements', {
  
  expect_equal(add_maintenance(parent_element = list(), status = "complete"), 
               list(maintenance = list(description = "complete")))
  
  expect_equal(add_maintenance(parent_element = list(), status = "ongoing",
                               update_frequency = "Data are updated annually at the end of the calendar year."),
               list(maintenance = list(description = "ongoing",
                                       maintenanceUpdateFrequency = "Data are updated annually at the end of the calendar year."))
)
  
})

#Tests for add_method function 

test_that('The method function errors when missing mandatory identifier inputs.', {
  
  expect_error(add_method(),
               'Please provide the document of which your methods information resides.')

})

test_that('The method function adds the method elements', {
  setwd("~/FlowWest/cvpiaEDIutils")
  expect_equal(add_method(methods_file = "tests/testthat/methods_test.docx",
                          instrumentation = "Thermometer"),
               list(sampling = NULL, 
                    methodStep = list(instrumentation = "Thermometer", 
                                                       software = NULL, 
                                      description = list(section = list("<title>Title 1</title>\n<para>\n    This is the first paragraph.\n  </para>\n<para>\n    This is the second paragraph.\n  </para>", 
                                                                                                          "<title>Title 2 </title>\n<para>\n    This is the third paragraph.\n  </para>\n<para>\n    This is the fourth paragraph.\n  </para>"), 
                                                                                           para = list()))))
})

# Tests for publication date 

test_that('The publication date function adds the appropriate elements.', {
  
  expect_equal(add_pub_date(parent_element = list(), date = "2020-08-19"),
               list(pubDate = "2020-08-19"))
  
  expect_equal(add_pub_date(parent_element = list()),
               list(pubDate = Sys.Date()))
})




