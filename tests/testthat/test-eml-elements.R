parent_element <- list()
# Tests for create_access function 
test_that('create_accesss function returns proper default and non default access lists', {
  access <- list(scope = "document",
                 order = "allowFirst", 
                 authSystem = "https://pasta.edirepository.org/authentication",
                 allow = list(principal = "public", permission = "read"))
  expect_equal(add_access(), access)
  expect_equal(add_access(allow_principal = "private", 
                          allow_permission = "none"), 
               list(scope = "document",
                    order = "allowFirst", 
                    authSystem = "https://pasta.edirepository.org/authentication",
                    allow = list(principal = "private", permission = "none")))
})
# Tests for create_title function 

test_that('dataset title length is between 7 and 20 words long', {
  
  passing_dataset <- create_title(title = "This title is at least 7 words and not over 20 words.", 
                                  short_name = "This is a short name for this dataset")
  
  expect_gte(length(unlist(strsplit(passing_dataset$title, split = " "))), 7)
  
  expect_error(
    create_title(title = "Too long title lorem ipsum dolor sit amet, consectetur adipiscing elit. Aliquam ante ipsum, consectetur quis iaculis eget, rhoncus rutrum velit. Donec cursus massa non.",
              short_name = "This is a short name for this dataset"),
    "Please make sure your title is between 7 and 20 words long."
  )
  
  expect_error(
    create_title(title = "Too short title.",
              short_name = "This is a short name for this dataset"),
    "Please make sure your title is between 7 and 20 words long."
  )
  
  expect_error(
    create_title(title = "This title is at least 7 words and not over 20 words.",
              short_name = "This is a short name and it is way way way way way way way way way way way way way way way way way way too long."),
    "Short name should not be longer than the dataset's title."
  )
  
  
})

test_that('dataset title function adds title and short name.',{
  
  title <- "This title will work because it is of a sufficient length"
  short_name <- "This is the short name"
  
  title_1 <- create_title(title = title, short_name = short_name)
  
  expect_equal(create_title(title = title, short_name = short_name),
               list(title = "This title will work because it is of a sufficient length", 
                    short_name = "This is the short name"))
  
  
})

#Tests for create_abstract function 

test_that('the dataset create_abstract function adds abstract', {
  abstract <- create_abstract(abstract = "abstract_test.docx")
  expect_equal(abstract,
               list(section = list(), para = list("\n    This is a test for the abstract. This abstract is of sufficient\n    length. Please make sure your abstract meets the word limit of\n    twenty words or more.\n  ")))
})

#Tests for create_keyword_set function 
test_that('the dataset create_keyword_set function adds the keyword set',{
  
  keyword_set_1 <- dplyr::tibble(keyword = list("dog", "cat", "cow", "pig"),
                                 keywordThesaurus = "LTER Controlled Vocabulary")
  
  keyword_set_2 <- dplyr::tibble(keyword = list("bear", "lion"), 
                                 keywordThesaurus = list(NA))
  
  first <- create_keyword_set(keyword_set_1)
  second <- create_keyword_set(keyword_set_2)
  
  expect_equal(first,
               list(list(keyword = list("dog", "cat", "cow", "pig"), 
                    keywordThesaurus = "LTER Controlled Vocabulary")))
  
  expect_equal(second, list(list(keyword = list("bear", "lion")))
  )
  
})

#Tests for create_personnel function 
test_that('personnel function errors when missing mandatory identifier inputs',  {
  role1 <- "creator"
  role2 <- "Data Manager"
  first_name <- "Susan"
  last_name <- "Susanton"
  email <- "susanton@fake.com"
  orcid <- "00110011"
  organization <- "USFWS"
 
  expect_error(create_person(role = role1, 
                             first_name = first_name, last_name = last_name,
                             organization = oganization),
               "Please supply an email.")
  
  expect_error(create_person(role = role1, 
                             first_name = first_name, email = email, 
                             organization = organization),
               "Please supply a last name.")
  
  expect_error(create_person(role = role1, 
                             last_name = last_name, email = email,
                             organization = organization), 
               "Please supply a first name.")
  
  expect_error(create_person(first_name = first_name, 
                             last_name = last_name, email = email, 
                             organization = organization), 
               "Please supply a role. Use 'creator' if you are the main originator of the dataset or project")
  
  expect_error(create_person(first_name = first_name, 
                             last_name = last_name, role = role, email = email), 
               "Please supply the name of the organization employing the personnel")
  
  expect_equal(create_person(first_name = first_name, 
                             last_name = last_name, email = email, role = role1, 
                             organization = organization, orcid = orcid),
               list(individualName = list(givenName = "Susan", surName = "Susanton"), 
                    electronicMailAddress = "susanton@fake.com", organizationName = "USFWS", 
                    `@id` = "00110011"))
  
  expect_equal(create_person(first_name = first_name,
                             last_name = last_name, email = email, role = role2, 
                             organization = organization),
               list(individualName = list(givenName = "Susan", surName = "Susanton"), 
                                           electronicMailAddress = "susanton@fake.com", 
                                           organizationName = "USFWS", role = "Data Manager"))
  
  creator_1 <- create_person(first_name = first_name, 
                             last_name = last_name, email = email, role = role1,
                             organization = organization)
  
  expect_equal(create_person(first_name = "Not Susan", 
                             last_name = "Smith", email = "free_cats@aol.com", 
                             organization = organization, role = role1),
               list(individualName = list(givenName = "Not Susan", surName = "Smith"), 
                    electronicMailAddress = "free_cats@aol.com", organizationName = "USFWS"))
})


#Tests for create_license function 

test_that('Intellectual rights function errors when missing mandatory identifier inputs', {
  
  expect_error(create_license(default_license = NULL, license_name = "Creative Commons",
                           license_identifier = "CC-BY-NC-SA-4.0",
                           intellectual_rights_descripiton = "The description goes here"), 
               "Please provide a url for the license.")
  
  expect_error(create_license(default_license = NULL, license_name = "Creative Commons",
                           license_url = "https://spdx.org/licenses/CC-BY-NC-SA-4.0.html",
                           intellectual_rights_descripiton = "The description goes here"), 
               "Please provide the license identifier.")
  
  expect_error(create_license(default_license = NULL, license_name = "Creative Commons",
                           license_identifier = "CC-BY-NC-SA-4.0",
                           license_url = "https://spdx.org/licenses/CC-BY-NC-SA-4.0.html"), 
               "Please provide a simplified description of the license.")
})

test_that('Intellectual rights function outputs the correct values when given the default inputs of "CCO" and "CCBY', {
  
  
  expect_equal(create_license("CCO"), 
               list(intellectualRights = list(para = "This data package is released to the \"public domain\" under Creative Commons CC0 1.0 \"No Rights Reserved\" (see: https://creativecommons.org/publicdomain/zero/1.0/). It is considered professional etiquette to provide attribution of the original work if this data package is shared in whole or by individual components. A generic citation is provided for this data package on the website https://portal.edirepository.org (herein \"website\") in the summary metadata page. Communication (and collaboration) with the creators of this data package is recommended to prevent duplicate research or publication. This data package (and its components) is made available \"as is\" and with no warranty of accuracy or fitness for use. The creators of this data package and the website shall not be liable for any damages resulting from misinterpretation or misuse of the data package or its components. Periodic updates of this data package may be available from the website. Thank you."), 
                    licensed = list(licenseName = "Creative Commons Zero v1.0 Universal", 
                                    url = "https://spdx.org/licenses/CC0-1.0.html", identifier = "CC0-1.0")))
  
  
  expect_equal(create_license("CCBY"),
               list(intellectualRights = list(para = "This information is released under the Creative Commons license - Attribution - CC BY (https://creativecommons.org/licenses/by/4.0/). The consumer of these data (\"Data User\" herein) is required to cite it appropriately in any publication that results from its use. The Data User should realize that these data may be actively used by others for ongoing research and that coordination may be necessary to prevent duplicate publication. The Data User is urged to contact the authors of these data if any questions about methodology or results occur. Where appropriate, the Data User is encouraged to consider collaboration or co-authorship with the authors. The Data User should realize that misinterpretation of data may occur if used out of context of the original study. While substantial efforts are made to ensure the accuracy of data and associated documentation, complete accuracy of data sets cannot be guaranteed. All data are made available \"as is.\" The Data User should be aware, however, that data are updated periodically and it is the responsibility of the Data User to check for new versions of the data. The data authors and the repository where these data were obtained shall not be liable for damages resulting from any use or misinterpretation of the data. Thank you."), 
                    licensed = list(licenseName = "Creative Commons Attribution 4.0 International", 
                                    url = "https://spdx.org/licenses/CC-BY-4.0.html", identifier = "CC-BY-4.0")))
})



#Tests for create_maintenance function 

test_that('The maintenance function errors when missing mandatory identifier inputs.', {
  
  expect_error(create_maintenance(), 
               "Please provide the status of your project or dataset.")
  
  expect_error(create_maintenance(status = "ongoing"),
               "Please provide the frequency of when this project or dataset is updated.")
  
})

test_that('The maintenance function adds the maintenance elements', {
  
  expect_equal(create_maintenance(status = "complete"), 
               list(description = "complete"))
  
  expect_equal(create_maintenance(status = "ongoing",
                                  update_frequency = "annually"),
               list(description = "ongoing",
                    maintenanceUpdateFrequency = "annually")
)
  
})

#Tests for create_method function 

test_that('The method function errors when missing mandatory identifier inputs.', {
  
  expect_error(create_method(),
               'Please provide the document of which your methods information resides.')

})

test_that('The method function adds the method elements', {
  method <- create_method(methods_file = "methods_test.docx",
             instrumentation = "Thermometer")
  expect_equal(method,
               list(sampling = NULL, methodStep = list(instrumentation = "Thermometer", 
                                                        software = NULL, description = list(section = list("<title>Title 1</title>\n<para>\n      This is the first paragraph.\n    </para>\n<para>\n      This is the second paragraph.\n    </para>", 
                                                                                                           "<title>Title 2 </title>\n<para>\n      This is the third paragraph.\n    </para>\n<para>\n      This is the fourth paragraph.\n    </para>"), 
                                                                                            para = list()))))
})

# Tests for publication date 

test_that('The publication date function adds the appropriate elements.', {
  
  expect_equal(create_pub_date(date = "2020-08-19"),
               "2020-08-19")
  
  expect_equal(create_pub_date(),
               Sys.Date())
})




