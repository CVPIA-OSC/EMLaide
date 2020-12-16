parent_element <- list()
# Tests for add project element 
test_that('project function errors when missing mandatory identifier inputs', {
  expect_error(add_project(parent_element = parent_element,
                           award_information = list(),
                           project_personnel = list()),
               "Please provide the project title")
  
  expect_error(add_project(parent_element = parent_element, 
                           project_title = "my project title", 
                           project_personnel = list()),
               "Please provide the award information.")
  
  expect_error(add_project(parent_element = parent_element,
                           project_title = "my project title", 
                           award_information = list()), 
               "Please provide the project personnel")
})

# test_that('arguments are of the correct type', {
#   expect_error(add_project(parent_element = parent_element,
#                            project_title = "my project title", 
#                            award_information = "my cool award",
#                            personnel_info = list(individualName = list(givenName = "Sue",
#                                                                        surName ="Knew"),
#                                                  organizationName = "Something",
#                                                  electronicMailAddress = "i@didnt.know",
#                                                  role = "Manager")), 
#                "Please provide a list that includes the award title and funderName.")
#   
#   expect_error(add_project(parent_element = parent_element,
#                            project_title = "my project title", 
#                            award_information = list(title = "Money up for grabs",
#                                                     funderName = "Bank",
#                                                     awardNumber = "000",
#                                                     funderIdentifier = "Funder 1",
#                                                     awardUrl = "awardforme.com"),
#                            personnel_info = "Sue Knew"), 
#                "Please provide a list that includes the project personnel's name, organization, and position for this project")
# })

test_that('arguments have all the nessisary required values', {
  expect_error(add_project(parent_element = parent_element,
                           project_title = "my project title", 
                           award_information = list(title = "Money up for grabs",
                                                    awardNumber = "000",
                                                    funderIdentifier = "Funder 1",
                                                    awardUrl = "awardforme.com"),
                           project_personnel = list(individualName = list(givenName = "Sue",
                                                                       surName ="Knew"),
                                                 organizationName = "Something",
                                                 electronicMailAddress = "i@didnt.know",
                                                 role = "Manager")), 
               "Please provide a list that includes the funderName.")
  
  expect_error(add_project(parent_element = parent_element,
                           project_title = "my project title", 
                           award_information = list(funderName = "Bank",
                                                    awardNumber = "000",
                                                    funderIdentifier = "Funder 1",
                                                    awardUrl = "awardforme.com"),
                           project_personnel = list(individualName = list(givenName = "Sue",
                                                                       surName ="Knew"),
                                                 organizationName = "Something",
                                                 electronicMailAddress = "i@didnt.know",
                                                 role = "Manager")), 
               "Please provide a list that includes the title for this award.")
  
  expect_error(add_project(parent_element = parent_element,
                           project_title = "my project title", 
                           award_information = list(title = "Money up for grabs",
                                                    funderName = "Bank",
                                                    awardNumber = "000",
                                                    funderIdentifier = "Funder 1",
                                                    awardUrl = "awardforme.com"),
                           project_personnel = list(organizationName = "Something",
                                                    electronicMailAddress = "i@didnt.know",
                                                    role = "Manager")), 
               "Please provide a name for the project personnel.")
  
  expect_error(add_project(parent_element = parent_element,
                           project_title = "my project title", 
                           award_information = list(title = "Money up for grabs",
                                                    funderName = "Bank",
                                                    awardNumber = "000",
                                                    funderIdentifier = "Funder 1",
                                                    awardUrl = "awardforme.com"),
                           project_personnel = list(individualName = list(givenName = "Sue",
                                                                          surName ="Knew"),
                                                    electronicMailAddress = "i@didnt.know",
                                                    role = "Manager")), 
               "Please provide an organization for the project personnel.")
  
  expect_error(add_project(parent_element = parent_element,
                           project_title = "my project title", 
                           award_information = list(title = "Money up for grabs",
                                                    funderName = "Bank",
                                                    awardNumber = "000",
                                                    funderIdentifier = "Funder 1",
                                                    awardUrl = "awardforme.com"),
                           project_personnel = list(individualName = list(givenName = "Sue",
                                                                          surName ="Knew"),
                                                    organizationName = "Something",
                                                    electronicMailAddress = "i@didnt.know")), 
               "Please provide a role for the project personnel.")
})

test_that('The add_project function adds the project elements', {
  funding <- add_funding(funder_name = "National Science Foundation",
                         funder_identifier = "http://dx.doi.org/10.13039/100000001",
                         award_number = "1656026",
                         award_title = "LTER: Beaufort Sea Lagoons: An Arctic Coastal Ecosystem in Transition",
                         award_url = "https://www.nsf.gov/awardsearch/showAward?AWD_ID=1656026",
                         funding_description = "BLE LTER is supported by the National Science Foundation under award #1656026 (2017-08-01 to 2022-07-31).")
  
  person <- add_personnel(parent_element = parent_element,
                          first_name <- "Susan", 
                          last_name <- "Susanton", 
                          email <- "susanton@fake.com", 
                          role <- "Data Manager", 
                          organization <- "USFWS")
  
  expected_project = list(project = list(title = "This is a new project",
                                     personnel = list(associatedParty = list(individualName = list(givenName = "Susan", 
                                                                                                   surName = "Susanton"), 
                                                                             electronicMailAddress = "susanton@fake.com", 
                                                                             organizationName = "USFWS", 
                                                                             role = "Data Manager")),
                                     award = list(funderName = "National Science Foundation", 
                                                  funderIdentifier = "http://dx.doi.org/10.13039/100000001", 
                                                  awardNumber = "1656026", 
                                                  title = "LTER: Beaufort Sea Lagoons: An Arctic Coastal Ecosystem in Transition", 
                                                  description = "BLE LTER is supported by the National Science Foundation under award #1656026 (2017-08-01 to 2022-07-31).",
                                                  awardUrl = "https://www.nsf.gov/awardsearch/showAward?AWD_ID=1656026")))
  
  expect_equal(add_project(parent_element = parent_element,
                           project_title = "This is a new project",
                           award_information = funding,
                           project_personnel = person), 
               expected_project)
               
})  

# Tests for add funding element 
# Tests for add_funding function 

test_that('funding function errors when missing mandatory identifier inputs', {
  
  expect_error(add_funding(funder_identifier = "http://dx.doi.org/10.13039/100000001",
                           award_number = "1656026",
                           award_title = "LTER: Beaufort Sea Lagoons: An Arctic Coastal Ecosystem in Transition",
                           award_url = "https://www.nsf.gov/awardsearch/showAward?AWD_ID=1656026",
                           funding_description = "BLE LTER is supported by the National Science Foundation under award #1656026 (2017-08-01 to 2022-07-31)." ), 
               "Please provide funders name.")
  
  expect_error(add_funding(funder_name = "National Science Foundation",
                           award_number = "1656026",
                           award_title = "LTER: Beaufort Sea Lagoons: An Arctic Coastal Ecosystem in Transition",
                           award_url = "https://www.nsf.gov/awardsearch/showAward?AWD_ID=1656026",
                           funding_description = "BLE LTER is supported by the National Science Foundation under award #1656026 (2017-08-01 to 2022-07-31)." ),
               "Please provide funder identifier link.")
  
  expect_error(add_funding(funder_name = "National Science Foundation",
                           funder_identifier = "http://dx.doi.org/10.13039/100000001",
                           award_title = "LTER: Beaufort Sea Lagoons: An Arctic Coastal Ecosystem in Transition",
                           award_url = "https://www.nsf.gov/awardsearch/showAward?AWD_ID=1656026",
                           funding_description = "BLE LTER is supported by the National Science Foundation under award #1656026 (2017-08-01 to 2022-07-31)." ),
               "Please provide your award number.")
  
  expect_error(add_funding(funder_name = "National Science Foundation",
                           funder_identifier = "http://dx.doi.org/10.13039/100000001",
                           award_number = "1656026",
                           award_url = "https://www.nsf.gov/awardsearch/showAward?AWD_ID=1656026",
                           funding_description = "BLE LTER is supported by the National Science Foundation under award #1656026 (2017-08-01 to 2022-07-31)." ),
               "Please provide the title of your award.")
  
  
  expect_warning(add_funding(funder_name = "National Science Foundation",
                             funder_identifier = "http://dx.doi.org/10.13039/100000001",
                             award_number = "1656026",
                             award_title = "LTER: Beaufort Sea Lagoons: An Arctic Coastal Ecosystem in Transition",
                             funding_description = "BLE LTER is supported by the National Science Foundation under award #1656026 (2017-08-01 to 2022-07-31)." ),
                 "Please provide the award url.")
  
  expect_warning(add_funding(funder_name = "National Science Foundation",
                             funder_identifier = "http://dx.doi.org/10.13039/100000001",
                             award_number = "1656026",
                             award_title = "LTER: Beaufort Sea Lagoons: An Arctic Coastal Ecosystem in Transition",
                             award_url = "https://www.nsf.gov/awardsearch/showAward?AWD_ID=1656026"),
                 "Please provide the description of the funding recieved.")
  
  
})

test_that('The add_funding function adds the funding elements', {
  
  expect_equal(add_funding(funder_name = "National Science Foundation",
                           funder_identifier = "http://dx.doi.org/10.13039/100000001",
                           award_number = "1656026",
                           award_title = "LTER: Beaufort Sea Lagoons: An Arctic Coastal Ecosystem in Transition",
                           award_url = "https://www.nsf.gov/awardsearch/showAward?AWD_ID=1656026",
                           funding_description = "BLE LTER is supported by the National Science Foundation under award #1656026 (2017-08-01 to 2022-07-31)."),
               list(funderName = "National Science Foundation", 
                    funderIdentifier = "http://dx.doi.org/10.13039/100000001", 
                    awardNumber = "1656026", 
                    title = "LTER: Beaufort Sea Lagoons: An Arctic Coastal Ecosystem in Transition", 
                    description = "BLE LTER is supported by the National Science Foundation under award #1656026 (2017-08-01 to 2022-07-31).",
                    awardUrl = "https://www.nsf.gov/awardsearch/showAward?AWD_ID=1656026"))
})

# Tests for add project personnel element (IF I make a seperate function)