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
                                                      title = "LTER: Beaufort Sea Lagoons: An Arctic Coastal Ecosystem in Transition", 
                                                      funderIdentifier = "http://dx.doi.org/10.13039/100000001", 
                                                      awardNumber = "1656026", 
                                                      description = "BLE LTER is supported by the National Science Foundation under award #1656026 (2017-08-01 to 2022-07-31).",
                                                      awardUrl = "https://www.nsf.gov/awardsearch/showAward?AWD_ID=1656026")))
  
  expect_equal(add_project(parent_element = parent_element,
                           project_title = "This is a new project",
                           award_information = funding,
                           project_personnel = person), 
               expected_project)
  
})  

# Tests for add_funding function 

test_that('funding function errors when missing mandatory identifier inputs', {
  
  expect_error(add_funding(funder_identifier = "http://dx.doi.org/10.13039/100000001",
                           award_number = "1656026",
                           award_title = "LTER: Beaufort Sea Lagoons: An Arctic Coastal Ecosystem in Transition",
                           award_url = "https://www.nsf.gov/awardsearch/showAward?AWD_ID=1656026",
                           funding_description = "BLE LTER is supported by the National Science Foundation under award #1656026 (2017-08-01 to 2022-07-31)." ), 
               "Please provide funders name.")
  
  expect_warning(add_funding(funder_name = "National Science Foundation",
                           award_number = "1656026",
                           award_title = "LTER: Beaufort Sea Lagoons: An Arctic Coastal Ecosystem in Transition",
                           award_url = "https://www.nsf.gov/awardsearch/showAward?AWD_ID=1656026",
                           funding_description = "BLE LTER is supported by the National Science Foundation under award #1656026 (2017-08-01 to 2022-07-31)." ),
               "Please provide funder identifier link.")
  
  expect_warning(add_funding(funder_name = "National Science Foundation",
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
                    title = "LTER: Beaufort Sea Lagoons: An Arctic Coastal Ecosystem in Transition",
                    funderIdentifier = "http://dx.doi.org/10.13039/100000001", 
                    awardNumber = "1656026", 
                    description = "BLE LTER is supported by the National Science Foundation under award #1656026 (2017-08-01 to 2022-07-31).",
                    awardUrl = "https://www.nsf.gov/awardsearch/showAward?AWD_ID=1656026"))
})

test_that('The default funding options are adding the correct default funding information', {
  expect_equal(add_funding(CVPIA_default_funder = "CDWR"),
               list(funderName = "CDWR",
                    title = "California Department of Water Resources Funding",
                    awardUrl = "https://www.wikidata.org/wiki/Q5020440"))
  
  expect_equal(add_funding(CVPIA_default_funder = "USBR"),
               list(funderName = "USBR",
                    title = "United States Bureau of Reclamation Funding",
                    funderIdentifier = "100006450", 
                    awardUrl = "https://www.wikidata.org/wiki/Q1010548"))
  
  expect_equal(add_funding(CVPIA_default_funder = "CDFW"),
               list(funderName = "CDFW",
                    title = "California Department of Fish and Wildlife Funding",
                    funderIdentifier = "100006238",
                    awardUrl = "https://www.wikidata.org/wiki/Q5020421"))
})
