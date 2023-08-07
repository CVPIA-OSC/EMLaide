parent_element <- list()
funding <- list(funder_name = "National Science Foundation",
                funder_identifier = "http://dx.doi.org/10.13039/100000001",
                award_number = "1656026",
                award_title = "LTER: Beaufort Sea Lagoons: An Arctic Coastal Ecosystem in Transition",
                award_url = "https://www.nsf.gov/awardsearch/showAward?AWD_ID=1656026")

person <- list(first_name = "Susan", 
               last_name = "Susanton", 
               email = "susanton@fake.com", 
               organization = "USFWS",
               role = "Project Lead")
# Tests for add project element 
test_that('project function errors when missing mandatory identifier inputs', {
  expect_error(create_project(project_title = "my project title", 
                              project_lead = list()),
               "argument \"funding_metadata\" is missing, with no default")
  
  expect_error(create_project(project_title = "my project title", 
                           funding_metadata = list()), 
               "argument \"project_lead\" is missing, with no default")
  expect_error(create_project(funding_metadata = funding, 
                              project_lead = person),
               "argument \"project_title\" is missing, with no default")
})

test_that('The create_project function adds the project elements', {
  
  expected_project = list(title = "This is a new project", 
                          personnel = list(individualName = list(givenName = "Susan", 
                                                                 surName = "Susanton"), 
                                           electronicMailAddress = "susanton@fake.com", 
                                           organizationName = "USFWS", 
                                           role = "Project Lead"),
                          award = list(list(funderName = "National Science Foundation", 
                                            title = "LTER: Beaufort Sea Lagoons: An Arctic Coastal Ecosystem in Transition", 
                                            funderIdentifier = "http://dx.doi.org/10.13039/100000001", 
                                            awardNumber = "1656026", 
                                            awardUrl = "https://www.nsf.gov/awardsearch/showAward?AWD_ID=1656026")))
  expect_equal(create_project(project_title = "This is a new project",
                           funding_metadata = funding,
                           project_lead = person), 
               expected_project)
  
})  

# Tests add_funding errors with incorrect inputs 
test_that('add_project has clear error messaging when inputs are not included ', {
  project_list <- list()
  expect_error(add_project(project_list, funding_metadata = funding),
               "please supply information about the project lead or run add_personnel first and the dataset creator will be used")
  expect_error(add_project(project_list, funding_metadata = funding, project_lead = person),
               "please supply a project title or run add_title first and the dataset title will be used")
  expect_error(add_project(project_list, project_lead = person))
})

test_that('add_project adds a project element to an EML document', {
  project_list <- list()
  expected_project = list(project = list(title = "This is a new project", 
                          personnel = list(individualName = list(givenName = "Susan", 
                                                                 surName = "Susanton"), 
                                           electronicMailAddress = "susanton@fake.com", 
                                           organizationName = "USFWS", 
                                           role = "Project Lead"),
                          award = list(list(funderName = "National Science Foundation", 
                                            title = "LTER: Beaufort Sea Lagoons: An Arctic Coastal Ecosystem in Transition", 
                                            funderIdentifier = "http://dx.doi.org/10.13039/100000001", 
                                            awardNumber = "1656026", 
                                            awardUrl = "https://www.nsf.gov/awardsearch/showAward?AWD_ID=1656026"))))
  expect_equal(add_project(project_list, project_title = "This is a new project",
              project_lead = person, funding_metadata = funding), 
              expected_project)
})

# Tests for create_funding function 
test_that('funding function errors when missing mandatory identifier inputs', {
  
  expect_error(create_funding(funder_identifier = "http://dx.doi.org/10.13039/100000001",
                           award_number = "1656026",
                           award_title = "LTER: Beaufort Sea Lagoons: An Arctic Coastal Ecosystem in Transition",
                           award_url = "https://www.nsf.gov/awardsearch/showAward?AWD_ID=1656026"), 
               "Please provide the funder_name")
  
  expect_warning(create_funding(funder_name = "National Science Foundation",
                           award_number = "1656026",
                           award_title = "LTER: Beaufort Sea Lagoons: An Arctic Coastal Ecosystem in Transition",
                           award_url = "https://www.nsf.gov/awardsearch/showAward?AWD_ID=1656026"),
               "Please provide the funder_identifier")
  
  expect_warning(create_funding(funder_name = "National Science Foundation",
                           funder_identifier = "http://dx.doi.org/10.13039/100000001",
                           award_title = "LTER: Beaufort Sea Lagoons: An Arctic Coastal Ecosystem in Transition",
                           award_url = "https://www.nsf.gov/awardsearch/showAward?AWD_ID=1656026"),
               "Please provide the award_number")
  
  expect_error(create_funding(funder_name = "National Science Foundation",
                           funder_identifier = "http://dx.doi.org/10.13039/100000001",
                           award_number = "1656026",
                           award_url = "https://www.nsf.gov/awardsearch/showAward?AWD_ID=1656026"),
               "Please provide the award_title")
  
  
  expect_warning(create_funding(funder_name = "National Science Foundation",
                             funder_identifier = "http://dx.doi.org/10.13039/100000001",
                             award_number = "1656026",
                             award_title = "LTER: Beaufort Sea Lagoons: An Arctic Coastal Ecosystem in Transition"),
                 "Please provide the award_url")

})

test_that('The create_funding function adds the funding elements', {
  
  expect_equal(create_funding(funder_name = "National Science Foundation",
                           funder_identifier = "http://dx.doi.org/10.13039/100000001",
                           award_number = "1656026",
                           award_title = "LTER: Beaufort Sea Lagoons: An Arctic Coastal Ecosystem in Transition",
                           award_url = "https://www.nsf.gov/awardsearch/showAward?AWD_ID=1656026"),
               list(funderName = "National Science Foundation", 
                    title = "LTER: Beaufort Sea Lagoons: An Arctic Coastal Ecosystem in Transition",
                    funderIdentifier = "http://dx.doi.org/10.13039/100000001", 
                    awardNumber = "1656026", 
                    awardUrl = "https://www.nsf.gov/awardsearch/showAward?AWD_ID=1656026"))
})

test_that('The default funding options are adding the correct default funding information', {
  expect_equal(create_funding(funder_name = "CDWR", 
                           award_title = "CDWR Fish Predation Grant"),
               list(funderName = "California Department of Water Resources",
                    funderIdentifier = "https://www.wikidata.org/wiki/Q5020440",
                    title = "CDWR Fish Predation Grant"))
  
  expect_equal(create_funding(funder_name = "USBR",  
                           award_title = "USBR California Water Grant"),
               list(funderName = "United States Bureau of Reclamation",
                    funderIdentifier = "https://www.wikidata.org/wiki/Q1010548",
                    title = "USBR California Water Grant"))
  
  expect_equal(create_funding(funder_name = "CDFW", 
                           award_title = "California Department of Fish and Wildlife Grant"),
               list(funderName = "California Department of Fish and Wildlife",
                    funderIdentifier = "https://www.wikidata.org/wiki/Q5020421",
                    title = "California Department of Fish and Wildlife Grant"))
})
