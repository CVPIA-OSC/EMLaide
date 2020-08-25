parent_element <- list()
title <- read_excel("data-raw/template/template.xlsx", sheet = "title")
parent_element <- add_title(parent_element = parent_element, title = title$title, 
                            short_name = title$short_name)

personnel_table <- read_excel("data-raw/template/template.xlsx", sheet = "personnel")
for (i in 1:nrow(personnel_table)) {
  current <- personnel_table[i, ]
  parent_element<- add_personnel(parent_element = parent_element, 
                                  first_name = current$first_name, 
                                  last_name = current$last_name, 
                                  email = current$email, 
                                  role = current$role, 
                                  orcid = current$orcid)
}
parent_element <- add_pub_date(parent_element = parent_element)
pubDate <- list()
rights <- list()
abstract <- list()
keyword <- list()
coverage <- list()
method <- list()
dataTable <- list()

dataset <- list(dataset = parent_element)
eml <- list(
  packageId = "uuid::UUIDgenerate()",
  system = "uuid", # type of identifier
  dataset = parent_element)

write_eml(dataset, "eml.xml")
eml_validate("eml.xml")

                creator = aaron,
                pubDate = pubDate,
                intellectualRights = intellectualRights,
                abstract = abstract,
                associatedParty = associatedParty,
                keywordSet = keywordSet,
                coverage = coverage,
                contact = contact,
                methods = methods,
                dataTable = dataTable)
