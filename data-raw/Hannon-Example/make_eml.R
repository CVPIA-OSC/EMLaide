library(EML)
library(tidyverse)

metadata_file <- "data-raw/Hannon-Example/20200609 EDI_metadata_template Sac Monitoring - snorkel.docx"
title <- "Sacramento River Snorkel Monitoring"
abstract <- set_TextType(file = "data-raw/Hannon-Example/abstract.md")
intellectualRights <- "This work is licensed under a Creative Commons Attribution 4.0 International License."
pubDate <- "2020-06-22"

# what is the keyword thesaurus and is it a required element of EDI?
keywordSet <- list(
  list(
    keyword = list("Sacramento River", 
                   "Salmonid Habitat", 
                   "Restoration Projects", 
                   "Effectiveness Monitoring")
  ))

contact <-
  list(
    individualName = "John Hannon",
    electronicMailAddress = "jhannon@usbr.gov",
    address = HF_address,
    organizationName = "Harvard Forest",
    phone = "000-000-0000")   