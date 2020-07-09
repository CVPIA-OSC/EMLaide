# This script executes an EMLassemblyline workflow.

library(EMLassemblyline)

#these libraries are helpful but not necessary to run EMLassemblyline
library(dplyr)
library(tidyr)
library(stringr)

#------------------------------------------------------------
# parameters to enter directly into the R script that do not go into templates

file_path <- "."

dataset_title <- 'Biological invasions: a field synopsis, systematic review, and database of the literature'

file_names <- c('all-database-records.csv',
                'papers-in-the-field-synopsis.csv',
                'papers-in-the-systematic-review.csv')

file_labels <- c('all database records',
                 'papers in the field synopsis',
                 'paper in the systematic review')

file_descriptions <- c('Database of all the publication records that were collected. This includes all results after an initial screening of title and abstract, but before evaluation using the full text, and includes the SCOPUS record.',
                       'database of the publication records that were used in the ﬁeld synopsis',
                       'database of the publication records that were used in the systematic review')


quote_character <- rep("\"",3)

temp_cov <- c('2010-06-06', '2012-12-31')

maint_desc <- "completed"

user_id <- "EDI"

package_id <- "edi.540.1"

#geographic information
#use either this description or a .txt file generated via geographic template

geog_descr <- 'Global'

coord_south <- -80.0
coord_east <- 180.0
coord_north <- 80.0
coord_west <- -180.0

geog_coord <- c(coord_north, coord_east, coord_south, coord_west)


#-----------------------------------------------------------------
#generate templates

# given a filepath create template metadata files for the core pieces. These
# are sometimes filled in with col headers
# the following are created:
# abstract.txt
# additional_info.txt
# intellectual_rights.txt
# keywords.txt.
# methods.txt
# personnel.txt
template_core_metadata(
  path = file_path,
  license = 'CCBY'
)

# given some datasets determines the col_types and creates appropriate template files
# for these, output if formatted as attributes_filename.txt
# some are encoded incorrectly
# attributes_all-database-records.txt.
# attributes_papers-in-the-field-synopsis.txt.
# attributes_papers-in-the-systematic-review.txt.
# custom_units.txt 
template_table_attributes(
  path = file_path,
  data.path = file_path,
  data.table = file_names
)

# edit the attribute templates first before running categorical variables template

# this function looks for attribute_* and looks for categorical and create appropriate
# catvar_*
template_categorical_variables(
  path = file_path,
  data.path = file_path
)


# example for taxonomic coverage, geographic coverage would be similar if 
# generated from csv file of locations

template_taxonomic_coverage(
  path = file_path,
  data.path = file_path,
  taxa.table = 'taxon.csv',
  taxa.col = 'taxon',
  taxa.authority = c(3,11),
  taxa.name.type = 'scientific'
)

make_eml(
  path = file_path,
  data.path = file_path,
  eml.path = file_path,
  dataset.title = dataset_title,
  temporal.coverage = temp_cov,
  geographic.description = geog_descr,
  geographic.coordinates = geog_coord,
  maintenance.description = maint_desc,
  data.table = file_names,
  data.table.name = file_labels,
  data.table.description = file_descriptions,
  data.table.quote.character = quote_character,
  #other.entity = 'protocol.pdf',
  #other.entity.description = 'methods used for this study',
  user.id = 'edi',
  user.domain = 'EDI',
  package.id = package_id
)
