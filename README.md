# cvpiaEDIutils

### Installation
This package can be installed using the following commands: 
```{r}
# install.packages("devtools")
devtools::install_github("FlowWest/cvpiaEDIutils")
```

### Usage 
This package provides tools to create a fully developed EML file which is consitent
to the EDI standards. The package was designed to work specifically with CVPIA,
but can be utilized by any persons wishing to create a working EML document that can
be submitted to EDI. Documentation for each function can be found in the Reference tab.
The full EML schema can be found on their [website](https://eml.ecoinformatics.org/schema/index.html).

### EML Hierarchy 
Each of these tools can be used to append a specific piece of metadata to the overall 
file. Each element of the hierarchy has specific inputs which are unique to that particular 
field. The organized list of broad topics is as follows: 

```{r}
- Personnel 
- Title 
- Keyword Set 
- Abstract
- Intellectual Rights
- Coverage 
- Data Table 
- Mehtods 
- Funding Information 
- Maintenance 
```

#### Personnel 
The `add_personnel` function allows you to append information on the creator or
the associated parties to your file. The creator would be any person or organization
who is responsible for the creation of the data, while an associated party would 
be any person or organization which is associated to the dataset. Multiple creators, 
associated parties, and a mixture of both can be appended to the EML file by calling
this function for each party which needs to be appended. 

#### Title 
The `add_title` function allows you to append the title and short name of the dataset 
to the file. Only one title and short name should be appended. The title should be 
fairly descriptive and between 7 and 20 words long. The short name must be less than
the number of words present in the title and is your opportunity to give viewers a more 
accessible name to the dataset. 

#### Keyword Set 
The `add_keyword_set` function allows you to append a keyword set to your file. 
The keyword set should include a list of words under a controlled vocabulary, 
that helps better identify your dataset and if it will be applicable to viewers.

#### Abstract 
The `add_abstract` function allows you to append the abstract of the dataset to 
your file. The abstract should include basic information on the dataset that gives 
a brief summary to the viewers of what they are observing from the data. 

#### Intellectual Rights
The `add_license` function allows you to append the licensing and usage information 
to your file. The `intellectual_rights_description` input calls for the description of
rights, which may be explained in the license you are using. It can also contain 
other requirements for use, other requirements for attribution, etc. Both the CC0 
and CCBY licenses can be chosen as defaults using this function. 

#### Coverage 
The `add_coverage` function allows you to append full coverage information to your 
file. The function itself includes inputs for both temporal and geographic coverage. 
Taxonomic coverage, however, is optional and can be appended using the `add_taxonomic_coverage` 
function. Chinook, Delta Smelt, Steelhead, White Sturgeon, and Green Sturgeon are
all default options, with full taxonomic coverage already present for easy appendage
as these are all frequent species being studied under CVPIA. 

#### Data Table 
The `add_data-table` function allows you to append the entity name, entity description,
physical element, and attribute list to your file. To incorporate the physical 
element, the `add_physical` function must be used. Additionally, and attribute list 
can be created using the `add_attribute` function. A list of attributes can be created 
to then append to the attribute list input in this function. 

#### Methods
The `add_method` function allows you to append the different scientific methods 
used in the collection process of your data. Multiple methods can be appended in 
a simillar fashion as personnel. The function must be called individually for each 
method, and then appended to a list, which will be inputed into the template. 

#### Funding Information 
The `add_funding` function allows you to append both the description of the funding 
you have recieved as well as the organization you recieved the funding from. 

#### Maintenance 
The `add_maintenance` function allows you to append the status of the dataset to
your file. The dataset can only be `complete` or `ongoing`. If the dataset is still
in progress, the frequency of which it is updated must be provided. 

