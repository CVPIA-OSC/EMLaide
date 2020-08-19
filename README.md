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

### EML Heirarchy 
Each of these tools can be used to append a specific piece of metadata to the overall 
file. Each element of the hierarchy has specific inputs which are unique to that particular 
field. The organized list is as follows: 

```{r}
- Personnel 
- Title 
- Keyword Set 
- Abstract
- Intellectual Rights
- Funding Information 
- Maintenance 
- Mehtods 
- Coverage 
  - Geographic Coverage 
  - Temporal Coverage 
  - Taxonomic Coverage 
- Data Table 
  - Entity Name 
  - Entity Description
  - Attribute List 
  - Physical
```

### EDI 