#' @title Add Atribute List 
#' @description adds the attribute list element according to EML standards. Multiple attributes can be created.  
#' @param attribute_name The name	of a field in	a	data table. This is	often	a short	and/or cryptic name. It is recommended that the attribute names	be suitable	for	use	as a	variable,	e.g.,	composed of ASCII	characters,	and	that the	attribute names	match	the	column headers of	a	CSV	or other text table.	
#' @param attribute_label Optional. Used to provide	a	less ambiguous or	less cryptic alternative identification	than what	is provided	in attribute_name
#' @param attribute_defenition A precise and complete	definition of the attribute	being	documented.
#' @param storage_type An indicator	to processing	systems	as to	how	the	attribute	might	be represented in	a	system or	language,	but	is distinct	from the actual	expression of	the	domain of	the	attribute. Non system-specific examples	include: float,	integer	and	string.
#' @param measurement_scale The	type of	scale	from which values are	drawn	for	the	attribute. Must use one of the following types: nominal, ordinal, interval, ratio, or dateTime.
#' @param measurement_scale_description TODO
#' @param units TODO
#' @param number_type TODO
#' @return the project or dataset list with an attribute list appended
#' @examples TODO
#' @export

add_attribute_list <- function(attribute_name, attribute_label, attribute_definition,
                               storage_type, measurement_scale, measurement_scale_description,
                               units, number_type) {
  attributeList <- list(attribute = list(attributeName = attribute_name,
                                         attributeLabel = attribute_label,
                                         attributeDefinition = attribute_definition,
                                         storageType = storage_type))
  
  
  if (measurement_scale == "non-numeric") {
    attributeList$attribute$measurementScale <- list(nominal = list(nonNumericDomain = list(textDomain =
                                                list(domain = measurement_scale_description))))
  }
  
  if (measurement_scale == "numeric") {
    attributeList$attribute$measurementScale <- list(ratio = list(unit = list(standardUnit = units),
                                                                  numericDomain = list(numberType = number_type)))
  }
  
  
  
  

}