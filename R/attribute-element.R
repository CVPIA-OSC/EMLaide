#' @title Add Atribute  
#' @description adds an attribute element according to EML standards. Multiple attributes can be created. Will be appended to the attribute list.  
#' @param attribute_list The list of which all the attributes are appended. 
#' @param attribute_name The name	of a field in	a	data table. This is	often	a short	and/or cryptic name. It is recommended that the attribute names	be suitable	for	use	as a	variable,	e.g.,	composed of ASCII	characters,	and	that the	attribute names	match	the	column headers of	a	CSV	or other text table.	
#' @param attribute_label Optional. Used to provide	a	less ambiguous or	less cryptic alternative identification	than what	is provided	in attribute_name
#' @param attribute_defenition A precise and complete	definition of the attribute	being	documented.
#' @param storage_type An indicator	to processing	systems	as to	how	the	attribute	might	be represented in	a	system or	language,	but	is distinct	from the actual	expression of	the	domain of	the	attribute. Non system-specific examples	include: float,	integer	and	string.
#' @param measurement_scale The	type of	scale	from which values are	drawn	for	the	attribute. Must use one of the following types: nominal, ordinal, interval, ratio, or dateTime.
#' @param nominal_scale_definition Description for nominal measurement scale values. 
#' @param units The units assigned to this particular attributes values. 
#' @param number_type Can be defined as: real, natural, whole, or integer 
#' @param code_number The reference number to the code definition.
#' @param code_number_definition The explaination of a listed code number 
#' @param unit_precision How precise units are measured 
#' @param ISO_date_time YYYY-MM-DDThh:mm:ssTZD format 
#' @param date_time_precision What level of time is being measured. 
#' @param minimum Theoretical or allowable minimum value. 
#' @param maximum Theoretical or allowable maximum value.
#' @return The project or dataset list with an attribute list appended
#' @examples
#' Different measurement scale values will indicate different inputs: 
#' 
#' Nominal: Please provide measurement_scale_definition.
#' 
#' add_attribute_list(attribute_name = "site_id", attribute_definition = "Site id as used in sites table",
#'                    storage_type = "typeSystem = 'http://www.w3.org/2001/XMLSchema-datatypes'>string<",
#'                    measurement_scale = "nominal", nominal_scale_definition = "Site id as used in sites table.")
#' 
#' Ordinal: Please provide code_number and code_number_definition.
#' 
#' add_attribute_list(attribute_name = "q110", attribute_definition = "Q110 - preference for front yard",
#'                    storage_type = "typeSystem = 'http://www.w3.org/2001/XMLSchema-datatypes'>float<",
#'                    measurement_scale = "ordinal", code_number = "1.00", code_number_definition = "A desert landscape.",
#'                    code_number = "2.00", code_number_definition = "Mostly lawn.",
#'                    code_number = "3.00", code_number_definition = "Some lawn.")
#'  
#' Interval: Please provide units, unit_precision, number_type, minimum, and maximum.
#' 
#' add_attribute_list(attribute_name = "Count", attribute_definition = "Number of individuals observed",
#'                    measurement_scale = "interval",  storage_type = "integer", units = "number", unit_precision = "1",
#'                    number_type = "whole", minimum = "0")
#'                    
#' Ratio: Please provide units, unit_precision, and number_type.
#' 
#' add_attribute_list(attribute_name = "pH", attribute_definition = "pH of soil solution",
#'                    storage_type = "typeSystem = 'http://www.w3.org/2001/XMLSchema-datatypes'>float<",
#'                    measurement_scale = "ratio", units = "dimensionless", unit_precision = "0.01",
#'                    number_type = "real")
#'                    
#' dateTime: Please provide ISO_date_time, date_time_precision, minimum, and maximum
#' 
#' add_attribute_list(attribute_name = "Yrs", attribute_label = "Years",
#'                    attribute_definition = "Calendar year of the observation from years 1990 - 2010.",
#'                    storage_type = "integer", measurement_scale = "dateTime", ISO_date_time = "YYYY",
#'                    date_time_precision = "1", minimum = "1993", maximum = "2003")
#' @export

add_attribute <- function(attribute_list, attribute_name, attribute_label = NULL, attribute_definition,
                               storage_type, measurement_scale,
                               nominal_scale_definition = NULL,
                               units = NULL, number_type = NULL, code_number = NULL,
                               code_number_definition = NULL, unit_precision = NULL, ISO_date_time = NULL,
                               date_time_precision = NULL, minimum = NULL, maximum = NULL) {
  
  
  if (missing(attribute_name)) {stop('Please provide attribute name.', call. = FALSE)}
  if (missing(attribute_label)) {warning('No attribute label provided.', call. = FALSE)}
  if (missing(attribute_definition)) {stop('Please provide a brief definition of the attribute you are including.', call. = FALSE)}
  if (missing(storage_type)) {stop('Please provide a storage type.', call. = FALSE)}
  if (missing(measurement_scale)) {stop('Please provide a measurement scale')}


if (measurement_scale == "nominal") {
  attribute_list$attribute$measurementScale <- list(nominal = list(nonNumericDomain = list(textDomain = list(domain = nominal_scale_definition))))
}

attribute_list$attribute <- list(attributeName = attribute_name,
                                 attributeDefinition = attribute_definition,
                                 storageType = storage_type)

return(attribute_list)

}
 