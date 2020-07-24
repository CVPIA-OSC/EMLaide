#' @title Add Atribute  
#' @description Adds an attribute element according to EML standards. Multiple attributes can be created. Will be appended to the attribute list.  
#' @param attribute_name The name	of a field in	a	data table. This is	often	a short	and/or cryptic name. It is recommended that the attribute names	be suitable	for	use	as a	variable,	e.g.,	composed of ASCII	characters,	and	that the	attribute names	match	the	column headers of	a	CSV	or other text table.	
#' @param attribute_label Optional. Used to provide	a	less ambiguous or	less cryptic alternative identification	than what	is provided	in attribute_name
#' @param attribute_defenition A precise and complete	definition of the attribute	being	documented.
#' @param storage_type An indicator	to processing	systems	as to	how	the	attribute	might	be represented in	a	system or	language,	but	is distinct	from the actual	expression of	the	domain of	the	attribute. Non system-specific examples	include: float,	integer	and	string.
#' @param measurement_scale The	type of	scale	from which values are	drawn	for	the	attribute. Must use one of the following types: nominal, ordinal, interval, ratio, or dateTime. Further explination of the choices and required inputs are described below.
#' @param text_definition Description for non-numeric, text, measurement scale values. 
#' @param text_pattern A regular expression pattern constraining the attribute.
#' @param domain Input for the non-numeric measurement scales only. Please list either "text" or "enumerated".
#' @param units The units assigned to this attribute's values. 
#' @param number_type Can be defined as: real, natural, whole, or integer. 
#' @param code The reference for the code definition.
#' @param code_definition The explaination of the listed code. 
#' @param unit_precision How precise units are measured.
#' @param date_time_format The format your date/time attribute is recorded in.
#' @param date_time_precision To what level time is being measured. 
#' @param minimum Theoretical or allowable minimum value. 
#' @param maximum Theoretical or allowable maximum value.
#' @section Measurement Scales: 
#' Different measurement scale values will indicate different inputs: 
#' 
#' \subsection{Non-numeric:} Please provide a domain to indicate if your attribute is text or enumerated.
#' 
#' \strong{Nominal:}
#' Used to define categorical scale attributes. If your attribute falls
#' under the domain of text, please provide the inputs of text_definition and text_pattern.
#' If your attribute falls under the domain of enumerated, please provide the inputs
#' of code and code_definition.
#' 
#' \strong{Ordinal:}
#' Used to define ordered scale attributes. If your attribute falls under the domain 
#' of text, please provide the inputs of text_definition and text_pattern. If your
#' attribute falls under the domain of enumerated, please provide the inputs of 
#' code and code_definition. 
#'
#'\subsection{Numeric:}
#'
#'\strong{Interval:}
#' Used to define interval scale attributes. Please provide the inputs of units, 
#' unit_precision, number_type, minimum, and maximum.
#' 
#' \strong{Ratio:}
#' Used to define ratio scale attributes. Please provide the inputs of units, 
#' unit_precision, number_type, minimum, and maximum.
#' 
#' \strong{dateTime:}
#' Used to define date and time attributes. Please provide the inputs of date_time_format, 
#' date_time_precision, minimum, and maximum.
#' @return The project or dataset list with an attribute list appended
#' @examples
#' Nominal(text):
#' add_attribute(attribute_name = "site_id", attribute_definition = "Site id as used in sites table",
#'               storage_type = "string",measurement_scale = "nominal", domain= "text"
#'               text_definition = "Site id as used in sites table.")
#'
#' Nominal(enumerated):
#' add_attribute(attribute_name = "Recap", attribute_definition = "Has the Turtle been captured and tagged previously",
#'               storage_type = "text", measurement_scale = "nominal", domain = "enumerated",
#'               code = "yes", code_definition = "captured previously, No")
#'               
#' Ordinal(text):
#' add_attribute(attribute_name = "LatitudeDD", attribute_definition = "Latitude",
#'               storage_type = "coordinate", measurement_scale = "ordinal",
#'               domain= "text", text_definition = "Latitude")
#'               
#' Ordinal(enumerated): 
#' add_attribute(attribute_name = "hwa", attribute_definition = "Hemlock woolly adelgid density per meter of branch",
#'               storage_type = "number", measurement_scale = "ordinal",
#'               domain = "enumerated", code = c("0", "1", "2", "3"),
#'               code_definition = c("0 insects per meter of branch", "1-10 insects per meter",
#'                                   "11 – 100 insects per meter", "more than 100 insects per meter")
#'               
#' Interval:
#' add_attribute(attribute_name = "Count", attribute_definition = "Number of individuals observed",
#'                    measurement_scale = "interval",  storage_type = "integer", units = "number",
#'                    unit_precision = "1", number_type = "whole", minimum = "0")
#'                    
#' Ratio: 
#' add_attribute(attribute_name = "pH", attribute_definition = "pH of soil solution",
#'                    storage_type = "float", measurement_scale = "ratio",
#'                    units = "dimensionless", unit_precision = "0.01", number_type = "real")
#'                    
#' dateTime:
#' add_attribute(attribute_name = "Yrs", attribute_label = "Years",
#'               attribute_definition = "Calendar year of the observation from years 1990 - 2010.",
#'               storage_type = "integer", measurement_scale = "dateTime", date_time_format = "YYYY",
#'               date_time_precision = "1", minimum = "1993", maximum = "2003")
#' @export

add_attribute <- function(attribute_name, attribute_label = NULL, attribute_definition,
                          storage_type, measurement_scale, text_definition = NULL,
                          text_pattern = NULL, domain = NULL, units = NULL,
                          number_type = NULL, code = NULL, code_definition = NULL,
                          unit_precision = NULL, date_time_format = NULL,
                          date_time_precision = NULL, minimum = NULL, maximum = NULL) {
  
  
  if (missing(attribute_name)) {stop('Please provide attribute name.', call. = FALSE)}
  if (missing(attribute_label)) {warning('No attribute label provided.', call. = FALSE)}
  if (missing(attribute_definition)) {stop('Please provide a brief definition of the attribute you are including.', call. = FALSE)}
  if (missing(storage_type)) {stop('Please provide a storage type.', call. = FALSE)}
  if (missing(measurement_scale)) {stop('Please provide a measurement scale', call. = FALSE)} 
  
  attribute <- list(attributeName = attribute_name,
                    attributeDefinition = attribute_definition,
                    storageType = storage_type)
  
  text <- list(nonNumericDesign = list(textDomain = list(definition = text_definition,
                                                       pattern = text_pattern)))
  enumerated <- list(nonNumericDesign = list(enumeratedDomain = list(codeDefinition = list(code = code,
                                                                                         definition = code_definition))))
  unit <-  list(standardUnit = units,
              precision = unit_precision,
              numericDomain = list(numberType = number_type,
                                   bounds = list(minimum = minimum,
                                                 maximum = maximum)))
if (measurement_scale == "nominal") {
  if (domain == "text") {
  attribute$measurementScale$nominal <- text
  } else {
    if (domain == "enummerated"){
      attribute$measurementScale <- enumerated
    }
  }
} else { 
if (measurement_scale == "ordinal") {
  if (domain == "text") {
    attribute$measurementScale$ordinal <- text
  } else {
    if (domain == "enummerated"){
      attribute$measurementScale <- enumerated
    }
  }
} else {
if (measurement_scale == "interval") {
  attribute$measurementScale$interval <- unit
} else {
  
if (measurement_scale == "ratio") {
  attribute$measurementScale$ratio <- unit
} else {
  
if (measurement_scale == "dateTime") {
  attribute$measurementScale <- list(dateTime = list(formatString = date_time_format, 
                                                     dateTimePrecision = date_time_precision,
                                                     dateTimeDomain = list(bounds = list(minimum = minimum,
                                                                                         maximum = maximum))))
}}}}}
  
 return(attribute) 
}
 