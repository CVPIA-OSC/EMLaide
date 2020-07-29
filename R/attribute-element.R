#' @title Add Atribute  
#' @description Adds an attribute element according to EML standards. Multiple attributes can be created. Will be appended to the attribute list.  
#' @param attribute_name The name	of a field in	a	data table. This is	often	a short	and/or cryptic name. It is recommended that the attribute names	be suitable	for	use	as a	variable,	e.g.,	composed of ASCII	characters,	and	that the	attribute names	match	the	column headers of	a	CSV	or other text table.	
#' @param attribute_defenition A precise and complete	definition of the attribute	being	documented.
#' @param storage_type An indicator	to processing	systems	as to	how	the	attribute	might	be represented in	a	system or	language,	but	is distinct	from the actual	expression of	the	domain of	the	attribute. Non system-specific examples	include: float,	integer	and	string.
#' @param measurement_scale The	type of	scale	from which values are	drawn	for	the	attribute. Must use one of the following types: nominal, ordinal, interval, ratio, or dateTime. Further explination of the choices and required inputs are described below.
#' @param attribute_label Optional. Used to provide	a	less ambiguous or	less cryptic alternative identification	than what	is provided	in attribute_name
#' @param text_definition Description for non-numeric, text, measurement scale values. 
#' @param text_pattern A regular expression pattern constraining the attribute.
#' @param domain Input for the non-numeric measurement scales only. Please list either "text" or "enumerated".
#' @param units The units assigned to this attribute's values. 
#' @param number_type Can be defined as: real, natural, whole, or integer. 
#' @param code_definition A list of lists of your code, including all codes and their meanings. An example of how to append this is seen below 
#' @param unit_precision How precise units are measured.
#' @param date_time_format The format your date/time attribute is recorded in. ISO 8601 standard should be used (YYYY-MM-DD)
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
#' under the domain of text, please provide the inputs of \code{text_definition} and \code{text_pattern}.
#' If your attribute falls under the domain of enumerated, please provide the input of \code{code_definition}.
#' 
#' \strong{Ordinal:}
#' Used to define ordered scale attributes. If your attribute falls under the domain 
#' of text, please provide the inputs of \code{text_definition} and \code{text_pattern}. If your
#' attribute falls under the domain of enumerated, please provide the input of \code{code_definition}. 
#'
#'\subsection{Numeric:}
#'
#'\strong{Interval:}
#' Used to define interval scale attributes. Please provide the inputs of \code{units}, 
#' \code{unit_precision}, \code{number_type}, \code{minimum}, and \code{maximum}.
#' 
#' \strong{Ratio:}
#' Used to define ratio scale attributes. Please provide the inputs of \code{units}, 
#' \code{unit_precision}, \code{number_type}, \code{minimum}, and \code{maximum}.
#' 
#' \strong{dateTime:}
#' Used to define date and time attributes. Please provide the inputs of \code{date_time_format}, 
#' \code{date_time_precision}, \code{minimum}, and \code{maximum}.
#' @return The project or dataset list with an attribute list appended
#' @examples
#' Nominal(text):
#' add_attribute(attribute_name = "site_id", attribute_definition = "Site id as used in sites table",
#'               storage_type = "string", measurement_scale = "nominal", domain= "text",
#'               text_definition = "Site id as used in sites table.")
#'
#' Nominal(enumerated):
#' code_def_1 = list(code = "yes", definition = "has been captured previously")
#' code_def_2 = list(code = "no", definition = "has not been captured previously")
#' code_definition = list(code_def_1, code_def_2)
#' add_attribute(attribute_name = "Recap", attribute_definition = "Has the Turtle been captured and tagged previously",
#'               storage_type = "text", measurement_scale = "nominal", domain = "enumerated",
#'               code_definition = code_definition)
#'               
#' Ordinal(text):
#' add_attribute(attribute_name = "LatitudeDD", attribute_definition = "Latitude",
#'               storage_type = "coordinate", measurement_scale = "ordinal",
#'               domain= "text", text_definition = "Latitude")
#'               
#' Ordinal(enumerated): 
#' code_def_1 = list(code = "0", definition = "0 insects per meter of branch")
#' code_def_2 = list(code = "1", definition = "1-10 insects per meter")
#' code_def_3 = list(code = "2", definition = "11 – 100 insects per meter")
#' code_def_2 = list(code = "3", definition = "more than 100 insects per meter")
#' code_definition = list(code_def_1, code_def_2, code_def_3, code_def_4)
#' add_attribute(attribute_name = "hwa", attribute_definition = "Hemlock woolly adelgid density per meter of branch",
#'               storage_type = "number", measurement_scale = "ordinal",
#'               domain = "enumerated", code_definition = code_definition)
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
#' add_attribute(attribute_name = "Yrs", attribute_definition = "Calendar year of the observation from years 1990 - 2010.",
#'               storage_type = "integer", measurement_scale = "dateTime", attribute_label = "Years",
#'               date_time_format = "YYYY", date_time_precision = "1", minimum = "1993", maximum = "2003")
#' @export

add_attribute <- function(attribute_name, attribute_definition, storage_type,
                          measurement_scale, attribute_label = NULL, text_definition = NULL,
                          text_pattern = NULL, domain = NULL, units = NULL,
                          number_type = NULL, code_definition = NULL,
                          unit_precision = NULL, date_time_format = NULL,
                          date_time_precision = NULL, minimum = NULL, maximum = NULL) {
  
#error messages for mandatory inputs ---------
  
    mandatory_missing_arg <- c("attribute_name", "attribute_definition", "storage_type", "measurement_scale")
    which_missing <- which(c(missing(attribute_name), missing(attribute_definition), missing(storage_type), missing(measurement_scale)))
    
    if (length(which_missing) > 0) {
      missing <- mandatory_missing_arg[which_missing][1]
      warning_message <- switch(missing, attribute_name = "Please provide attribute name.",
                                attribute_definition = "Please provide a brief definition of the attribute you are including.",
                                storage_type = "Please provide a storage type.",
                                measurement_scale = "Please provide a measurement scale.")
      stop(warning_message, call. = FALSE)
    }
  
  attribute <- list(attributeName = attribute_name,
                    attributeDefinition = attribute_definition,
                    storageType = storage_type)
  
  if (missing(attribute_label)) {warning('No attribute label provided.', call. = FALSE)}
  if (!is.null(attribute_label)){
    attribute$attributeLabel <- attribute_label
  }
  
  text <- list(nonNumericDesign = list(textDomain = list(definition = text_definition,
                                                         pattern = text_pattern)))
  
  enumerated <- list(nonNumericDesign = list(enumeratedDomain = list(codeDefinition = code_definition)))
  
  unit <-  list(standardUnit = units,
                precision = unit_precision,
                numericDomain = list(numberType = number_type,
                                     bounds = list(minimum = minimum,
                                                   maximum = maximum)))
if (measurement_scale == "nominal") {
  if (missing(domain))
    {stop('Please provide a domain of text or enumerated and supply the remaining applicable inputs.', call. = FALSE)}
  if (domain == "text") {
    
      # text_missing_arg <- c("text_definition", "text_pattern")
      # which_missing <- which(c(missing(text_definition), missing(text_pattern)))
      # 
      # if (length(which_missing) > 0) {
      #   missing <- missing_arg[which_missing][1]
      #   error_message <- switch(missing,  text_definition = "x is missing")
      #   stop(error_message, call. = FALSE)
      #   warning_message <- switch(missing, text_pattern = "y is missing.")
      #   warning(warning_message, call. = FALSE)
      # }
      # 
    if (missing(text_definition))
      {stop('Please provide the description for your nominal measurment scale.', call. = FALSE)}
    if (missing(text_pattern))
      {warning('No text pattern is provided. Please add if applicable.', call. = FALSE)}

  attribute$measurementScale$nominal <- text
  
  } else {
    if (domain == "enumerated"){
      if (missing(code_definition))
        {stop('Please provide a list of your enumerated, nominal codes and their definitions.', call. = FALSE)}
      
      attribute$measurementScale <- enumerated
    }
  }
} else { 
if (measurement_scale == "ordinal") {
  if (missing(domain))
  {stop('Please provide a domain of text or enumerated and supply the remaining applicable inputs.', call. = FALSE)}
  if (domain == "text") {
    if (missing(text_definition))
      {stop('Please provide the description for your ordinal measurment scale.', call. = FALSE)}
    if (missing(text_pattern))
      {warning('No text pattern is provided. Please add if applicable.', call. = FALSE)}
    
    attribute$measurementScale$ordinal <- text
  } else {
    if (domain == "enumerated"){
      if (missing(code_definition))
        {stop('Please provide a list of your enumerated, ordinal codes and their definitions.', call. = FALSE)}
      
      attribute$measurementScale <- enumerated
    }
  }
} else {
if (measurement_scale == "interval") {
  if (missing(units))
  {stop('Please provide what units your interval measurement scale uses.', call. = FALSE)}
  if (missing(unit_precision))
  {stop('Please provide what level of precision your interval measurments use.', call. = FALSE)}
  if (missing(number_type))
  {stop('Please provide what type of numbers are being used.', call. = FALSE)}
  if (missing(minimum))
  {warning('Please provide a minimum theoretical value if applicable.', call. = FALSE)}
  if (missing(maximum))
  {warning('Please provide a maximum theoretical value if applicable.', call. = FALSE)}
  
  # interval_missing_arg <- c("units", "unit_precision", "number_type", "minimum", "maximum")
  # interval_which_missing <- which(c(missing(units), missing(unit_precision),
  #                                   missing(number_type), missing(minimum), missing(maximum)))
  # 
  # if (length(interval_which_missing) > 0) {
  #   interval_missing <- interval_missing_arg[interval_which_missing][1]
  #   interval_error_message <- switch(interval_missing, units = "Please provide attribute name.",
  #                             unit_precision = "Please provide a brief definition of the attribute you are including.",
  #                             number_type = "Please provide a storage type.")
  #   stop(interval_error_message, call. = FALSE)
  #   interval_warning_message <- switch(interval_missing, minimum = "hi",
  #                                      maximum = "hi")
  #   warning(interval_warning_message, call. = FALSE)
  # }
  
  
  attribute$measurementScale$interval <- unit
} else {
  
if (measurement_scale == "ratio") {
  if (missing(units))
    {stop('Please provide what units your ratio measurement scale uses.', call. = FALSE)}
  if (missing(unit_precision))
    {stop('Please provide what level of precision your ratio measurments use.', call. = FALSE)}
  if (missing(number_type))
    {stop('Please provide what type of numbers are being used.', call. = FALSE)}
  if (missing(minimum))
    {warning('Please provide a minimum theoretical value if applicable.', call. = FALSE)}
  if (missing(maximum))
    {warning('Please provide a maximum theoretical value if applicable', call. = FALSE)}
  
  attribute$measurementScale$ratio <- unit
} else {
  
if (measurement_scale == "dateTime") {
  if (missing(date_time_format))
    {stop('Please provide the correct format of which your date time attribute is in.', call. = FALSE)}
  if (missing(date_time_precision))
    {stop('Please provide the level of precision your date time attribute has.', call. = FALSE)}
  if (missing(minimum))
    {stop('Please provide the earliest date time used.', call. = FALSE)}
  if (missing(maximum))
    {stop('Please provide the latest date time used.', call. = FALSE)}
  
  attribute$measurementScale <- list(dateTime = list(formatString = date_time_format, 
                                                     dateTimePrecision = date_time_precision,
                                                     dateTimeDomain = list(bounds = list(minimum = minimum,
                                                                                         maximum = maximum))))
}
  }
    }
      }
        }
  
 return(attribute) 
}
 