#' @title Add Atribute  
#' @description Adds an attribute element according to EML standards. Multiple attributes can be created. Will be appended to the attribute list.  
#' @param attribute_name The name	of a field in	a	data table. This is	often	a short	and/or cryptic name. It is recommended that the attribute names	be suitable	for	use	as a	variable,	e.g.,	composed of ASCII	characters,	and	that the attribute names match the column headers of a CSV or other text table.	
#' @param attribute_defenition A precise and complete	definition of the attribute	being	documented.
#' @param storage_type An indicator	to processing	systems	as to	how	the	attribute	might	be represented in	a	system or	language,	but	is distinct	from the actual	expression of	the	domain of	the	attribute. Non system-specific examples	include: float,	integer	and	string.
#' @param measurement_scale The	type of	scale	from which values are	drawn	for	the	attribute. Must use one of the following types: nominal, ordinal, interval, ratio, or dateTime. Further explination of the choices and required inputs are described below.
#' @param attribute_label Optional. Used to provide	a	less ambiguous or	less cryptic alternative identification	than what	is provided	in \code{attribute_name}.
#' @param text_definition Description for non-numeric, text, measurement scale values. 
#' @param text_pattern A regular expression pattern constraining the attribute.
#' @param domain Input for the non-numeric measurement scales only. Please list either "text" or "enumerated".
#' @param units The units assigned to this attribute's values. 
#' @param number_type Can be defined as: real, natural, whole, or integer. 
#' @param code_definition A list of lists of your code, including all codes and their meanings. An example of how to append this is seen below.
#' @param unit_precision How precise units are measured.
#' @param date_time_format The format your date/time attribute is recorded in. ISO 8601 standard should be used (YYYY-MM-DD).
#' @param date_time_precision To what level time is being measured. 
#' @param minimum Theoretical or allowable minimum value. 
#' @param maximum Theoretical or allowable maximum value.
#' @section Measurement Scales: 
#' Different measurement scale values will indicate different inputs: 
#' 
#' \subsection {Non-numeric:} Please provide a domain to indicate if your attribute is text or enumerated.
#' 
#' \strong{Nominal:}
#' Used to define categorical scale attributes. If your attribute falls
#' under the domain of "text", please provide the inputs of \code{text_definition} and \code{text_pattern}.
#' If your attribute falls under the domain of "enumerated", please provide the input of \code{code_definition}.
#' 
#' \strong{Ordinal:}
#' Used to define ordered scale attributes. If your attribute falls under the domain 
#' of "text", please provide the inputs of \code{text_definition} and \code{text_pattern}. If your
#' attribute falls under the domain of "enumerated", please provide the input of \code{code_definition}. 
#'
#'\subsection {Numeric:}
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
  
  mandatory_missing_arg <- c("attribute_name", "attribute_definition", "storage_type", "measurement_scale")
  which_missing <- which(c(missing(attribute_name), missing(attribute_definition),
                           missing(storage_type), missing(measurement_scale)))
  
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
  
  if (measurement_scale == "nominal") {
    measurementScale <- add_nominal(domain = domain, text_definition = text_definition, text_pattern = text_pattern, code_definition = code_definition)
  }
  
  if (measurement_scale == "ordinal") {
    measurementScale <- add_ordinal(domain = domain, text_definition = text_definition, text_pattern = text_pattern,
                                    code_definition = code_definition)
  } 
  
  if (measurement_scale == "interval") {
    measurementScale <- add_interval(units = units, unit_precision = unit_precision, number_type = number_type,
                                     minimum = minimum, maximum = maximum)
  } 
  
  if (measurement_scale == "ratio") {
    measurementScale <- add_ratio(units = units, unit_precision = unit_precision, number_type = number_type,
                                  minimum = minimum, maximum = maximum)
  } 
  
  if (measurement_scale == "dateTime") {
    measurementScale <- add_datetime(date_time_format = date_time_format, date_time_precision = date_time_precision,
                                     minimum = minimum, maximum = maximum)
  }
  
  attribute$measurementScale <- measurementScale
  
  return(attribute) 
}
#nominal------
add_nominal <- function(domain, text_definition = NULL, text_pattern = NULL,
                        code_definition = NULL) {
  if (is.null(domain))
    {stop('Please provide a domain of text or enumerated and supply the remaining applicable inputs.', call. = FALSE)}
  if (domain == "text") {
    if (is.null(text_definition))
      {stop('Please provide the description for your measurement scale.', call. = FALSE)}
    if (is.null(text_pattern))
      {warning('No text pattern is provided. Please add if applicable.', call. = FALSE)}
    
    measurementScale <- list(nominal = list(nonNumericDesign = list(textDomain = list(definition = text_definition,
                                                                                      pattern = text_pattern))))
  } else {
    if (domain == "enumerated"){
      if (is.null(code_definition))
        {stop('Please provide a list of your enumerated codes and their definitions.', call. = FALSE)}
      
      measurementScale <- list(nominal = list(nonNumericDesign = list(enumeratedDomain =
                                                                        list(codeDefinition = code_definition))))
    } 
  }
  return(measurementScale)
}
#ordinal------
add_ordinal <- function(domain, text_definition, text_pattern,
                        code_definition) {
  if (is.null(domain))
    {stop('Please provide a domain of text or enumerated and supply the remaining applicable inputs.', call. = FALSE)}
  if (domain == "text") {
    if (is.null(text_definition))
      {stop('Please provide the description for your measurement scale.', call. = FALSE)}
    if (is.null(text_pattern))
      {warning('No text pattern is provided. Please add if applicable.', call. = FALSE)}
    
    measurementScale <- list(ordinal = list(nonNumericDesign = list(textDomain = list(definition = text_definition,
                                                                                      pattern = text_pattern))))
  } else {
    if (domain == "enumerated"){
      if (is.null(code_definition))
        {stop('Please provide a list of your enumerated codes and their definitions.', call. = FALSE)}
      
      measurementScale <- list(ordinal = list(nonNumericDesign = list(enumeratedDomain =
                                                                        list(codeDefinition = code_definition))))
    } 
  }
  return(measurementScale)
}
#interval------
add_interval <- function(units, unit_precision, number_type, minimum = NULL, maximum = NULL) {
  interval_error_arg <- c("units", "unit_precision", "number_type", "minimum", "maximum")
  interval_which_error <- which(c(is.null(units), is.null(unit_precision), is.null(number_type),
                                  is.null(minimum), is.null(maximum)))
  
  if (length(interval_which_error) > 0) {
    interval_error <- interval_error_arg[interval_which_error][1]
    interval_error_message <- switch(interval_error, units = "Please provide what units your measurement scale uses.",
                                     unit_precision = "Please provide what level of precision your measurements use.",
                                     number_type = "Please provide what type of numbers are being used.", 
                                     minimum = "Please provide a minimum theoretical value if applicable.",
                                     maximum = "Please provide a maximum theoretical value if applicable.")
    if (is.null(units) | is.null(unit_precision) | is.null(number_type)) {
      do.call(stop, list(interval_error_message, call. = FALSE))
    } else{
      if (is.null(minimum) | is.null(maximum)) {
        do.call(warning, list(interval_error_message, call. = FALSE))
      }
    } 
  } 
  
  measurementScale <- list(interval = list(standardUnit = units,
                                           precision = unit_precision,
                                           numericDomain = list(numberType = number_type,
                                                                bounds = list(minimum = minimum,
                                                                              maximum = maximum))))
  return(measurementScale)
}


#ratio-----
add_ratio <- function(units, unit_precision, number_type, minimum = NULL, maximum = NULL) {
  ratio_error_arg <- c("units", "unit_precision", "number_type", "minimum", "maximum")
  ratio_which_error <- which(c(is.null(units), is.null(unit_precision), is.null(number_type),
                               is.null(minimum), is.null(maximum)))
  
  if (length(ratio_which_error) > 0) {
    ratio_error <- ratio_error_arg[ratio_which_error][1]
    ratio_error_message <- switch(ratio_error, units = "Please provide what units your measurement scale uses.",
                                  unit_precision = "Please provide what level of precision your measurements use.",
                                  number_type = "Please provide what type of numbers are being used.", 
                                  minimum = "Please provide a minimum theoretical value if applicable.",
                                  maximum = "Please provide a maximum theoretical value if applicable.")
    if (is.null(units) | is.null(unit_precision) | is.null(number_type)) {
      do.call(stop, list(ratio_error_message, call. = FALSE))
    } else{
      if (is.null(minimum) | is.null(maximum)) {
        do.call(warning, list(ratio_error_message, call. = FALSE))
      } 
    }
  }
  measurementScale <- list(ratio = list(standardUnit = units,
                                        precision = unit_precision,
                                        numericDomain = list(numberType = number_type,
                                                             bounds = list(minimum = minimum,
                                                                           maximum = maximum))))
  
  
  
  return(measurementScale)
}



#dateTime ------
add_datetime <- function(date_time_format, date_time_precision, minimum, maximum) {
  
  dt_error_arg <- c("date_time_format", "date_time_precision", "minimum", "maximum")
  dt_which_error <- which(c(is.null(date_time_format), is.null(date_time_precision),
                            is.null(minimum), is.null(maximum)))
  
  if (length(dt_which_error) > 0) {
    dt_error <- dt_error_arg[dt_which_error][1]
    dt_error_message <- switch(dt_error,
                               date_time_format = "Please provide the correct format of which your date time attribute is in.",
                               date_time_precision = "Please provide the level of precision your date time attribute has.",
                               minimum = "Please provide the earliest date time used.",
                               maximum = "Please provide the latest date time used.")
    stop(dt_error_message, call. = FALSE)
  } 
  
  measurementScale <- list(dateTime = list(formatString = date_time_format, 
                                           dateTimePrecision = date_time_precision,
                                           dateTimeDomain = list(bounds = list(minimum = minimum,
                                                                               maximum = maximum))))
  return(measurementScale)
}
  
  