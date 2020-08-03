#' @title Attribute Storage Types
#' @description A set of common options for different attribute storage types. 
#' A helper data object to be used with \code{\link{add_attribute}}
#' @format A named list, keys are named the same as values.
#' @section Character Storage Types: 
#' 
#' \itemize
#'   \item{\strong{string}}: The string datatype represents character strings in XML.
#'   Use \href{https://www.w3.org/TR/xmlschema-2/#string}{this} link for more information. 
#' @section Locical Storage Types:
#' 
#' \itemize 
#'   \item{\strong{boolean}}: The boolean datatype has the value space required 
#'   to support the mathematical concept of binary-valued logic i.e {true, false}. 
#'   Use \href{https://www.w3.org/TR/xmlschema-2/#boolean}{this} link for more information.
#' @section Numeric Storage Types:
#' 
#' \itemize 
#'   \item{\strong{decimal}}: The decimal datatype represents a subset of the real numbers, 
#'   which can be represented by decimal numerals. Use 
#'   \href{https://www.w3.org/TR/xmlschema-2/#decimal}{this} link for more information.
#'   \item{\strong{float}}: The float datatype is patterned after the IEEE 
#'   single-precision 32-bit floating point type. Use \href{https://www.w3.org/TR/xmlschema-2/#float}{this} 
#'   link for more information.
#'   \item{\strong{double}}: The double datatype is patterned after the IEEE 
#'   double-precision 64-bit floating point type. Use \href{https://www.w3.org/TR/xmlschema-2/#double}{this} 
#'   link for more information.
#' @section Time Storage Types: 
#' 
#' \itemize 
#'   \item{\strong{duration}}:The duration datatype represents a duration of time.
#'   The value space of duration is a six-dimensional space where the coordinates 
#'   designate the Gregorian year, month, day, hour, minute, and second components 
#'   defined in ISO 8601. Use \href{https://www.w3.org/TR/xmlschema-2/#duration}{this} link for more information.
#'   \item{\strong{dateTime}}:The dateTime datatype values may be viewed as objects 
#'   with integer-valued year, month, day, hour and minute properties, a 
#'   decimal-valued second property, and a boolean timezoned property. Use 
#'   \href{https://www.w3.org/TR/xmlschema-2/#dateTime}{this} link for more information.
#'   \item{\strong{time}}: The time datatype represents an instant of time that recurs every day.
#'   Use \href{https://www.w3.org/TR/xmlschema-2/#time}{this} link for more information.
#'   \item{\strong{date}}: The date datatype consists of top-open intervals of 
#'   exactly one day in length on the timelines of dateTime, beginning on the 
#'   beginning moment of each day (in each timezone), i.e. '00:00:00', up to but not 
#'   including '24:00:00'. Use \href{https://www.w3.org/TR/xmlschema-2/#date}{this} 
#'   link for more information.
#'   \item{\strong{gYearMonth}}: The gYearMonth datatype represents a specific 
#'   gregorian month in a specific gregorian year. Use \href{https://www.w3.org/TR/xmlschema-2/#gYearMonth}{this}
#'   link for more information.
#'   \item{\strong{gYear}}: The gYear datatype represents a gregorian calendar year.
#'   Use \href{https://www.w3.org/TR/xmlschema-2/#gYear}{this} link for more information.
#'   \item{\strong{gMonthDay}}: The gMonthDay datatype is a gregorian date that 
#'   recurs, specifically a day of the year such as the third of May.
#'   Use \href{https://www.w3.org/TR/xmlschema-2/#gMonthDay}{this} link for more information.
#'   \item{\strong{gDay}}: The gDay datatype is a gregorian day that recurs, 
#'   specifically a day of the month such as the 5th of the month. Use 
#'   \href{https://www.w3.org/TR/xmlschema-2/#gDay}{this} link for more information.
#'   \item{\strong{gMonth}} The gMonth datatype is a gregorian month that recurs every year.
#'   Use \href{https://www.w3.org/TR/xmlschema-2/#gMonth}{this} link for more information.
#' @examples 
#' storage_type$integer #"integer"
#' 
"storage_type"

#' @title Attribute Measurement Scales
#' @description The 5 options for measurement scales which can be appended to the dataset. 
#' A helper data object to be used with \code{\link{add_attribute}}.
#' @format A named list, keys are named the same as values.
#' @section Measurement Scales:
#' \itemize 
#'   \item{\strong{nominal}}: Used to define categorical scale attributes.
#'   \item{\strong{ordinal}}: Used to define ordered scale attributes.
#'   \item{\strong{interval}}: Used to define interval scale attributes.
#'   \item{\strong{ratio}}: Used to define ratio scale attributes.
#'   \item{\strong{dateTime}}: Used to define date and time attributes.
#' 
#' @examples
#' measurement_scale$nominal #"nominal"
#' 
"measurement_scale"


#' @title Attribute Number Types 
#' @description The 4 options for number types that can be appended to the dataset.
#' A helper data object to be used with \code{\link{add_attribute}}.
#' @format A named list, keys are named the same as values. 
#' @section Number Types: 
#' \itemize 
#'   \item{\strong{natural}}:The number type for this attribute consists of the 
#'   'natural' numbers, otherwise known as the counting numbers: 1, 2, 3, 4, etc.
#'   \item{\strong{whole}}: The number type for this attribute consists of the 
#'   'whole' numbers, which are the natural numbers plus the zero value: 
#'   0, 1, 2, 3, 4, etc.
#'   \item{\strong{integer}}: The number type for this attribute consists of the
#'   'integer' numbers, which are the natural numbers, plus the zero value, plus
#'   the negatives of the natural numbers: ..., -4, -3, -2, -1, 0, 1, 2, 3, 4, etc.
#'   \item{\strong{real}}: The number type for this attribute consists of the
#'   'real' numbers, which contains both the rational numbers that can be expressed
#'   as fractions and the irrational numbers that can not be expressed as 
#'   fractions (such as the square root of 2).
#'  
#' @examples 
#' number_type$natural #"natural"
#'   
"number_type"
