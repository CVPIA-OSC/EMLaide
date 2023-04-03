#' @title Attribute Storage Types
#' @description A set of common options for different attribute storage types. 
#' A helper data object to be used with \code{\link{create_attribute}}
#' @format A named list, keys are named the same as values.
#' @section Character Storage Types: 
#' 
#' \itemize{
#'   \item \strong{string}: The string datatype represents character strings in XML.
#'   Use \href{https://www.w3.org/TR/xmlschema-2/#string}{this} link for more information.
#' }
#' @section Locical Storage Types:
#' 
#' \itemize{ 
#'   \item \strong{boolean}: The boolean datatype has the value space required 
#'   to support the mathematical concept of binary-valued logic i.e {true, false}. 
#'   Use \href{https://www.w3.org/TR/xmlschema-2/#boolean}{this} link for more information.
#' }  
#' @section Numeric Storage Types:
#' 
#' \itemize{ 
#'   \item \strong{decimal}: The decimal datatype represents a subset of the real numbers, 
#'   which can be represented by decimal numerals. Out of the 3 numeric storage types,
#'   decimal is the most precise. It uses up to 28-29 significant digits.
#'    Use \href{https://www.w3.org/TR/xmlschema-2/#decimal}{this} link for more 
#'    information. Examples include: 2.00, -1.23, 0.0002, 210., 210.0
#'   \item \strong{float}: The float datatype is patterned after the IEEE 
#'   single-precision 32-bit floating point type. It can be further represented as 
#'   either a whole plus fractional digits (like decimal values) or as a mantissa 
#'   plus an exponent. It uses up to 7 significant digits.
#'   Use \href{https://www.w3.org/TR/xmlschema-2/#float}{this} link for more 
#'   information. Examples include decimals and/or 1.23E4, 1.2e-39
#'   \item \strong{double}: The double datatype is patterned after the IEEE 
#'   double-precision 64-bit floating point type. The level of precision is the 
#'   main difference between decimal, float, and double. It uses up to 15-16 significant digits.
#'   Use \href{https://www.w3.org/TR/xmlschema-2/#double}{this} link for more 
#'   information. Examples include 3.14, 2.718
#'  }
#' @section Time Storage Types: 
#' 
#' \itemize{ 
#'   \item \strong{duration}:The duration datatype represents a duration of time.
#'   The value space of duration is a six-dimensional space where the coordinates 
#'   designate the Gregorian year, month, day, hour, minute, and second components 
#'   defined in ISO 8601. Use \href{https://www.w3.org/TR/xmlschema-2/#duration}{this} link for more information.
#'   For example, to indicate a duration of 1 year, 2 months, 3 days, 10 hours, and 
#'   30 minutes, one would write: P1Y2M3DT10H30M. One could also indicate a 
#'   duration of minus 120 days as: -P120D.
#'   \item \strong{dateTime}:The dateTime datatype values may be viewed as objects 
#'   with integer-valued year, month, day, hour and minute properties, a 
#'   decimal-valued second property, and a boolean timezoned property. Use 
#'   \href{https://www.w3.org/TR/xmlschema-2/#dateTime}{this} link for more information.
#'   The typical format is: YYYY '-' MM '-' DD 'T' hh ':' mm ':' ss. An example 
#'   of this format is: 2002-10-10T12:00:00-05:00 .
#'   \item \strong{time}: The time datatype represents an instant of time that recurs every day.
#'   Use \href{https://www.w3.org/TR/xmlschema-2/#time}{this} link for more information.
#'   The time datatype follows the format hh:mm:ss.sss, i.e 06:20:00.098
#'   \item \strong{date}: The date datatype consists of top-open intervals of 
#'   exactly one day in length on the timelines of dateTime, beginning on the 
#'   beginning moment of each day (in each timezone), i.e. '00:00:00', up to but not 
#'   including '24:00:00'. Use \href{https://www.w3.org/TR/xmlschema-2/#date}{this} 
#'   link for more information. The date datatype follows the format 
#'   YYYY '-' MM '-' DD, i.e 2012-02-02.  
#'   \item \strong{gYearMonth}: The gYearMonth datatype represents a specific 
#'   gregorian month in a specific gregorian year. Use \href{https://www.w3.org/TR/xmlschema-2/#gYearMonth}{this}
#'   link for more information. The datatype gYearMonth follows the format of CCYY-MM.
#'   For example May 1999 would be inputted as 1999-05.
#'   \item \strong{gYear}: The gYear datatype represents a gregorian calendar year.
#'   Use \href{https://www.w3.org/TR/xmlschema-2/#gYear}{this} link for more information.
#'   The datatype gYear follows the format of CCYY. For example the year 1999 would 
#'   be inputted as 1999. 
#'   \item \strong{gMonthDay}: The gMonthDay datatype is a gregorian date that 
#'   recurs, specifically a day of the year such as the third of May.
#'   Use \href{https://www.w3.org/TR/xmlschema-2/#gMonthDay}{this} link for more 
#'   information. The gMonthDay datatype follows the format of MM-DD. For 
#'   example, March 3rd would be inputted as 03-03. 
#'   \item \strong{gDay}: The gDay datatype is a gregorian day that recurs, 
#'   specifically a day of the month such as the 5th of the month. Use 
#'   \href{https://www.w3.org/TR/xmlschema-2/#gDay}{this} link for more information.
#'   The gDay datatype follows the format -DD. For example, the 4th of a month would 
#'   be inputted as 04. 
#'   \item \strong{gMonth}: The gMonth datatype is a gregorian month that recurs every year.
#'   Use \href{https://www.w3.org/TR/xmlschema-2/#gMonth}{this} link for more information.
#'   The gMonth datatype follows the format -MM. For example, the month of March
#'   would be inputted as 03. 
#'  }
#' @examples 
#' storage_type$boolean
#' 
"storage_type"

#' @title Attribute Measurement Scales
#' @description The 5 options for measurement scales which can be appended to the dataset. 
#' A helper data object to be used with \code{\link{create_attribute}}.
#' @format A named list, keys are named the same as values.
#' @section Measurement Scales:
#' \itemize{ 
#'   \item \strong{nominal}: Used to define categorical scale attributes. 
#'   Nominal is used when numbers have only been assigned to a variable for the 
#'   purpose of categorizing the variable. An example of a nominal scale is 
#'   assigning the number 1 for male and 2 for female.
#'   \item \strong{ordinal}: Used to define ordered scale attributes. Ordinal is 
#'   used when the categories have a logical or ordered relationship to each other. 
#'   These types of scale allow one to distinguish the order of values, but not 
#'   the magnitude of the difference between values. An example of an ordinal 
#'   scale is a categorical survey where you rank a variable 1=good, 2=fair, 3=poor.
#'   \item \strong{interval}: Used to define interval scale attributes. Intervals 
#'   define data which consist of equidistant points on a scale. For example 
#'   temperature data, mark grading, IQ scale, etc. Intervals can be negative 
#'   while ratios cannot
#'   \item \strong{ratio}: Used to define ratio scale attributes. Ratios define 
#'   data which consists not only of equidistant points but also has a meaningful zero 
#'   point, which allows the ratio to have meaning. For example measurement heights, 
#'   flow rates, weight, length, etc. 
#'   \item \strong{dateTime}: Used to define date and time attributes. DateTime 
#'   is used when the values fall on the Gregorian calendar system.  DateTime values 
#'   are special because they have properties of interval values (most of the 
#'   time it is legitimate to treat them as interval values by converting them 
#'   to a duration from a fixed point) but they sometimes only behave as ordinals 
#'   (because the calendar is not predetermined, for some dateTime values one can
#'   only find out the order of the points and not the magnitude of the duration 
#'   between those points). The most encompassing format is: YYYY-MM-DDThh:mm:ss. 
#' }
#' @examples
#' measurement_scale$nominal #"nominal"
#' 
"measurement_scale"

#' @title Attribute Number Types 
#' @description The 4 options for number types that can be appended to the dataset.
#' A helper data object to be used with \code{\link{create_attribute}}.
#' @format A named list, keys are named the same as values. 
#' @section Number Types: 
#' \itemize{ 
#'   \item \strong{natural}:The number type for this attribute consists of the 
#'   'natural' numbers, otherwise known as the counting numbers: 1, 2, 3, 4, etc.
#'   \item \strong{whole}: The number type for this attribute consists of the 
#'   'whole' numbers, which are the natural numbers plus the zero value: 
#'   0, 1, 2, 3, 4, etc.
#'   \item \strong{integer}: The number type for this attribute consists of the
#'   'integer' numbers, which are the natural numbers, plus the zero value, plus
#'   the negatives of the natural numbers: ..., -4, -3, -2, -1, 0, 1, 2, 3, 4, etc.
#'   \item \strong{real}: The number type for this attribute consists of the
#'   'real' numbers, which contains both the rational numbers that can be expressed
#'   as fractions and the irrational numbers that can not be expressed as 
#'   fractions (such as the square root of 2).
#'  }
#' @examples 
#' number_type$natural #"natural"
#' number_type$whole   
"number_type"

#' @title CVPIA_funders
#' @description A list of CVPIA_funders that can be appended to the funding element. 
#' @format A named list, keys are names of CVPIA funding organizations and keys contain 
#' information about each funder. 
#' @section CVPIA_funders:
#' \itemize{
#'     \item \strong{USBR}
#'     \item \strong{CDWR}
#'     \item \strong{CDFW}
#'     \item \strong{USFWS}
#' }
#' @examples 
#' CVPIA_funders$USBR 
#'
#'
"CVPIA_funders"

#' @title CVPIA Common Species 
#' @description A named list containing the names and complete taxonomic information of 
#' CVPIA common species. \code{CVPIA_common_species} is used by \code{add_taxonomic_coverage}
#' to generate the taxonomic coverage element according the the EML schema.
#' @format A named list, keys are names of CVPIA common species and values are nested
#' lists containing complete taxonomic information for each species.  
#' 
#' @section CVPIA Common Species:
#' \itemize{
#'  \item \strong{chinook}
#'  \item \strong{steelhead}
#'  \item \strong{delta_smelt}
#'  \item \strong{white_sturgeon}
#'  \item \strong{green_sturgeon}
#' }
#'
#' @examples 
#' CVPIA_common_species$chinook 
"CVPIA_common_species"

#' @title standard_units 
#' @description A list of all valid standard units accepted by EML schema. This list is used 
#' to by \code{create_attribute()}. As \code{create_attribute()} filters through all attribute components 
#' it will check that the units are in the standard unit list. 
#' @format A list.   
#' @examples 
#' "meters" %in% standard_units
#' "meter" %in% standard_units
#' "m" %in% standard_units
"standard_units"