#' Fishery mappings
#' @format
#' \describe{
#'    \item{FISHERY_CODE}{The IOTCStatistics (legacy) fishery code}
#'    \item{GEAR_CODE}{The IOTDB (legacy) gear code}
#'    \item{MAIN_GEAR_CODE}{The IOTDB (legacy) main gear code}
#'    \item{SCHOOL_TYPE_CODE}{The IOTDB (legacy) school type code}
#'    \item{NAME_EN}{A description of the fishery}
#' }
"FISHERY_MAPPINGS"

#' Effort unit mappings
#' @format
#' \describe{
#'    \item{CODE}{The new effort unit code}
#'    \item{EFFORT_CODE}{The IOTDB (legacy) effor unit code}
#'    \item{NAME_EN}{A description of the effort unit}
#' }
"EFFORT_MAPPINGS"

#' Measure mappings
#' @format
#' \describe{
#'    \item{TYPE_OF_MEASUREMENT_CODE}{The code for the type of measurement (LN: length, WG: weight)}
#'    \item{CODE}{The new measure type code}
#'    \item{MEASURE_TYPE_CODE}{The IOTDB (legacy) measure type code}
#'    \item{NAME_EN}{A description of the measure type}
#' }
"MEASURE_MAPPINGS"
