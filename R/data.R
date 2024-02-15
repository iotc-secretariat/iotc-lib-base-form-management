#' RAV
#'
#' @format
#' \describe{
#'   \item{IOTC_NUMBER}{The numeric part (without leading zeros) of the IOTC number}
#'   \item{NAME}{The vessel name}
#'   \item{FLAG_CODE}{The vessel flag code}
#'   \item{GEAR_CODE}{The vessel gear code}
#'   \item{CURRENT}{1 if the records refer to a currently authorized vessel, 0 otherwise}
#'   \item{LAST_UPDATE}{The last recorded update for this vessel in the RAV}
#' }
#' @source <https://iotc.org/vessels/date>
"RAV"

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
