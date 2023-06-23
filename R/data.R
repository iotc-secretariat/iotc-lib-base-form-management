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

#' Effort mappings
#' @format
#' \describe{
#'    \item{CODE}{The new effort unit code}
#'    \item{EFFORT_CODE}{The IOTDB (legacy) effor unit code}
#'    \item{NAME_EN}{A description of the effort unit}
#' }
"EFFORT_MAPPINGS"
