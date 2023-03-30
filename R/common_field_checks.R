### COMMON FIELD CHECKS (SCALAR)

mandatory = function(value, field) {
  if(is.null(value) || is.na(value) || ( is.character(value) && trim(value) == "" ))
    stop(paste0("Missing mandatory value for '", field, "'"))

  return(value)
}

is_integer = function(value, field) {
  if(!is.na(value) && !is.null(value) && !is.integer(value))
    stop(paste0("Value for '", field, "' is not an integer"))

  return(value)
}

is_double = function(value, field) {
  if(!is.na(value) && !is.null(value) && !is.double(value))
    stop(paste0("Value for '", field, "' is not a double"))

  return(value)
}

validate_year = function(year) {
  current_year = as.integer(format(Sys.time(), "%Y"))

  if(year <= current_year) return(year)

  stop(paste0("The reporting year value (", year, ") should not be set in the future"))
}

validate_quarter = function(quarter) {
  if(quarter %in% 0:4) return(quarter)

  stop(paste0("Quarter value (", quarter, ") should be one among { ", paste0(seq(0, 4, 1), collapse = ", "), " }"))
}

validate_percentage = function(percentage) {
  if(percentage >=0 && percentage <= 100) return(percentage)

  stop(paste0("Percentage value (", percentage, ") should be between 0 and 100 (included)"))
}

positive_value = function(value) {
  if(value >= 0) return(value)

  stop(paste0("Value (", value, ") should be greater than or equal to zero"))
}

strictly_positive_value = function(value) {
  if(value > 0) return(value)

  stop(paste0("Value (", value, ") should be strictly positive"))
}
