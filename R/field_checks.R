### COMMON FIELD CHECKS (SCALAR)

is_provided = function(value) {
  return(
    !is.null(value) &&
    !is.na(value) &&
   (!is.character(value) || trim(value) != "")
  )
}

is_integer = function(value) {
  return(
    is_provided(value) & is.integer(value)
  )
}

is_double = function(value) {
  return(
    is_provided(value) & is.double(value)
  )
}

is_year_valid = function(year) {
  current_year = as.integer(format(Sys.time(), "%Y"))

  return(
    is_integer(year) &&
    year <= current_year
  )
}

is_quarter_valid_ = function(quarter) {
  return(
    is_integer(quarter) &&
    quarter %in% 0:4
  )
}

is_quarter_valid = memoise(is_quarter_valid_)

is_month_valid_ = function(month) {
  return(
    is_integer(month) &&
    month %in% 1:12
  )
}

is_month_valid = memoise(is_month_valid_)

is_percentage_valid_ = function(percentage) {
  return(
    is_double(percentage) &&
    percentage >=0 &&
    percentage <= 100
  )
}

is_percentage_valid = memoise(is_percentage_valid_)

is_value_positive_ = function(value) {
  return(
    is_double(value) &&
    value >= 0
  )
}

is_value_positive = memoise(is_value_positive_)

is_value_strictly_positive = function(value) {
  return(
    is_double(value) &&
      value > 0
  )
}

check_mandatory = function(value, field) {
  if(!is_provided(value)) stop(paste0("Missing mandatory value for '", field, "'"))

  return(value)
}

should_be_integer = function(value, field) {
  if(!is_integer(value)) stop(paste0("Value for '", field, "' is not an integer"))

  return(value)
}

should_be_double = function(value, field) {
  if(!is_double(value)) stop(paste0("Value for '", field, "' is not a double"))

  return(value)
}

validate_year = function(year, field = "Reporting year") {
  if(!is_year_valid(year)) stop(paste0("The ", field, " value (", year, ") should not be NULL or set in the future"))

  return(year)
}

validate_quarter = function(quarter) {
  if(!is_quarter_valid(quarter)) stop(paste0("Quarter value (", quarter, ") should not be NULL and be one among { ", paste0(seq(0, 4, 1), collapse = ", "), " }"))

  return(quarter)
}

validate_percentage = function(percentage) {
  if(!is_percentage_valid(percentage)) stop(paste0("Percentage value (", percentage, ") should not be NULL and must be between 0 and 100 (included)"))

  return(percentage)
}

positive_value = function(value) {
  if(!is_value_positive(value)) stop(paste0("Value (", value, ") should not be NULL and must be greater than or equal to zero"))

  return(value)
}

strictly_positive_value = function(value) {
  if(!is_value_strictly_positive(value)) stop(paste0("Value (", value, ") should not be NULL and must be greater than zero"))

  return(value)
}
