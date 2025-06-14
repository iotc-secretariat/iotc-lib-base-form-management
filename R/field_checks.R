### COMMON FIELD CHECKS (SCALAR)

is_provided = function(value) {
  return(
    !is.null(value) &
    !is.na(value) &
   (!is.character(value) | trim(value) != "")
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
    is.integer(year) & year <= current_year
  )
}

is_quarter_valid = function(quarter) {
  return(
    quarter %in% 0:4
  )
}

is_month_valid = function(month) {
  return(
    month %in% 1:12
  )
}

is_percentage_valid = function(percentage) {
  return(
    is.numeric(percentage) &
    percentage >=   0 &
    percentage <= 100
  )
}

is_value_positive = function(value) {
  return(
    is.numeric(value) & value >= 0
  )
}

is_value_strictly_positive = function(value) {
  return(
    is.numeric(value) & value > 0
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

is_latitude_valid = function(latitude) {
  return(is_provided(latitude) & abs(latitude) < 90)
}

is_longitude_valid = function(longitude) {
  return(is_provided(longitude) & abs(longitude) < 180)
}

are_coordinates_valid = function(latitude, longitude) {
  return(
    is_latitude_valid(latitude) &
    is_longitude_valid(longitude)
  )
}

is_IO = function(latitude, longitude) {
  return(latitude < 35 & longitude >= 20 & longitude <= 150 )
}

to_CWP_grid_1 = function(latitude, longitude) {
  quadrant = ifelse(latitude > 0, ifelse(longitude > 0, "1", "4"),
                    ifelse(longitude > 0, "2", "3"))

  lat2 = str_sub(paste0("00",  floor(abs(latitude))),  -2)
  lon3 = str_sub(paste0("000", floor(abs(longitude))), -3)

  return(
    paste0(
      "5", quadrant, lat2, lon3
    )
  )
}

