### COMMON UTILITIES

is_numeric_ = function(value) {
  str_detect(value, "^\\s*\\-?[0-9]+\\.?[0-9]*(E\\-?[0-9]+)?\\s*$")
}

is_numeric = memoise(is_numeric_)

### STRINGS

trim = function(string) {
  trimmed = str_trim(string)

  if(is.null(trimmed) || is.na(trimmed) || trimmed == "") trimmed = NA_character_

  return(trimmed)
}

### VECTORS

pad = function(original_vector, target_length, padding_values = NA) {
  current_length = length(original_vector)

  if(target_length < current_length) stop("Provided length is smaller than current length")

  return(
    c(original_vector, rep(padding_values, target_length - current_length))
  )
}

### REFERENCE DATA LINKS

reference_codes = function(domain, codelist) {
  return(
    paste0(
      "https://data.iotc.org/reference/latest/domain/", domain, "/#", codelist
    )
  )
}

### DATA TABLES

find_empty_rows = function(data_table) {
  return(
    data_table[
      rowSums(is.na(data_table)) == ncol(data_table),
      which = TRUE
    ]
  )
}

has_empty_rows = function(data_table) {
  return(
    length(
      find_empty_rows(data_table)
    ) > 0
  )
}

find_empty_columns = function(data_table) {
  return(
    which(
      colSums(is.na(data_table)) == nrow(data_table)
    )
  )
}

has_empty_columns = function(data_table) {
  return(
    length(
      find_empty_columns(data_table)
    ) > 0
  )
}
