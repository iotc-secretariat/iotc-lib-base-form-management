### COMMON UTILITIES

### STRINGS

trim = function(string) {
  trimmed = str_trim(string)

  if(is.null(trimmed) | is.na(trimmed) | trimmed == "") trimmed = NA

  return(trimmed)
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
