### SUPPORT FOR EXCEL DATA CHECKS

UPPERCASE_LETTERS    = str_to_upper(letters)
LETTERS_COMBINATIONS = expand.grid(UPPERCASE_LETTERS, UPPERCASE_LETTERS, stringsAsFactors = FALSE)

EXCEL_COLUMNS = append(UPPERCASE_LETTERS, sort(paste0(LETTERS_COMBINATIONS$Var1, LETTERS_COMBINATIONS$Var2)))

coordinates_to_cells = function(form, array_indexes) {
  result = data.table(COL = EXCEL_COLUMNS[as.integer(array_indexes[, 2] + first_data_column(form) - 1)],
                      ROW = as.integer(array_indexes[, 1] + first_data_row   (form) - 1))

  # Coordinates ([ COL, ROW ])
  #result[, INDEXES := paste0("[ ", EXCEL_COLUMNS[COL], ", ", ROW, " ]")]

  # Excel notation ([COLROW])
  result[, INDEXES := paste0("[", COL, ROW, "]")]

  return(result[order(ROW, COL)])
}

### COMMON UTILITIES

is_numeric_ = function(value) {
  str_detect(value, "^\\s*\\-?[0-9]+\\.?[0-9]*(E\\-?[0-9]+)?\\s*$")
}

is_numeric = memoise(is_numeric_)

### STRINGS

trim = function(string) {
  trimmed = str_trim(string)

  trimmed[is.na(trimmed) | str_length(trimmed) == 0] = NA_character_

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
