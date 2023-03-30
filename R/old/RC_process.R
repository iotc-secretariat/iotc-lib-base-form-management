RC_read = function(filename) {
  RC = read_form(filename)

  RC_check_metadata(RC$form_metadata)
  RC_check_data(RC$form_data)
}

RC_check_metadata = function(form_metadata) {
  RC_form_details = form_details(form_metadata)

  if(RC_form_details$type != "1-RC")
    stop("Wrong type of form selected: ensure it's a form 1-RC")

  print(paste0("Loaded form ", RC_form_details$type, " - v", RC_form_details$version))

  validate_common_metadata(form_metadata)
}

RC_extract_and_standardize_data = function(form_data) {
  # Removes the first (empty) column
  form_data[, 1] = NULL

  # Extract the species codes from the header of the third row...
  species = as.character(form_data[3, 10:ncol(form_data)])
  species =
    lapply(
      species,
      function(c) {
        c = trim(c)

        return(ifelse(is.null(c), NA, c))
      }
    )

  species = as.character(species)

  # Actual data starts from row 4 onwards
  form_data = form_data[4:nrow(form_data)]

  # Updates the column names, assigning species codes to all columns after "COVERAGE"
  colnames(form_data) =
    append(
      c("QUARTER", "FISHERY", "IOTC_AREA", "RETAIN_REASON",  # Main stratum
        "DATA_TYPE", "DATA_SOURCE", "DATA_PROCESSING"     ,  # Original data
        "COVERAGE_TYPE", "COVERAGE"                       ), # Source coverage
      species
    )

  form_data[, (species) := lapply(.SD, function(v) { round(as.double(v), 2) }), .SDcols = species]

  return(form_data)
}

RC_check_data = function(form_data) {

}

RC_strata = function(RC_data) {
  return(RC_data[, 1:9])
}

RC_values = function(RC_data) {
  return(RC_data[, 10:ncol(RC_data)])
}

FOO = read_form("Form-1RC.xlsx")
BAZ = RC_extract_and_standardize_data(FOO$form_data)

RC_strata(BAZ)
V = RC_values(BAZ)

find_empty_columns(V)
find_empty_rows(V)

