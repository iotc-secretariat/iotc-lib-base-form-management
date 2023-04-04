verify_form_4SF_multiple = function(form) {
  form_details = extract_metadata_1DI(form)$form_details

  form_type    = form_details$type
  form_version = form_details$version

  if(check_mandatory(form_type, "Form type") != "4-SF-multiple")
    stop(paste0("Please provide a valid form 4-SF-multiple (current form type: ", form_type, " - required: 4-SF-multiple)"))

  if(check_mandatory(form_version, "Form version") != "1.0.0")
    stop(paste0("Please provide a valid form 4-SF-multiple (current form version: ", form_version, " - required: 1.0.0)"))
}

extract_metadata_4SF_multiple = function(form) {
  metadata = common_metadata(form$form_metadata)

  metadata$comment = comments(form$form_metadata, 23)

  return(metadata)
}

extract_strata_and_records_4SF_multiple = function(form) {
  form_metadata = form$form_metadata
  form_data     = form$form_data

  strata = form_data[4:nrow(form_data)][, 2:18]
  colnames(strata) = c("MONTH", "FISHERY_CODE", "GRID_CODE", "SPECIES_CODE", "SEX_CODE", "TYPE_OF_FATE_CODE", "FATE_CODE", "ESTIMATION_CODE",
                       "DATA_TYPE_CODE", "DATA_SOURCE_CODE", "DATA_PROCESSING_CODE", "DATA_RAISING_CODE",
                       "COVERAGE_TYPE_CODE", "COVERAGE",
                       "TYPE_OF_MEASUREMENT_CODE", "MEASUREMENT_TYPE_CODE", "MEASUREMENT_TOOL_CODE")

  strata[, COVERAGE   := round(as.numeric(COVERAGE),   0)]

  records = form_data[4:nrow(form_data), 19:ncol(form_data)]

  # Rounds size-class (low / high) to lower integer
  records = records[, 1:2 := lapply(.SD, function(value) {
    return(floor(as.numeric(value)))
  }), .SDcols = 1:2]

  # Rounds no. samples and no. fish to decimal with two digits
  records = records[, 3:4 := lapply(.SD, function(value) {
    return(round(as.numeric(value), 2))
  }), .SDcols = 3:4]

  return(
    list(
      strata  = strata,
      records = list(
        data = data.table(
          size_class_low  = records[, 1],
          size_class_high = records[, 2],
          num_samples     = records[, 3],
          num_fish        = records[, 4]
        )
      )
    )
  )
}
