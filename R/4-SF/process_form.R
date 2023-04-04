verify_form_4SF = function(form) {
  form_details = extract_metadata_1DI(form)$form_details

  form_type    = form_details$type
  form_version = form_details$version

  if(check_mandatory(form_type, "Form type") != "4-SF")
    stop(paste0("Please provide a valid form 4-SF (current form type: ", form_type, " - required: 4-SF)"))

  if(check_mandatory(form_version, "Form version") != "1.0.0")
    stop(paste0("Please provide a valid form 4-SF (current form version: ", form_version, " - required: 1.0.0)"))
}

extract_metadata_4SF = function(form) {
  metadata = common_metadata(form$form_metadata)

  fishery = trim(as.character(form$form_metadata[16, 7]))
  species = trim(as.character(form$form_metadata[17, 7]))

  type_of_data    = trim(as.character(form$form_metadata[23, 4]))
  data_source     = trim(as.character(form$form_metadata[25, 4]))
  data_processing = trim(as.character(form$form_metadata[26, 4]))
  data_raising    = trim(as.character(form$form_metadata[27, 4]))
  coverage_type   = trim(as.character(form$form_metadata[28, 4]))
  coverage_value  = trim(as.character(form$form_metadata[29, 4]))
  coverage_value  = as.numeric(coverage_value)

  measurement_type    = trim(as.character(form$form_metadata[23, 7]))
  measurement_measure = trim(as.character(form$form_metadata[24, 7]))
  measurement_tool    = trim(as.character(form$form_metadata[25, 7]))

  size_interval       = trim(as.character(form$form_metadata[26, 7]))

  type_of_fate          = trim(as.character(form$form_metadata[28, 7]))
  fate_of_catch_sampled = trim(as.character(form$form_metadata[29, 7]))

  metadata$comment = comments(form$form_metadata, 34)

  metadata$general_information$fishery = fishery
  metadata$general_information$species = species

  metadata$data_specifications = list(
    type_of_data    = type_of_data,
    data_source     = data_source,
    data_processing = data_processing,
    data_raising    = data_raising,
    coverage_type   = coverage_type,
    coverage_value  = coverage_value,
    measurements = list(
      type          = measurement_type,
      measure       = measurement_measure,
      tool          = measurement_tool,
      size_interval = size_interval
    ),
    fate = list(
      type_of_fate          = type_of_fate,
      fate_of_catch_sampled = fate_of_catch_sampled
    )
  )

  return(metadata)
}

extract_strata_and_records_4SF = function(form) {
  form_metadata = form$form_metadata
  form_data     = form$form_data

  strata = form_data[4:nrow(form_data)][, 2:5]
  colnames(strata) = c("MONTH", "GRID_CODE", "SEX_CODE", "ESTIMATION_CODE")

  records = form_data[4:nrow(form_data), 6:ncol(form_data)]

  # Rounds size-class (low) to lower integer
  records = records[, 1 := lapply(.SD, function(value) {
    return(floor(as.numeric(value)))
  }), .SDcols = 1]

  # Rounds no. samples and no. fish to decimal with two digits
  records = records[, 2:3 := lapply(.SD, function(value) {
    return(round(as.numeric(value), 2))
  }), .SDcols = 2:3]

  return(
    list(
      strata  = strata,
      records = list(
        data = list(
          size_class  = records[, 1],
          num_samples = records[, 2],
          num_fish    = records[, 3]
        )
      )
    )
  )
}
