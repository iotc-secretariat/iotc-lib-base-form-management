#' @include IOTC_form_class.R
#' @export IOTCFormCESF
IOTCFormCESF = setClass(
  "IOTCFormCESF",
  contains = "IOTCForm"
)

setGeneric("validate_months", function(form, strata) {
  standardGeneric("validate_months")
})

setGeneric("allow_empty_data", function(form) {
  standardGeneric("allow_empty_data")
})

setGeneric("estimation_column", function(form) {
  standardGeneric("estimation_column")
})

setMethod("form_comment_cell_row", "IOTCFormCESF", function(form) {
  return(34) #Default for CE / SF
})

setMethod("extract_metadata", list(form = "IOTCFormCESF", common_metadata = "list"), function(form, common_metadata) {
  l_info("IOTCFormCESF.extract_metadata")

  metadata_sheet = form@original_metadata

  common_metadata$general_information$fishery        = trim(as.character(metadata_sheet[16, 7]))
  common_metadata$general_information$target_species = trim(as.character(metadata_sheet[17, 7]))

  data_specifications = list(
    type_of_data    = trim(as.character(metadata_sheet[23, 4])),
    data_source     = trim(as.character(metadata_sheet[25, 4])),
    data_processing = trim(as.character(metadata_sheet[26, 4])),
    data_raising    = trim(as.character(metadata_sheet[27, 4])),
    coverage_type   = trim(as.character(metadata_sheet[28, 4])),
    coverage_value  = trim(as.character(metadata_sheet[29, 4]))
  )

  common_metadata$data_specifications = data_specifications

  return(common_metadata)
})

setMethod("validate_metadata", list(form = "IOTCFormCESF", common_metadata_validation_results = "list"), function(form, common_metadata_validation_results) {
  l_info("IOTCFormCESF.validate_metadata")

  general_information = form@metadata$general_information
  data_specifications = form@metadata$data_specifications

  fishery_available        = is_provided(general_information$fishery)
  fishery_valid            = fishery_available && is_fishery_valid(general_information$fishery)
  fishery_multiple         = fishery_valid && is_multiple_gear_fishery(general_information$fishery)
  fishery_valid            = fishery_valid && !fishery_multiple

  fishery = ifelse(fishery_available && fishery_valid, fisheries_for(general_information$fishery), NA)
  fishery = iotc.data.reference.codelists::LEGACY_FISHERIES[CODE == fishery]

  fishery_group    = ifelse(fishery_valid, fishery$FISHERY_GROUP_CODE, NA)
  fishery_type     = ifelse(fishery_valid, fishery$FISHERY_TYPE_CODE , NA)
  fishery_category = ifelse(fishery_valid, fishery$FISHERY_CATEGORY,   NA)

  common_metadata_validation_results$general_information$fishery =
    list(
      available = fishery_available,
      code      = general_information$fishery,
      multiple  = fishery_multiple,
      valid     = fishery_valid,
      group     = fishery_group,
      type      = fishery_type,
      category  = fishery_category
    )

  target_species_available = is_provided(general_information$target_species)
  target_species_valid     = target_species_available && is_species_valid(general_information$target_species)
  target_species_multiple  = target_species_valid && is_species_aggregate(general_information$target_species)

  common_metadata_validation_results$general_information$target_species =
    list(
      available = target_species_available,
      code      = general_information$target_species,
      multiple  = target_species_multiple,
      valid     = target_species_valid
    )

  common_metadata_validation_results$data_specifications = list()

  data_type_available = is_provided(data_specifications$type_of_data)
  data_type_valid     = data_type_available && is_data_type_valid(data_specifications$type_of_data)

  common_metadata_validation_results$data_specifications$type_of_data =
    list(
      available = data_type_available,
      code      = data_specifications$type_of_data,
      valid     = data_type_valid
    )

  data_source_available = is_provided(data_specifications$data_source)
  data_source_valid     = data_source_available && is_data_source_valid(form_dataset_code(form), data_specifications$data_source)

  common_metadata_validation_results$data_specifications$source =
    list(
      available = data_source_available,
      dataset   = form_dataset_code(form),
      code      = data_specifications$data_source,
      valid     = data_source_valid
    )

  data_processing_available = is_provided(data_specifications$data_processing)
  data_processing_valid     = data_processing_available && is_data_processing_valid(form_dataset_code(form), data_specifications$data_processing)

  common_metadata_validation_results$data_specifications$processing =
    list(
      available = data_processing_available,
      dataset   = form_dataset_code(form),
      code      = data_specifications$data_processing,
      valid     = data_processing_valid
    )

  data_raising_available = is_provided(data_specifications$data_raising)
  data_raising_valid     = data_raising_available && is_data_raising_valid(data_specifications$data_raising)

  common_metadata_validation_results$data_specifications$raising =
    list(
      available = data_raising_available,
      code      = data_specifications$data_raising,
      valid     = data_raising_valid
    )

  common_metadata_validation_results$data_specifications$coverage = list()

  coverage_type_available = is_provided(data_specifications$coverage_type)
  coverage_type_valid     = coverage_type_available && is_data_coverage_type_valid(data_specifications$coverage_type)

  common_metadata_validation_results$data_specifications$coverage$type =
    list(
      available = coverage_type_available,
      code      = data_specifications$coverage_type,
      valid     = coverage_type_valid
    )

  coverage_available      = is_provided(data_specifications$coverage_value)
  coverage_numeric        = is_numeric(data_specifications$coverage_value)
  coverage_valid          = coverage_available && coverage_numeric &&
                            as.numeric(data_specifications$coverage_value)  >   0 &&
                            as.numeric(data_specifications$coverage_value) <= 100

  common_metadata_validation_results$data_specifications$coverage$value =
    list(
      available = coverage_available,
      value     = data_specifications$coverage_value,
      valid     = coverage_numeric && coverage_valid
    )

  return(common_metadata_validation_results)
})

setMethod("metadata_validation_summary", list(form = "IOTCFormCESF", metadata_validation_results = "list"), function(form, metadata_validation_results) {
  l_info("IOTCFormCESF.metadata_validation_summary")

  validation_messages = new("MessageList")

  general_information = metadata_validation_results$general_information
  data_specifications = metadata_validation_results$data_specifications

  # General information

  ## Fishery

  if(!general_information$fishery$available)
    validation_messages = add(validation_messages, new("Message", level = "ERROR", source = "Metadata", row = 18, column = "G", text = "The fishery is mandatory"))
  else {
    if(general_information$fishery$multiple)
      validation_messages = add(validation_messages, new("Message", level = "ERROR", source = "Metadata", row = 18, column = "G", text = paste0("The provided fishery (", general_information$fishery$code, ") is a fishery aggregate")))

    if(!general_information$fishery$valid)
      validation_messages = add(validation_messages, new("Message", level = "ERROR", source = "Metadata", row = 18, column = "G", text = paste0("The provided fishery (", general_information$fishery$code, ") is not valid. Please refer to ", reference_codes("legacy", "fisheries"), " for a list of valid fishery codes")))
  }

  ## Species

  if(!general_information$target_species$available)
    validation_messages = add(validation_messages, new("Message", level = "ERROR", source = "Metadata", row = 19, column = "G", text = "The target species is mandatory"))
  else if(!general_information$target_species$valid)
    validation_messages = add(validation_messages, new("Message", level = "ERROR", source = "Metadata", row = 19, column = "G", text = paste0("The provided target species (", general_information$target_species$code, ") is not valid. Please refer to ", reference_codes("legacy", "species"), " for a list of valid species codes")))

  # Data specifications

  ## Type of data

  if(!data_specifications$type_of_data$available)
    validation_messages = add(validation_messages, new("Message", level = "ERROR", source = "Metadata", row = 25, column = "D", text = "The type of data is mandatory"))
  else if(!data_specifications$type_of_data$valid)
    validation_messages = add(validation_messages, new("Message", level = "ERROR", source = "Metadata", row = 25, column = "D", text = paste0("The provided type of data (", data_specifications$type_of_data$code, ") is not valid. Please refer to ", reference_codes("data", "types"), " for a list of valid data type codes")))

  ## Data source

  if(!data_specifications$source$available)
    validation_messages = add(validation_messages, new("Message", level = "ERROR", source = "Metadata", row = 27, column = "D", text = "The data source is mandatory"))
  else if(!data_specifications$source$valid)
    validation_messages = add(validation_messages, new("Message", level = "ERROR", source = "Metadata", row = 27, column = "D", text = paste0("The provided data source (", data_specifications$source$dataset, " / ", data_specifications$source$code, ") is not valid. Please refer to ", reference_codes("data", "sources"), " for a list of valid data source codes for the ", data_specifications$source$dataset, " dataset")))

  ## Data processing

  if(!data_specifications$processing$available)
    validation_messages = add(validation_messages, new("Message", level = "ERROR", source = "Metadata", row = 28, column = "D", text = "The data processing is mandatory"))
  else if(!data_specifications$processing$valid)
    validation_messages = add(validation_messages, new("Message", level = "ERROR", source = "Metadata", row = 28, column = "D", text = paste0("The provided data processing (", data_specifications$source$dataset, " / ", data_specifications$processing$code, ") is not valid. Please refer to ", reference_codes("data", "processings"), " for a list of valid data processing codes for the ", data_specifications$processing$dataset, " dataset")))

  ## Raising

  if(!data_specifications$raising$available)
    validation_messages = add(validation_messages, new("Message", level = "ERROR", source = "Metadata", row = 29, column = "D", text = "The data raising is mandatory"))
  else if(!data_specifications$raising$valid)
    validation_messages = add(validation_messages, new("Message", level = "ERROR", source = "Metadata", row = 29, column = "D", text = paste0("The provided data raising (", data_specifications$raising$code, ") is not valid. Please refer to ", reference_codes("data", "raisings"), " for a list of valid data raising codes")))

  ## Coverage

  if(!data_specifications$coverage$type$available)
    validation_messages = add(validation_messages, new("Message", level = "ERROR", source = "Metadata", row = 30, column = "D", text = "The coverage type is mandatory"))
  else if(!data_specifications$coverage$type$valid)
    validation_messages = add(validation_messages, new("Message", level = "ERROR", source = "Metadata", row = 30, column = "D", text = paste0("The provided coverage type (", data_specifications$coverage$type$code, ") is not valid. Please refer to ", reference_codes("data", "coverageTypes"), " for a list of valid data coverage type codes")))

  if(!data_specifications$coverage$value$available)
    validation_messages = add(validation_messages, new("Message", level = "ERROR", source = "Metadata", row = 31, column = "D", text = "The coverage value is mandatory"))
  else if(!data_specifications$coverage$value$valid)
    validation_messages = add(validation_messages, new("Message", level = "ERROR", source = "Metadata", row = 31, column = "D", text = paste0("The provided coverage value (", data_specifications$coverage$value$value, ") is not valid, as it should be numeric, higher than 0 and less than (or equal) to 100 (%)")))

  return(validation_messages)
})

setMethod("validate_data", list(form = "IOTCFormCESF", metadata_validation_results = "list"), function(form, metadata_validation_results) {
  l_info("IOTCFormCESF.validate_data")

  strata_orig = form@data$strata
  strata_orig$MONTH = strata_orig$MONTH_ORIGINAL
  strata_orig$MONTH_ORIGINAL  = NULL

  strata  = form@data$strata
  records = form@data$records

  ### TO BE MOVED TO SUBCLASSES
  CE_SF_data_original = records$data$CE_SF_data_original
  CE_SF_data          = records$data$CE_SF_data

  strata_empty_rows    = find_empty_rows(strata_orig)
  strata_empty_columns = find_empty_columns(strata_orig)

  strata[, IS_EMPTY := .I %in% strata_empty_rows]

  total_strata     = nrow(strata)
  non_empty_strata = which(strata$IS_EMPTY == FALSE) #strata[ !1:.N %in% strata_empty_rows ]

  ### TO BE MOVED TO SUBCLASSES
  data_empty_rows    = find_empty_rows(CE_SF_data)
  data_empty_columns = find_empty_columns(CE_SF_data)

  missing_months   = which( is.na(strata$MONTH_ORIGINAL))
  invalid_months   = which(!is.na(strata$MONTH_ORIGINAL) & !is_month_valid(strata$MONTH))
  invalid_months   = invalid_months[ ! invalid_months %in% missing_months ]
  missing_months   = missing_months[ ! missing_months %in% strata_empty_rows]

  # If all months are provided and valid, we check that they're also consistent...
  months_check = validate_months(form, strata)

  missing_grids  = which( is.na(strata$GRID_CODE))
  invalid_grids  = which(!is_grid_AR_valid(strata$GRID_CODE))
  invalid_grids  = invalid_grids[ ! invalid_grids %in% missing_grids ]
  missing_grids  = missing_grids[ ! missing_grids %in% strata_empty_rows]

  missing_estimations = which( is.na(strata$ESTIMATION_CODE))
  invalid_estimations = which(!is_data_estimation_valid(strata$ESTIMATION_CODE))
  invalid_estimations = invalid_estimations[ ! invalid_estimations %in% missing_estimations ]
  missing_estimations = missing_estimations[ ! missing_estimations %in% strata_empty_rows]

  return(
    list(
      strata = list(
        empty_rows = list(
          number      = length(strata_empty_rows),
          row_indexes = spreadsheet_rows_for(form, strata_empty_rows)
        ),
        empty_columns = list(
          number      = length(strata_empty_columns),
          col_indexes = spreadsheet_cols_for_strata(form, strata_empty_columns)
        ),
        total = list(
          number = total_strata
        ),
        non_empty = list(
          number = length(non_empty_strata),
          row_indexes = spreadsheet_rows_for(form, non_empty_strata)
        ),
        checks = list(
          main = list(
            months = list(
              missing = list(
                number      = length(missing_months),
                row_indexes = spreadsheet_rows_for(form, missing_months)
              ),
              invalid = list(
                number        = length(invalid_months),
                row_indexes   = spreadsheet_rows_for(form, invalid_months),
                values        = strata$MONTH[invalid_months],
                values_unique = unique(strata$MONTH[invalid_months])
              )
              # REMOVED: while we expect data to be provided for all quarters in 1-RC and 1-DI, same is not the case for 3-CE or 4-SF where stratification is much finer
              #, incomplete = list(
              #  number = length(months_check$incomplete_months),
              #  row_indexes = spreadsheet_rows_for(form, months_check$incomplete_months)
              #)
            ),
            grids = list(
              invalid = list(
                number       = length(invalid_grids),
                row_indexes  = spreadsheet_rows_for(form, invalid_grids),
                codes        = strata[invalid_grids]$GRID_CODE,
                codes_unique = unique(strata[invalid_grids]$GRID_CODE)
              ),
              missing = list(
                number      = length(missing_grids),
                row_indexes = spreadsheet_rows_for(form, missing_grids)
              )
            ),
            estimations = list(
              invalid = list(
                number       = length(invalid_estimations),
                row_indexes  = spreadsheet_rows_for(form, invalid_estimations),
                codes        = strata[invalid_estimations]$ESTIMATION_CODE,
                codes_unique = unique(strata[invalid_estimations]$ESTIMATION_CODE)
              ),
              missing = list(
                number      = length(missing_estimations),
                row_indexes = spreadsheet_rows_for(form, missing_estimations)
              )
            )
          )
        )
      ),
      records = list(
        total = nrow(CE_SF_data),
        empty_rows = list(
          number      = length(data_empty_rows),
          row_indexes = spreadsheet_rows_for(form, data_empty_rows)
        ),
        empty_columns = list(
          number      = length(data_empty_columns),
          col_indexes = spreadsheet_cols_for(form, data_empty_columns)
        )
      )
    )
  )
})

setMethod("common_data_validation_summary", list(form = "IOTCFormCESF", metadata_validation_results = "list", data_validation_results = "list"), function(form, metadata_validation_results, data_validation_results) {
  l_info("IOTCFormCESF.common_data_validation_summary")

  validation_messages = new("MessageList")

  ### STRATA AND RECORDS

  strata  = data_validation_results$strata
  records = data_validation_results$records

  checks_strata  = strata$checks
  checks_records = records$checks

  # Strata issues / summary

  validation_messages = report_strata(validation_messages, strata)

  # Strata checks

  ## Main strata

  checks_strata_main = checks_strata$main

  months = checks_strata_main$months

  # REMOVED, see also the comment in the validate_data above...
  #if(months$incomplete$number > 0)
  #  validation_messages = add(validation_messages, new("Message", level = "WARN", source = "Data", text = paste0("Data is not provided for all months within the strata in row(s) #", paste0(months$incomplete$row_indexes, collapse = ", "))))

  validation_messages = report_months(validation_messages, months)

  grids = checks_strata_main$grids

  if(grids$missing$number > 0) { # This remains identical for 3-CE and 4-SF, whose 'GRID' column is the same
    if(grids$missing$number > 1) validation_messages = add(validation_messages, new("Message", level = "ERROR", source = "Data", column = "C", text = paste0(grids$missing$number, " missing grids")))

    for(row in grids$missing$row_indexes)
      validation_messages = add(validation_messages, new("Message", level = "ERROR", source = "Data", row = row, column = "C", text = paste0("Missing grid in row #", row)))
  }

  # MOVED TO 3CE
  #if(grids$invalid$number > 0) {
  #  validation_messages = add(validation_messages, new("Message", level = "ERROR", source = "Data", text = paste0("Invalid grid code in row(s) #", paste0(grids$invalid$row_indexes, collapse = ", "), ". Please refer to ", reference_codes("admin", "IOTCgridsCESF"), " for a list of valid grid codes")))
  #}

  estimations = checks_strata_main$estimations # NOT PART OF THE STRATUM
  estimation_col = estimation_column(form)

  if(estimations$missing$number > 0) { # Applies to 3CE and 4SF
    if(estimations$missing$number > 1) validation_messages = add(validation_messages, new("Message", level = "ERROR", source = "Data", column = estimation_col, text = paste0(estimations$missing$number, " missing estimation codes")))

    for(row in estimations$missing$row_indexes)
      validation_messages = add(validation_messages, new("Message", level = "ERROR", source = "Data", row = row, column = estimation_col, text = paste0("Missing estimation code in row #", row)))
  }

  if(estimations$invalid$number > 0) { # Applies to 3CE and 4SF
    validation_messages = add(validation_messages, new("Message", level = "ERROR", source = "Data", column = estimation_col, text = paste0(estimations$invalid$number, " invalid estimation code(s) reported. Please refer to ", reference_codes("data", "estimates"), " for a list of valid estimation codes")))

    for(row in estimations$invalid$row_indexes)
      validation_messages = add(validation_messages, new("Message", level = "ERROR", source = "Data", row = row, column = estimation_col, text = paste0("Invalid estimation code in row #", row)))
  }

  ###

  # Data issues / summary

  ## Empty rows / columns
  return(
    report_data(
      validation_messages,
      records,
      allow_empty_data(form)
    )
  )
})
