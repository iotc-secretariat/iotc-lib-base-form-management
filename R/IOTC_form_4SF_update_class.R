#' @include IOTC_form_CESF_update_class.R
#' @export IOTCForm4SFUpdate
IOTCForm4SFUpdate = setClass(
  "IOTCForm4SFUpdate",
  contains = "IOTCFormCESFUpdate"
)

setMethod("form_type", "IOTCForm4SFUpdate", function(form) {
  return(c("4-SF-update", "4SF-update")) # For backwards compatibility
})

setMethod("form_version", "IOTCForm4SFUpdate", function(form) {
  return("1.0.0")
})

setMethod("form_dataset_code", "IOTCForm4SFUpdate", function(form) {
  return("SF")
})

setMethod("allow_empty_data", "IOTCForm4SFUpdate", function(form) {
  return(FALSE)
})

setMethod("estimation_column", "IOTCForm4SFUpdate", function(form) {
  return("E")
})

setMethod("first_data_column", "IOTCForm4SFUpdate", function(form) {
  return(which(EXCEL_COLUMNS == "G"))
})

setMethod("first_data_row", "IOTCForm4SFUpdate", function(form) {
  return(6)
})

setMethod("first_strata_column", "IOTCForm4SFUpdate", function(form) {
  return(which(EXCEL_COLUMNS == "B"))
})

setMethod("last_strata_column", "IOTCForm4SFUpdate", function(form) {
  return(which(EXCEL_COLUMNS == "F"))
})

setMethod("validate_months", list(form = "IOTCForm4SFUpdate", strata = "data.table"), function(form, strata) {
  l_info("IOTCForm4SFUpdate.validate_months")

  # This form is used to provide updates for a given fishery and species only, so the provision of 'complete' month data
  # (i.e., for each and every month of the year) # (i.e., for each month of the year) should be considered at the grid level rather than at the full stratum level
  valid_months_strata   = strata[!is.na(MONTH_ORIGINAL) & MONTH %in% 1:12, .(NUM_MONTHS = .N), keyby = .(GRID_CODE)]

  incomplete_months_strata  = valid_months_strata[NUM_MONTHS < 12]

  incomplete_months  = merge(strata, incomplete_months_strata, all.x = TRUE, sort = FALSE, by = c("GRID_CODE"))
  incomplete_months  = which(!is.na(incomplete_months$NUM_MONTHS))

  return(
    list(
      incomplete_months  = incomplete_months
    )
  )
})

setMethod("extract_metadata", list(form = "IOTCForm4SFUpdate", common_metadata = "list"), function(form, common_metadata) {
  l_info("IOTCForm4SFUpdate.extract_metadata")

  custom_metadata = callNextMethod(form, common_metadata)

  metadata_sheet = form@original_metadata

  custom_metadata$general_information$species = trim(as.character(metadata_sheet[18, 7]))

  custom_metadata$data_specifications$measurements = list(
    type     = trim(as.character(metadata_sheet[23, 7])),
    measure  = trim(as.character(metadata_sheet[24, 7])),
    tool     = trim(as.character(metadata_sheet[25, 7])),
    interval = trim(as.character(metadata_sheet[26, 7]))
  )

  custom_metadata$data_specifications$fate = list(
    type = trim(as.character(metadata_sheet[28, 7])),
    fate = trim(as.character(metadata_sheet[29, 7]))
  )

  return(custom_metadata)
})

setMethod("validate_metadata", list(form = "IOTCForm4SFUpdate", common_metadata_validation_results = "list"), function(form, common_metadata_validation_results) {
  l_info("IOTCForm4SFUpdate.validate_metadata")

  common_metadata_validation_results = callNextMethod(form, common_metadata_validation_results)

  general_information = form@metadata$general_information

  species_available = is_provided(general_information$species)
  species_valid     = species_available && is_species_valid(general_information$species)
  species_multiple  = species_valid && is_species_aggregate(general_information$species)

  common_metadata_validation_results$general_information$species =
    list(
      available = species_available,
      code      = general_information$species,
      multiple  = species_multiple,
      valid     = species_valid
    )

  data_specifications = form@metadata$data_specifications

  type_of_measurement_available   = is_provided(data_specifications$measurements$type)
  type_of_measurement_valid       = type_of_measurement_available && is_measurement_type_valid(data_specifications$measurements$type)

  measurement_available   = is_provided(data_specifications$measurements$measure)
  measurement_valid       = measurement_available && type_of_measurement_valid && is_measurement_valid(data_specifications$measurements$type,
                                                                                                       data_specifications$measurements$measure)

  measuring_tool_available   = is_provided(data_specifications$measurements$type)
  measuring_tool_valid       = measuring_tool_available && type_of_measurement_valid && is_measuring_tool_valid(data_specifications$measurements$type,
                                                                                                                data_specifications$measurements$tool)

  size_interval_available = is_provided(data_specifications$measurements$interval)
  size_interval_valid     = size_interval_available &&
                            is_numeric(data_specifications$measurements$interval) &&
                            as.numeric(data_specifications$measurements$interval) > 0

  custom_metadata_validation_results = common_metadata_validation_results

  custom_metadata_validation_results$data_specifications$measurements =
    list(
      type =  list(
        available = type_of_measurement_available,
        code      = data_specifications$measurements$type,
        valid     = type_of_measurement_valid
      ),
      measure = list(
        available = measurement_available,
        code      = data_specifications$measurements$measure,
        valid     = measurement_valid
      ),
      tool = list(
        available = measuring_tool_available,
        code      = data_specifications$measurements$tool,
        valid     = measuring_tool_valid
      ),
      interval = list(
        available = size_interval_available,
        value     = ifelse(size_interval_valid, as.numeric(data_specifications$measurements$interval), NA),
        valid     = size_interval_valid
      )
    )

  type_of_fate_available   = is_provided(data_specifications$fate$type)
  type_of_fate_valid       = type_of_fate_available && is_fate_type_valid(data_specifications$fate$type)

  fate_available   = is_provided(data_specifications$fate$fate)
  fate_valid       = fate_available && type_of_fate_valid && is_fate_valid(data_specifications$fate$type,
                                                                           data_specifications$fate$fate)

  custom_metadata_validation_results$data_specifications$fate = list(
    type = list(
      available = type_of_fate_available,
      code      = data_specifications$fate$type,
      valid     = type_of_fate_valid
    ),
    fate = list(
      available = fate_available,
      code      = data_specifications$fate$fate,
      valid     = fate_valid
    )
  )

  return(custom_metadata_validation_results)
})

setMethod("metadata_validation_summary", list(form = "IOTCForm4SFUpdate", metadata_validation_results = "list"), function(form, metadata_validation_results) {
  l_info("IOTCForm4SFUpdate.metadata_validation_summary")

  validation_messages = callNextMethod(form, metadata_validation_results) #new("MessageList")

  general_information    = metadata_validation_results$general_information
  data_specifications    = metadata_validation_results$data_specifications

  measurements           = data_specifications$measurements
  fate                   = data_specifications$fate

  # Data specifications

  ## Species

  if(!general_information$species$available)
    validation_messages = add(validation_messages, new("Message", level = "ERROR", source = "Metadata", row = 20, column = "G", text = "The species is mandatory"))
  else if(!general_information$species$valid)
    validation_messages = add(validation_messages, new("Message", level = "ERROR", source = "Metadata", row = 20, column = "G", text = paste0("The provided species (", general_information$species$code, ") is not valid. Please refer to ", reference_codes("biological", "species"), " for a list of valid species codes")))
  else if(general_information$species$multiple)
    validation_messages = add(validation_messages, new("Message", level = "ERROR", source = "Metadata", row = 20, column = "G", text = paste0("The provided species (", general_information$species$code, ") correspond to a species aggregate. Please refer to ", reference_codes("biological", "species"), " for a list of valid, distinct species codes")))

  ## Measurements

  ### Type

  if(!measurements$type$available)
    validation_messages = add(validation_messages, new("Message", level = "ERROR", source = "Metadata", row = 25, column = "G", text = "The measurement type is mandatory"))
  else if(!measurements$type$valid)
    validation_messages = add(validation_messages, new("Message", level = "ERROR", source = "Metadata", row = 25, column = "G", text = paste0("The provided measurement type (", measurements$type$code, ") is not valid. Please refer to ", reference_codes("biological", "typesOfMeasurement"), " for a list of valid measurement type codes")))

  if(measurements$type$valid && measurements$type$code == "LN")
    validation_messages = add(validation_messages, new("Message", level = "INFO", source = "Metadata", row = 25, column = "G", text = paste0("The provided measurement type (", measurements$type$code, ") refers to individual lengths: the measurement unit will be assumed to be centimeters (cm)")))

  if(measurements$type$valid && measurements$type$code == "WG")
    validation_messages = add(validation_messages, new("Message", level = "INFO", source = "Metadata", row = 25, column = "G", text = paste0("The provided measurement type (", measurements$type$code, ") refers to individual weights: the measurement unit will be assumed to be kilograms (kg)")))

  ### Measure

  if(!measurements$measure$available)
    validation_messages = add(validation_messages, new("Message", level = "ERROR", source = "Metadata", row = 26, column = "G", text = "The measure is mandatory"))
  else if(!measurements$measure$valid)
    validation_messages = add(validation_messages, new("Message", level = "ERROR", source = "Metadata", row = 26, column = "G", text = paste0("The provided measure (", measurements$measure$code, ") is not valid. Please refer to ", reference_codes("biological", "allMeasurementTypes"), " for a list of valid measure codes")))

  ### Measuring tool

  if(!measurements$tool$available)
    validation_messages = add(validation_messages, new("Message", level = "ERROR", source = "Metadata", row = 27, column = "G", text = "The measuring tool is mandatory"))
  else if(!measurements$tool$valid)
    validation_messages = add(validation_messages, new("Message", level = "ERROR", source = "Metadata", row = 27, column = "G", text = paste0("The provided measuring tool (", measurements$tool$code, ") is not valid. Please refer to ", reference_codes("biological", "allMeasurementTools"), " for a list of valid measuring tool codes")))

  ### Size interval

  if(!measurements$interval$available)
    validation_messages = add(validation_messages, new("Message", level = "ERROR", source = "Metadata", row = 28, column = "G", text = "The size interval is mandatory"))
  else if(!measurements$interval$valid)
    validation_messages = add(validation_messages, new("Message", level = "ERROR", source = "Metadata", row = 28, column = "G", text = paste0("The provided size interval (", measurements$interval$value, ") is not valid. Please ensure to provide a numeric value greater than zero")))

  ## Fate

  ### Type

  if(!fate$type$available)
    validation_messages = add(validation_messages, new("Message", level = "ERROR", source = "Metadata", row = 30, column = "G", text = "The type of fate is mandatory"))
  else if(!fate$type$valid)
    validation_messages = add(validation_messages, new("Message", level = "ERROR", source = "Metadata", row = 30, column = "G", text = paste0("The provided type of fate (", fate$fate$code, ") is not valid. Please refer to ", reference_codes("biological", "typesOfFate"), " for a list of valid fate type codes")))

  ### Code

  if(!fate$fate$available)
    validation_messages = add(validation_messages, new("Message", level = "ERROR", source = "Metadata", row = 31, column = "G", text = "The fate is mandatory"))
  else if(!fate$fate$valid)
    validation_messages = add(validation_messages, new("Message", level = "ERROR", source = "Metadata", row = 31, column = "G", text = paste0("The provided fate (", fate$fate$code, ") is not valid. Please refer to ", reference_codes("biological", "fates"), " for a list of valid fate codes")))

  return(validation_messages)
})

setMethod("extract_data", "IOTCForm4SFUpdate", function(form) {
  l_info("IOTCForm4SFUpdate.extract_data")

  form_metadata = form@original_metadata
  form_data     = form@original_data

  has_data = nrow(form_data) >= 4

  strata = form_data[4:ifelse(has_data, nrow(form_data), 4)][, first_strata_column(form):last_strata_column(form)]

  if(!has_data) {
    strata = as.data.table(matrix(nrow = 0, ncol = length(colnames(strata))))
  }

  colnames(strata) = c("MONTH", "GRID_CODE", "SEX_CODE", "ESTIMATION_CODE", "SIZE_CLASS_LOW")

  strata[, SIZE_CLASS_LOW  := floor(as.numeric(SIZE_CLASS_LOW))]

  strata[, MONTH_ORIGINAL := MONTH]
  strata[, MONTH          := as.integer(MONTH)]

  records = form_data[4:ifelse(has_data, nrow(form_data), 4), first_data_column(form):ncol(form_data)]

  if(has_data) {
    records_original = data.table(NUM_SAMPLES = records[, 1], NUM_FISH = records[, 2])
    colnames(records_original) = c("NUM_SAMPLES", "NUM_FISH")
    # Might raise the "Warning in FUN(X[[i]], ...) : NAs introduced by coercion" message when catches include non-numeric values...
    records = records_original[, lapply(.SD, function(value) { return(round(as.numeric(value), 2)) })]
  } else {
    records_original = as.data.table(matrix(nrow = 0, ncol = 2))
    colnames(records_original) = c("NUM_SAMPLES", "NUM_FISH")
    records = records_original
  }

  return(
    list(
      strata = strata,
      records =
        list(
          data = list(
            CE_SF_data_original = records_original,
            CE_SF_data          = records
          )
        )
    )
  )
})

setMethod("validate_data", list(form = "IOTCForm4SFUpdate", metadata_validation_results = "list"), function(form, metadata_validation_results) {
  l_info("IOTCForm4SFUpdate.validate_data")

  data_validation_results = callNextMethod(form, metadata_validation_results)

  strata  = form@data$strata
  strata$IS_EMPTY = NULL

  strata_orig = form@data$strata
  strata_orig$MONTH = strata_orig$MONTH_ORIGINAL
  strata_orig$MONTH_ORIGINAL  = NULL
  strata_orig$IS_EMPTY = NULL # Otherwise the 'find_empty_rows' call below will never return anything meaningful...

  strata_empty_rows    = find_empty_rows(strata_orig)
  strata_empty_columns = find_empty_columns(strata_orig[, c(1:3, 5)])

  strata[, IS_EMPTY := .I %in% strata_empty_rows]
  strata[, OCCURRENCES := .N, by = .(MONTH, GRID_CODE, SEX_CODE, SIZE_CLASS_LOW)]

  # If all months are provided and valid, we check that they're also consistent...
  valid_months_strata   = strata[!is.na(MONTH_ORIGINAL) & MONTH %in% 1:12, .(NUM_MONTHS = .N), keyby = .(GRID_CODE, SEX_CODE)]

  incomplete_months_strata  = valid_months_strata[NUM_MONTHS < 12]

  incomplete_months  = merge(strata, incomplete_months_strata, all.x = TRUE, sort = FALSE, by = c("GRID_CODE", "SEX_CODE"))
  incomplete_months  = which(!is.na(incomplete_months$NUM_MONTHS))

  non_empty_strata = which(strata$IS_EMPTY == FALSE) #strata[ !1:.N %in% strata_empty_rows ]
  duplicate_strata = which(strata$OCCURRENCES > 1)   #which(strata_duplicated$COUNT > 1)
  duplicate_strata = duplicate_strata[ ! duplicate_strata %in% strata_empty_rows ]
  unique_strata    = non_empty_strata[ ! non_empty_strata %in% duplicate_strata ]

  data_validation_results$strata$duplicate =
    list(
      number = length(duplicate_strata),
      row_indexes = spreadsheet_rows_for(form, duplicate_strata)
    )

  data_validation_results$strata$unique =
    list(
      number = length(unique_strata),
      row_indexes = spreadsheet_rows_for(form, unique_strata)
    )

  grid_size = function(code) {
    return(
      fifelse(is.na(code) | code == "",
              "OTHER",
              fifelse(str_sub(code, 1, 1) == "5",
                      "1_DEG",
                      fifelse(str_sub(code, 1, 1) == "6",
                              "5_DEG",
                              "OTHER"
                      )
              )
      )
    )
  }

  grid_status    = data.table(GRID_CODE = strata$GRID_CODE,
                              MISSING   = is.na(strata$GRID_CODE),
                              VALID     = is_grid_CE_SF_valid(strata$GRID_CODE),
                              SIZE      = grid_size(strata$GRID_CODE))

  wrong_grid_types = which(grid_status$SIZE == "OTHER")
  wrong_grid_types = wrong_grid_types[ which(wrong_grid_types %in% which(grid_status$VALID)) ]

  data_validation_results$strata$checks$main$grids$wrong = list(
    number       = length(wrong_grid_types),
    row_indexes  = spreadsheet_rows_for(form, wrong_grid_types),
    codes        = strata$GRID_CODE[wrong_grid_types],
    codes_unique = unique(strata$GRID_CODE[wrong_grid_types])
  )

  missing_sex = which( is.na(strata$SEX_CODE))
  invalid_sex = which(!is_sex_valid(strata$SEX_CODE))
  invalid_sex = invalid_sex[ ! invalid_sex %in% missing_sex ]
  missing_sex = missing_sex[ ! missing_sex %in% strata_empty_rows ]

  data_validation_results$strata$checks$sex = list(
    missing = list(
      number      = length(missing_sex),
      row_indexes = spreadsheet_rows_for(form, missing_sex)
    ),
    invalid = list(
      number        = length(invalid_sex),
      row_indexes   = spreadsheet_rows_for(form, invalid_sex),
      values        = strata$SEX_CODE[invalid_sex],
      values_unique = unique(strata$SEX_CODE[invalid_sex])
    )
  )

  missing_size_class = which( is.na(strata$SIZE_CLASS_LOW))
  invalid_size_class = which(!is_value_positive(strata$SIZE_CLASS_LOW))
  invalid_size_class = invalid_size_class[ ! invalid_size_class %in% missing_size_class ]
  missing_size_class = missing_size_class[ ! missing_size_class %in% strata_empty_rows ]

  data_validation_results$strata$checks$size_class = list(
    missing = list(
      number      = length(missing_size_class),
      row_indexes = spreadsheet_rows_for(form, missing_size_class)
    ),
    invalid = list(
      number        = length(invalid_size_class),
      row_indexes   = spreadsheet_rows_for(form, invalid_size_class),
      values        = strata$SIZE_CLASS_LOW[invalid_size_class],
      values_unique = unique(strata$SIZE_CLASS_LOW[invalid_size_class])
    )
  )

  records = form@data$records

  sizes_original = records$data$CE_SF_data_original
  sizes          = records$data$CE_SF_data

  numeric_sizes = sizes_original[, lapply(.SD, function(value) { lapply(value, function(v) { is.na(v) | is_numeric(v) }) })]

  non_num_sizes_samples  = numeric_sizes$NUM_SAMPLES == FALSE
  non_num_sizes_fish     = numeric_sizes$NUM_FISH    == FALSE

  na_samples       = which(non_num_sizes_samples == FALSE    & is.na(sizes$NUM_SAMPLES), arr.ind = TRUE)
  zero_samples     = which(numeric_sizes$NUM_SAMPLES == TRUE & sizes$NUM_SAMPLES == 0,   arr.ind = TRUE)
  positive_samples = which(numeric_sizes$NUM_SAMPLES == TRUE & sizes$NUM_SAMPLES  > 0,   arr.ind = TRUE)
  negative_samples = which(numeric_sizes$NUM_SAMPLES == TRUE & sizes$NUM_SAMPLES  < 0,   arr.ind = TRUE)

  na_fish       = which(non_num_sizes_fish == FALSE  & is.na(sizes$NUM_FISH), arr.ind = TRUE)
  zero_fish     = which(numeric_sizes$NUM_FISH == TRUE & sizes$NUM_FISH == 0, arr.ind = TRUE)
  positive_fish = which(numeric_sizes$NUM_FISH == TRUE & sizes$NUM_FISH  > 0, arr.ind = TRUE)
  negative_fish = which(numeric_sizes$NUM_FISH == TRUE & sizes$NUM_FISH  < 0, arr.ind = TRUE)

  data_validation_results$records$checks = list(
    samples = list(
      na = list(
        number = length(na_samples),
        row_indexes = spreadsheet_rows_for(form, na_samples)
      ),
      zero = list(
        number = length(zero_samples),
        row_indexes = spreadsheet_rows_for(form, zero_samples)
      ),
      positive = list(
        number = length(positive_samples),
        row_indexes = spreadsheet_rows_for(form, positive_samples)
      ),
      negative = list(
        number = length(negative_samples),
        row_indexes = spreadsheet_rows_for(form, negative_samples)
      ),
      non_num  = list(
        number = length(which(non_num_sizes_samples == TRUE)),
        row_indexes = spreadsheet_rows_for(form, which(non_num_sizes_samples == TRUE))
      )
    ),
    fish = list(
      na = list(
        number = length(na_fish),
        row_indexes = spreadsheet_rows_for(form, na_fish)
      ),
      zero = list(
        number = length(zero_fish),
        row_indexes = spreadsheet_rows_for(form, zero_fish)
      ),
      positive = list(
        number = length(positive_fish),
        row_indexes = spreadsheet_rows_for(form, positive_fish)
      ),
      negative = list(
        number = length(negative_fish),
        row_indexes = spreadsheet_rows_for(form, negative_fish)
      ),
      non_num  = list(
        number = length(which(non_num_sizes_fish == TRUE)),
        row_indexes = spreadsheet_rows_for(form, which(non_num_sizes_fish == TRUE))
      )
    )
  )

  return(data_validation_results)
})

setMethod("data_validation_summary", list(form = "IOTCForm4SFUpdate", metadata_validation_results = "list", data_validation_results = "list"), function(form, metadata_validation_results, data_validation_results) {
  l_info("IOTCForm4SFUpdate.data_validation_summary")

  validation_messages = common_data_validation_summary(form,
                                                       metadata_validation_results,
                                                       data_validation_results)

  ### STRATA AND RECORDS

  strata  = data_validation_results$strata
  records = data_validation_results$records

  checks_strata  = strata$checks
  checks_records = records$checks

  # Strata issues / summary

  # Validation comes from the superclass

  # Strata checks

  ## Main strata

  if(strata$checks$main$grids$invalid$number > 0) {
    validation_messages = add(validation_messages, new("Message", level = "ERROR", source = "Data", column = "C", text = paste0(strata$checks$main$grids$invalid$number, " invalid grid code(s) reported. Please refer to ", reference_codes("admin", "IOTCgridsCESF"), " for a list of valid grid codes for this dataset")))

    for(row in strata$checks$main$grids$invalid$row_indexes)
      validation_messages = add(validation_messages, new("Message", level = "ERROR", source = "Data", row = row, column = "C", text = paste0("Invalid grid code in row #", row)))
  }

  if(strata$checks$main$grids$wrong$number > 0) {
    if(strata$checks$main$grids$wrong$number > 1) validation_messages = add(validation_messages, new("Message", level = "ERROR", source = "Data", column = "C", text = paste0(strata$checks$main$grids$wrong$number, " grid codes refer to the wrong type of grid for the fishery")))

    for(row in strata$checks$main$grids$wrong$row_indexes)
      validation_messages = add(validation_messages, new("Message", level = "ERROR", source = "Data", row = row, column = "C", text = paste0("Wrong type of grid for the fishery in row #", row)))
  }

  if(strata$duplicate$number > 0)
    validation_messages = add(validation_messages, new("Message", level = "FATAL", source = "Data", text = paste0(strata$duplicate$number, " duplicate strata detected: see row(s) #", paste0(strata$duplicate$row_indexes, collapse = ", "))))

  ## Sex

  validation_messages = report_sex(validation_messages, checks_strata$sex)

  ## Size class

  validation_messages = report_size_class(validation_messages, checks_strata$size_class)

  # Data issues / summary

  ## Number of samples

  validation_messages = report_number_of_samples(validation_messages, checks_records$samples)

  ## Number of fish

  validation_messages = report_number_of_fish(validation_messages, checks_records$fish)

  return(validation_messages)
})

## OUTPUT

setMethod("extract_output", list(form = "IOTCForm4SFUpdate", wide = "logical"),
          function(form, wide) {
            form = read(form)

            form_metadata = extract_metadata(form, common_metadata(form@original_metadata))
            form_data     = extract_data(form)

            strata = form_data$strata
            data   = form_data$records$data$CE_SF_data

            year = form_metadata$general_information$reporting_year
            fleet = fleets_for(form_metadata$general_information$reporting_entity,
                               form_metadata$general_information$flag_country)

            strata$YEAR                  = year
            strata$REPORTING_ENTITY_CODE = form_metadata$general_information$reporting_entity
            strata$FLAG_COUNTRY_CODE     = form_metadata$general_information$flag_country
            strata$FLEET_CODE            = fleet$FLEET_CODE

            strata$FISHERY_CODE          = form_metadata$general_information$fishery
            strata$SPECIES_CODE          = form_metadata$general_information$species

            strata$DATA_TYPE_CODE        = form_metadata$data_specifications$type_of_data
            strata$DATA_SOURCE_CODE      = form_metadata$data_specifications$data_source
            strata$DATA_PROCESSING_CODE  = form_metadata$data_specifications$data_processing
            strata$DATA_RAISING_CODE     = form_metadata$data_specifications$data_raising
            strata$COVERAGE_TYPE_CODE    = form_metadata$data_specifications$coverage_type
            strata$COVERAGE              = form_metadata$data_specifications$coverage_value

            strata$MEASUREMENT_TYPE_CODE = form_metadata$data_specifications$measurements$type
            strata$MEASURE_CODE          = form_metadata$data_specifications$measurements$measure
            strata$MEASURING_TOOL_CODE   = form_metadata$data_specifications$measurements$tool
            #strata$SIZE_INTERVAL         = form_metadata$data_specifications$measurements$interval

            strata$FATE_TYPE_CODE        = form_metadata$data_specifications$fate$type
            strata$FATE_CODE             = form_metadata$data_specifications$fate$fate

            # Not required when using the new fishery codes
            #strata = merge(strata, FISHERY_MAPPINGS, by = "FISHERY_CODE", all.x = TRUE, sort = FALSE)

            strata = strata[, .(REPORTING_ENTITY_CODE, FLAG_COUNTRY_CODE, FLEET_CODE,
                                YEAR, MONTH,
                                FISHERY_CODE,
                               #GEAR_CODE, MAIN_GEAR_CODE, SCHOOL_TYPE_CODE,
                                DATA_TYPE_CODE, DATA_SOURCE_CODE, DATA_PROCESSING_CODE, DATA_RAISING_CODE, COVERAGE_TYPE_CODE, COVERAGE,
                                GRID_CODE, ESTIMATION_CODE,
                                SPECIES_CODE, MEASUREMENT_TYPE_CODE, MEASURE_CODE, MEASURING_TOOL_CODE,
                                FATE_TYPE_CODE, FATE_CODE, SEX_CODE,
                                SIZE_CLASS_LOW, SIZE_CLASS_HIGH = SIZE_CLASS_LOW + as.numeric(form_metadata$data_specifications$measurements$interval) - 1,
                                NUM_SAMPLES_STRATA = NA_real_)]

            output_data = cbind(strata, data)

            output_data[, NUM_SAMPLES := round(as.numeric(output_data$NUM_SAMPLES), 2)]
            output_data[, NUM_FISH    := round(as.numeric(output_data$NUM_FISH),    2)]

            output_data =
              output_data[, NUM_SAMPLES_STRATA := sum(NUM_SAMPLES, na.rm = TRUE), by = .(REPORTING_ENTITY_CODE, FLAG_COUNTRY_CODE, FLEET_CODE,
                                                                                         YEAR, MONTH,
                                                                                         FISHERY_CODE,
                                                                                        #GEAR_CODE, MAIN_GEAR_CODE, SCHOOL_TYPE_CODE,
                                                                                         DATA_TYPE_CODE, DATA_SOURCE_CODE, DATA_PROCESSING_CODE, DATA_RAISING_CODE, COVERAGE_TYPE_CODE, COVERAGE,
                                                                                         GRID_CODE, ESTIMATION_CODE,
                                                                                         SPECIES_CODE, MEASUREMENT_TYPE_CODE, MEASURE_CODE, MEASURING_TOOL_CODE,
                                                                                         FATE_TYPE_CODE, FATE_CODE, SEX_CODE)]

            if(!wide) {
              output_data = output_data # no difference
            }

            return(output_data)
          }
)
