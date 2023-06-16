#' @include IOTC_form_CESF_multiple_class.R
#' @export IOTCForm4SFMultiple
IOTCForm4SFMultiple = setClass(
  "IOTCForm4SFMultiple",
  contains = "IOTCFormCESFMultiple"
)

setMethod("form_type", "IOTCForm4SFMultiple", function(form) {
  return("4-SF-multiple")
})

setMethod("form_version", "IOTCForm4SFMultiple", function(form) {
  return("1.0.0-legacy")
})

setMethod("form_dataset_code", "IOTCForm4SFMultiple", function(form) {
  return("SF")
})

setMethod("grid_validator", "IOTCForm4SFMultiple", function(form) {
  return(is_grid_CE_SF_valid)
})

setMethod("allow_empty_data_multiple", "IOTCForm4SFMultiple", function(form) {
  return(FALSE)
})

setMethod("estimation_column", "IOTCForm4SFMultiple", function(form) {
  return("J")
})

setMethod("optional_strata_columns", "IOTCForm4SFMultiple", function(form) {
  return(c()) # None
})

setMethod("first_data_column", "IOTCForm4SFMultiple", function(form) {
  return(which(EXCEL_COLUMNS == "V"))
})

setMethod("first_data_row", "IOTCForm4SFMultiple", function(form) {
  return(6)
})

setMethod("first_strata_column", "IOTCForm4SFMultiple", function(form) {
  return(which(EXCEL_COLUMNS == "B"))
})

setMethod("last_strata_column", "IOTCForm4SFMultiple", function(form) {
  return(which(EXCEL_COLUMNS == "U"))
})

setMethod("validate_months_multiple", list(form = "IOTCForm4SFMultiple", strata = "data.table"), function(form, strata) {
  start = Sys.time()
  l_info("IOTCForm4SFMultiple.validate_months")

# valid_months_strata   = strata[MONTH %in% 1:12, .(NUM_MONTHS = .N), keyby = .(FISHERY_CODE, TARGET_SPECIES_CODE, GRID_CODE, SPECIES_CODE, SEX_CODE, FATE_TYPE_CODE, FATE_CODE,
#                                                                               DATA_SOURCE_CODE, DATA_PROCESSING_CODE,
#                                                                               MEASUREMENT_TYPE_CODE, MEASURE_CODE,
#                                                                               SIZE_CLASS_LOW, SIZE_CLASS_HIGH)]

  valid_months_strata   = strata[MONTH %in% 1:12, .(NUM_MONTHS = .N), keyby = .(FISHERY_CODE, TARGET_SPECIES_CODE, SPECIES_CODE)]

  incomplete_months_strata  = valid_months_strata[NUM_MONTHS < 12]

# incomplete_months  = merge(strata, incomplete_months_strata, all.x = TRUE, sort = FALSE, by = c("FISHERY_CODE", "TARGET_SPECIES_CODE", "GRID_CODE", "SPECIES_CODE", "SEX_CODE", "FATE_TYPE_CODE", "FATE_CODE",
  incomplete_months  = merge(strata, incomplete_months_strata, all.x = TRUE, sort = FALSE, by = c("FISHERY_CODE", "TARGET_SPECIES_CODE", "SPECIES_CODE"))
  incomplete_months  = which(!is.na(incomplete_months$NUM_MONTHS))

  l_debug(paste0("IOTCForm4SFMultiple.validate_months: ", Sys.time() - start))

  return(
    list(
      incomplete_months  = incomplete_months
    )
  )
})

setMethod("extract_data", "IOTCForm4SFMultiple", function(form) {
  start = Sys.time()
  l_info("IOTCForm4SFMultiple.extract_data")

  form_metadata = form@original_metadata
  form_data     = form@original_data

  has_data = nrow(form_data) >= 4

  strata = form_data[4:ifelse(has_data, nrow(form_data), 4)][, first_strata_column(form):last_strata_column(form)]

  if(!has_data) {
    strata = as.data.table(matrix(nrow = 0, ncol = length(colnames(strata))))
  }

  colnames(strata) = c("MONTH", "FISHERY_CODE", "TARGET_SPECIES_CODE", "GRID_CODE", "SPECIES_CODE", "SEX_CODE", "FATE_TYPE_CODE", "FATE_CODE", "ESTIMATION_CODE",
                       "DATA_TYPE_CODE", "DATA_SOURCE_CODE", "DATA_PROCESSING_CODE", "DATA_RAISING_CODE",
                       "COVERAGE_TYPE_CODE", "COVERAGE",
                       "MEASUREMENT_TYPE_CODE", "MEASURE_CODE", "MEASURING_TOOL_CODE",
                       "SIZE_CLASS_LOW", "SIZE_CLASS_HIGH")

  strata[, MONTH    := as.integer(MONTH)]

  records = form_data[4:ifelse(has_data, nrow(form_data), 4), first_data_column(form):ncol(form_data)]

  if(has_data) {
    records_original = data.table(NUM_SAMPLES = records[, 1], NUM_FISH = records[, 2])
    # Might raise the "Warning in FUN(X[[i]], ...) : NAs introduced by coercion" message when catches include non-numeric values...
    records = records_original[, lapply(.SD, function(value) { return(round(as.numeric(value), 2)) })]
  } else {
    records_original = as.data.table(matrix(nrow = 0, ncol = 2))
    colnames(records_original) = c("NUM_SAMPLES", "NUM_FISH")
    records          = records_original
  }

  l_debug(paste0("IOTCForm4SFMultiple.extract_data: ", Sys.time() - start))

  return(
    list(
      strata = strata,
      records =
        list(
          data = list(
            CE_SF_data_original  = records_original,
            CE_SF_data           = records,
            num_samples_original = records_original$NUM_SAMPLES,
            num_samples          = records$NUM_SAMPLES,
            num_fish_original    = records_original$NUM_FISH,
            num_fish             = records$NUM_FISH
          )
        )
    )
  )
})

setMethod("validate_data", list(form = "IOTCForm4SFMultiple", metadata_validation_results = "list"), function(form, metadata_validation_results) {
  start = Sys.time()
  l_info("IOTCForm4SFMultiple.validate_data")

  data_validation_results = callNextMethod(form, metadata_validation_results)

  l_debug(paste0("IOTCForm4SFMultiple.validate_data (I): ", Sys.time() - start))
  start = Sys.time()

  strata  = form@data$strata

  strata_empty_rows    = find_empty_rows(strata)
  strata_empty_columns = find_empty_columns(strata[, 1:3]) # Effort values shall not be considered, as some of them (either secondary, or tertiary, or both) might be left all empty

  l_debug(paste0("IOTCForm4SFMultiple.validate_data (II): ", Sys.time() - start))
  start = Sys.time()

  strata[, IS_EMPTY := .I %in% strata_empty_rows]
  strata[, OCCURRENCES := .N, by = .(MONTH, FISHERY_CODE, TARGET_SPECIES_CODE, GRID_CODE, SPECIES_CODE, SEX_CODE, FATE_TYPE_CODE, FATE_CODE,
                                     DATA_SOURCE_CODE, DATA_PROCESSING_CODE,
                                     MEASUREMENT_TYPE_CODE, MEASURE_CODE,
                                     SIZE_CLASS_LOW, SIZE_CLASS_HIGH)]

  valid_months_strata   = strata[MONTH %in% 1:12, .(NUM_MONTHS = .N), keyby = .(FISHERY_CODE, TARGET_SPECIES_CODE, SPECIES_CODE)]

  incomplete_months_strata  = valid_months_strata[NUM_MONTHS < 12]

  incomplete_months  = merge(strata, incomplete_months_strata, all.x = TRUE, sort = FALSE, by = c("FISHERY_CODE", "TARGET_SPECIES_CODE", "SPECIES_CODE"))
  incomplete_months  = which(!is.na(incomplete_months$NUM_MONTHS))

  l_debug(paste0("IOTCForm4SFMultiple.validate_data (III): ", Sys.time() - start))
  start = Sys.time()

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

  # Merges the fishery codes in the strata with the LEGACY_FISHERIES table in order to recover - when possible - the fishery category
  fishery_categories = merge(strata, iotc.data.reference.codelists::LEGACY_FISHERIES[, .(CODE, FISHERY_CATEGORY)],
                             by.x = "FISHERY_CODE", by.y = "CODE")$FISHERY_CATEGORY

  grid_status    = data.table(FISHERY_CATEGORY_CODE = fishery_categories,
                              GRID_CODE = strata$GRID_CODE,
                              MISSING   = is.na(strata$GRID_CODE),
                              VALID     = grid_validator(form)(strata$GRID_CODE),
                              SIZE      = grid_size(strata$GRID_CODE))

  grid_status[, WRONG_GRID_TYPE := SIZE == "OTHER"]

  wrong_grid_types = which(grid_status$WRONG_GRID_TYPE == TRUE)

  data_validation_results$strata$checks$main$grids$wrong = list(
    number       = length(wrong_grid_types),
    row_indexes  = wrong_grid_types,
    codes        = strata$GRID_CODE[wrong_grid_types],
    codes_unique = unique(strata$GRID_CODE[wrong_grid_types])
  )

  l_debug(paste0("IOTCForm4SFMultiple.validate_data (IV): ", Sys.time() - start))
  start = Sys.time()

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

  l_debug(paste0("IOTCForm4SFMultiple.validate_data (V): ", Sys.time() - start))
  start = Sys.time()

  ### Species code

  missing_species  = which( is.na(strata$SPECIES_CODE))
  invalid_species  = which(!is_species_valid(strata$SPECIES_CODE))
  invalid_species  = invalid_species[ ! invalid_species %in% missing_species ]
  missing_species  = missing_species[ ! missing_species %in% strata_empty_rows ]

  species_aggregates     = which(is_species_aggregate(strata$SPECIES_CODE))
  species_aggregates     = species_aggregates[ ! species_aggregates %in% missing_species ]

  l_debug(paste0("IOTCForm4SFMultiple.validate_data (VI): ", Sys.time() - start))
  start = Sys.time()

  ### Sex code

  missing_sex = which( is.na(strata$SEX_CODE))
  invalid_sex = which(!is_sex_valid(strata$SEX_CODE))
  invalid_sex = invalid_sex[ ! invalid_sex %in% missing_sex ]
  missing_sex = missing_sex[ ! missing_sex %in% strata_empty_rows ]

  l_debug(paste0("IOTCForm4SFMultiple.validate_data (VII): ", Sys.time() - start))
  start = Sys.time()

  ### Type of fate / fate code

  missing_fate_type = which( is.na(strata$FATE_TYPE_CODE))
  invalid_fate_type = which(!is_fate_type_valid(strata$FATE_TYPE_CODE))
  invalid_fate_type = invalid_fate_type[ ! invalid_fate_type %in% missing_fate_type ]
  missing_fate_type = missing_fate_type[ ! missing_fate_type %in% strata_empty_rows ]

  missing_fate = which( is.na(strata$FATE_CODE))
  invalid_fate = which(!is.na(strata$FATE_CODE) & !is_fate_valid(strata$FATE_TYPE_CODE, strata$FATE_CODE))
  invalid_fate = invalid_fate[ ! invalid_fate %in% missing_fate ]
  missing_fate = missing_fate[ ! missing_fate %in% strata_empty_rows ]

  l_debug(paste0("IOTCForm4SFMultiple.validate_data (VIII): ", Sys.time() - start))
  start = Sys.time()

  ### Measurement type / measure / tool code

  missing_measurement_type = which( is.na(strata$MEASUREMENT_TYPE_CODE))
  invalid_measurement_type = which(!is.na(strata$MEASUREMENT_TYPE_CODE) & !is_measurement_type_valid(strata$MEASUREMENT_TYPE_CODE))
  invalid_measurement_type = invalid_measurement_type[ ! invalid_measurement_type %in% missing_measurement_type ]
  missing_measurement_type = missing_measurement_type[ ! missing_measurement_type %in% strata_empty_rows ]

  missing_measure = which( is.na(strata$MEASURE_CODE))
  invalid_measure = which(!is.na(strata$MEASURE_CODE) & !is_measurement_valid(strata$MEASUREMENT_TYPE_CODE, strata$MEASURE_CODE))
  invalid_measure = invalid_measure[ ! invalid_measure %in% missing_measure ]
  missing_measure = missing_measure[ ! missing_measure %in% strata_empty_rows ]

  missing_measuring_tool = which( is.na(strata$MEASURING_TOOL_CODE))
  invalid_measuring_tool = which(!is.na(strata$MEASURING_TOOL_CODE) & !is_measuring_tool_valid(strata$MEASUREMENT_TYPE_CODE, strata$MEASURING_TOOL_CODE))
  invalid_measuring_tool = invalid_measuring_tool[ ! invalid_measuring_tool %in% missing_measuring_tool ]
  missing_measuring_tool = missing_measuring_tool[ ! missing_measuring_tool %in% strata_empty_rows ]

  l_debug(paste0("IOTCForm4SFMultiple.validate_data (IX): ", Sys.time() - start))
  start = Sys.time()

  ### Class low / high

  missing_size_class_low = which( is.na(strata$SIZE_CLASS_LOW))
  invalid_size_class_low = which(!is_value_positive(strata$SIZE_CLASS_LOW))
  invalid_size_class_low = invalid_size_class_low[ ! invalid_size_class_low %in% missing_size_class_low ]
  missing_size_class_low = missing_size_class_low[ ! missing_size_class_low %in% strata_empty_rows ]

  missing_size_class_high = which( is.na(strata$SIZE_CLASS_HIGH))
  invalid_size_class_high = which(!is_value_positive(strata$SIZE_CLASS_HIGH))
  invalid_size_class_high = invalid_size_class_low[ ! invalid_size_class_high %in% missing_size_class_high ]
  missing_size_class_high = missing_size_class_low[ ! missing_size_class_high %in% strata_empty_rows ]

  l_debug(paste0("IOTCForm4SFMultiple.validate_data (X): ", Sys.time() - start))
  start = Sys.time()

  check_size_classes = function(low, high) { return(low < high) }

  invalid_size_classes = which(!mapply(check_size_classes, strata$SIZE_CLASS_LOW, strata$SIZE_CLASS_HIGH))
  invalid_size_classes = invalid_size_classes[ ! invalid_size_classes %in% missing_size_class_low &
                                               ! invalid_size_classes %in% missing_size_class_high ]

  l_debug(paste0("IOTCForm4SFMultiple.validate_data (XI): ", Sys.time() - start))
  start = Sys.time()

  ### TODO: add a check that the size bin is not >> than what expected for the class

  data_validation_results$strata$checks$main$species = list(
    invalid = list(
      number       = length(invalid_species),
      row_indexes  = spreadsheet_rows_for(form, invalid_species),
      codes        = strata$SPECIES_CODE[invalid_species],
      codes_unique = unique(strata$SPECIES_CODE[invalid_species])
    ),
    missing = list(
      number      = length(missing_species),
      row_indexes = spreadsheet_rows_for(form, missing_species)
    ),
    aggregates = list(
      number       = length(species_aggregates),
      row_indexes  = spreadsheet_rows_for(form, species_aggregates),
      codes        = strata[species_aggregates]$SPECIES_CODE,
      codes_unique = unique(strata[species_aggregates]$SPECIES_CODE)
    )
  )

  data_validation_results$strata$checks$main$sex = list(
    invalid = list(
      number       = length(invalid_sex),
      row_indexes  = spreadsheet_rows_for(form, invalid_sex),
      codes        = strata$SEX_CODE[invalid_sex],
      codes_unique = unique(strata$SEX_CODE[invalid_sex])
    ),
    missing = list(
      number      = length(missing_sex),
      row_indexes = spreadsheet_rows_for(form, missing_sex)
    )
  )

  data_validation_results$strata$checks$main$fate = list(
    type = list(
      invalid = list(
        number       = length(invalid_fate_type),
        row_indexes  = spreadsheet_rows_for(form, invalid_fate_type),
        codes        = strata$FATE_TYPE_CODE[invalid_fate_type],
        codes_unique = unique(strata$FATE_TYPE_CODE[invalid_fate_type])
      ),
      missing = list(
        number      = length(missing_fate_type),
        row_indexes = spreadsheet_rows_for(form, missing_fate_type)
      )
    ),
    fate = list(
      invalid = list(
        number       = length(invalid_fate),
        row_indexes  = spreadsheet_rows_for(form, invalid_fate),
        codes        = strata$FATE_CODE[invalid_fate],
        codes_unique = unique(strata$FATE_CODEv[invalid_fate])
      ),
      missing = list(
        number      = length(missing_fate),
        row_indexes = spreadsheet_rows_for(form, missing_fate)
      )
    )
  )

  data_validation_results$strata$checks$measurement = list(
    type = list(
      invalid = list(
        number       = length(invalid_measurement_type),
        row_indexes  = spreadsheet_rows_for(form, invalid_measurement_type),
        codes        = strata$MEASUREMENT_TYPE_CODE[invalid_measurement_type],
        codes_unique = unique(strata$MEASUREMENT_TYPE_CODE[invalid_measurement_type])
      ),
      missing = list(
        number      = length(missing_measurement_type),
        row_indexes = spreadsheet_rows_for(form, missing_measurement_type)
      )
    ),
    measure = list(
      invalid = list(
        number       = length(invalid_measure),
        row_indexes  = spreadsheet_rows_for(form, invalid_measure),
        codes        = strata$MEASURE_CODE[invalid_measure],
        codes_unique = unique(strata$MEASURE_CODE[invalid_measure])
      ),
      missing = list(
        number      = length(missing_measure),
        row_indexes = spreadsheet_rows_for(form, missing_measure)
      )
    ),
    measuring_tool = list(
      invalid = list(
        number       = length(invalid_measuring_tool),
        row_indexes  = spreadsheet_rows_for(form, invalid_measuring_tool),
        codes        = strata$MEASURING_TOOL_CODE[invalid_measuring_tool],
        codes_unique = unique(strata$MEASURING_TOOL_CODE[invalid_measuring_tool])
      ),
      missing = list(
        number      = length(missing_measuring_tool),
        row_indexes = spreadsheet_rows_for(form, missing_measuring_tool)
      )
    )
  )

  data_validation_results$strata$checks$size_classes = list(
    low = list(
      missing = list(
        number       = length(missing_size_class_low),
        row_indexes  = spreadsheet_rows_for(form, missing_size_class_low)
      ),
      invalid = list(
        number       = length(invalid_size_class_low),
        row_indexes  = spreadsheet_rows_for(form, invalid_size_class_low)
      )
    ),
    high = list(
      missing = list(
        number       = length(missing_size_class_high),
        row_indexes  = spreadsheet_rows_for(form, missing_size_class_high)
      ),
      invalid = list(
        number       = length(invalid_size_class_high),
        row_indexes  = spreadsheet_rows_for(form, invalid_size_class_high)
      )
    ),
    invalid = list(
      number      = length(invalid_size_classes),
      row_indexes = spreadsheet_rows_for(form, invalid_size_classes)
    )
  )

  start = Sys.time()

  records = form@data$records$data

  num_samples_original = records$num_samples_original
  num_samples          = records$num_samples

  is_value_numeric = function(value) { return(is.na(value) | is_numeric(value)) }

  numeric_num_samples = ifelse(length(num_samples_original) == 0, num_samples_original, is_value_numeric(num_samples_original))

  na_samples       = which(numeric_num_samples == TRUE & is.na(num_samples), arr.ind = TRUE)
  zero_samples     = which(numeric_num_samples == TRUE & num_samples == 0,   arr.ind = TRUE)
  negative_samples = which(numeric_num_samples == TRUE & num_samples  < 0,   arr.ind = TRUE)
  positive_samples = which(numeric_num_samples == TRUE & num_samples  > 0,   arr.ind = TRUE)

  num_fish_original = records$num_fish_original
  num_fish          = records$num_fish

  numeric_num_fish = ifelse(length(num_fish_original) == 0, num_fish_original, is_value_numeric(num_fish_original))

  na_fish       = which(numeric_num_fish == TRUE & is.na(num_fish), arr.ind = TRUE)
  zero_fish     = which(numeric_num_fish == TRUE & num_fish == 0,   arr.ind = TRUE)
  negative_fish = which(numeric_num_fish == TRUE & num_fish  < 0,   arr.ind = TRUE)
  positive_fish = which(numeric_num_fish == TRUE & num_fish  > 0,   arr.ind = TRUE)

  l_debug(paste0("IOTCForm4SFMultiple.validate_data (XII): ", Sys.time() - start))

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
        number = length(which(numeric_num_samples == FALSE)),
        row_indexes = spreadsheet_rows_for(form, which(numeric_num_samples == FALSE))
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
        number = length(which(numeric_num_fish == FALSE)),
        row_indexes = spreadsheet_rows_for(form, which(numeric_num_fish == FALSE))
      )
    )
  )

  l_debug(paste0("IOTCForm4SFMultiple.validate_data: ", Sys.time() - start))

  return(data_validation_results)
})

setMethod("data_validation_summary", list(form = "IOTCForm4SFMultiple", metadata_validation_results = "list", data_validation_results = "list"), function(form, metadata_validation_results, data_validation_results) {
  start = Sys.time()
  l_info("IOTCForm4SFMultiple.data_validation_summary")

  validation_messages = common_data_validation_summary(form,
                                                       metadata_validation_results,
                                                       data_validation_results)

  ### STRATA AND RECORDS

  # This is only true for 3CE / surface

  fishery_info        = metadata_validation_results$general_information$fishery

  strata  = data_validation_results$strata
  records = data_validation_results$records

  checks_strata  = strata$checks
  checks_records = records$checks

  # Strata issues / summary

  # Validation comes from the superclass

  # Strata checks

  ## Main strata

  if(checks_strata$main$grids$invalid$number > 0) {
    validation_messages = add(validation_messages, new("Message", level = "ERROR", source = "Data", column = "E", text = paste0(checks_strata$main$grids$invalid$number, " invalid grid code(s) reported. Please refer to ", reference_codes("admin", "IOTCgridsCESF"), " for a list of valid grid codes for this dataset")))

    for(row in checks_strata$main$grids$invalid$row_indexes)
      validation_messages = add(validation_messages, new("Message", level = "ERROR", source = "Data", row = row, column = "E", text = paste0("Invalid grid code in row #", row)))
  }

  if(checks_strata$main$grids$wrong$number > 0) {
    if(checks_strata$main$grids$wrong$number > 1) validation_messages = add(validation_messages, new("Message", level = "ERROR", source = "Data", column = "E", text = paste0(checks_strata$main$grids$wrong$number, " grid codes refer to the wrong type of grid for the fishery")))

    for(row in checks_strata$main$grids$wrong$row_indexes)
      validation_messages = add(validation_messages, new("Message", level = "ERROR", source = "Data", row = row, column = "E", text = paste0("Wrong type of grid for the fishery in row #", row)))
  }

  ## Species

  species = checks_strata$main$species

  if(species$missing$number > 0) {
    if(species$missing$number > 1) validation_messages = add(validation_messages, new("Message", level = "ERROR", source = "Data", text = paste0(species$missing$number, " missing species codes")))

    for(row_index in species$missing$row_indexes) {
      validation_messages = add(validation_messages, new("Message", level = "ERROR", source = "Data", row = as.integer(row_index), column = "F", text = paste0("Missing species code in row #", row_index)))
    }
    #validation_messages = add(validation_messages, new("Message", level = "ERROR", source = "Data", text = paste0("Missing species code in row(s) #", paste0(species$missing$row_indexes, collapse = ", "))))
  }

  if(species$invalid$number > 0) {
    if(species$invalid$number > 1) validation_messages = add(validation_messages, new("Message", level = "ERROR", source = "Data", text = paste0(species$invalid$number, " invalid species codes")))

    for(row_index in species$invalid$row_indexes) {
      validation_messages = add(validation_messages, new("Message", level = "ERROR", source = "Data", row = as.integer(row_index), column = "F", text = paste0("Invalid species code in row #", row_index)))
    }
    #validation_messages = add(validation_messages, new("Message", level = "ERROR", source = "Data", text = paste0("Invalid species code in row(s) #", paste0(species$invalid$row_indexes, collapse = ", "), ". Please refer to ", reference_codes("biological", "allSpecies"), " for a list of valid species codes")))
  }

  if(species$aggregates$number > 0) {
    if(species$aggregates$number > 1) validation_messages = add(validation_messages, new("Message", level = "ERROR", source = "Data", text = paste0(species$aggregates$number, " aggregate species codes")))

    for(row_index in species$aggregates$row_indexes) {
      validation_messages = add(validation_messages, new("Message", level = "ERROR", source = "Data", row = as.integer(row_index), column = "F", text = paste0("Aggregate species code in row #", row_index)))
    }
    #validation_messages = add(validation_messages, new("Message", level = "ERROR", source = "Data", text = paste0("Species code refers to a species aggregate in row(s) #", paste0(species$aggregates$row_indexes, collapse = ", "), ". Please refer to ", reference_codes("biological", "allSpecies"), " for a list of valid distinct species codes")))
  }
  ## Sex

  validation_messages = report_sex(validation_messages, checks_strata$main$sex)

  ## Fate type

  fate_type = checks_strata$main$fate$type

  if(fate_type$missing$number > 0)
    validation_messages = add(validation_messages, new("Message", level = "ERROR", source = "Data", text = paste0("Missing fate type code in row(s) #", paste0(fate_type$missing$row_indexes, collapse = ", "))))

  if(fate_type$invalid$number > 0)
    validation_messages = add(validation_messages, new("Message", level = "ERROR", source = "Data", text = paste0("Invalid fate type code in row(s) #", paste0(fate_type$invalid$row_indexes, collapse = ", "), ". Please refer to ", reference_codes("biological", "typesOfFate"), " for a list of valid fate type codes")))

  ## Fate

  fate = checks_strata$main$fate$fate

  if(fate$missing$number > 0)
    validation_messages = add(validation_messages, new("Message", level = "ERROR", source = "Data", text = paste0("Missing fate code in row(s) #", paste0(fate$missing$row_indexes, collapse = ", "))))

  if(fate$invalid$number > 0)
    validation_messages = add(validation_messages, new("Message", level = "ERROR", source = "Data", text = paste0("Invalid fate code in row(s) #", paste0(fate$invalid$row_indexes, collapse = ", "), ". Please refer to ", reference_codes("biological", "fates"), " for a list of valid fate codes by type of fate")))

  ## Measurement type

  measurement_type = checks_strata$measurement$type

  if(measurement_type$missing$number > 0)
    validation_messages = add(validation_messages, new("Message", level = "ERROR", source = "Data", text = paste0("Missing measurement type code in row(s) #", paste0(measurement_type$missing$row_indexes, collapse = ", "))))

  if(measurement_type$invalid$number > 0)
    validation_messages = add(validation_messages, new("Message", level = "ERROR", source = "Data", text = paste0("Invalid measurement type code in row(s) #", paste0(measurement_type$invalid$row_indexes, collapse = ", "), ". Please refer to ", reference_codes("biological", "typesOfMeasurement"), " for a list of valid measurement type codes")))

  ## Measure

  measure = checks_strata$measurement$measure

  if(measure$missing$number > 0)
    validation_messages = add(validation_messages, new("Message", level = "ERROR", source = "Data", text = paste0("Missing measure code in row(s) #", paste0(measure$missing$row_indexes, collapse = ", "))))

  if(measure$invalid$number > 0)
    validation_messages = add(validation_messages, new("Message", level = "ERROR", source = "Data", text = paste0("Invalid measure code in row(s) #", paste0(measure$invalid$row_indexes, collapse = ", "), ". Please refer to ", reference_codes("biological", "allMeasurementTypes"), " for a list of valid measure codes by type of measurement")))


  ## Measure

  measuring_tool = checks_strata$measurement$measuring_tool

  if(measuring_tool$missing$number > 0)
    validation_messages = add(validation_messages, new("Message", level = "ERROR", source = "Data", text = paste0("Missing measuring tool code in row(s) #", paste0(measuring_tool$missing$row_indexes, collapse = ", "))))

  if(measuring_tool$invalid$number > 0)
    validation_messages = add(validation_messages, new("Message", level = "ERROR", source = "Data", text = paste0("Invalid measuring tool code in row(s) #", paste0(measuring_tool$invalid$row_indexes, collapse = ", "), ". Please refer to ", reference_codes("biological", "allMeasurementTools"), " for a list of valid measuring tool codes by type of measurement")))

  # Data issues / summary

  ## Number of samples

  validation_messages = report_number_of_samples(validation_messages, checks_records$samples)

  ## Number of fish

  validation_messages = report_number_of_fish(validation_messages, checks_records$fish)

  l_debug(paste0("IOTCForm4SFMultiple.data_validation_summary: ", Sys.time() - start))

  return(validation_messages)
})
