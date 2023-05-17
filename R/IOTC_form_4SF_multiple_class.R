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

setMethod("allow_empty_data", "IOTCForm4SFMultiple", function(form) {
  return(FALSE)
})

setMethod("optional_strata_columns", "IOTCForm4SFMultiple", function(form) {
  return(c()) # None
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

  l_info(paste0("IOTCForm4SFMultiple.validate_months: ", Sys.time() - start))

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

  strata = form_data[4:nrow(form_data)][, c(2:21)]
  colnames(strata) = c("MONTH", "FISHERY_CODE", "TARGET_SPECIES_CODE", "GRID_CODE", "SPECIES_CODE", "SEX_CODE", "FATE_TYPE_CODE", "FATE_CODE", "ESTIMATION_CODE",
                       "DATA_TYPE_CODE", "DATA_SOURCE_CODE", "DATA_PROCESSING_CODE", "DATA_RAISING_CODE",
                       "COVERAGE_TYPE_CODE", "COVERAGE",
                       "MEASUREMENT_TYPE_CODE", "MEASURE_CODE", "MEASURING_TOOL_CODE",
                       "SIZE_CLASS_LOW", "SIZE_CLASS_HIGH")

  strata[, MONTH    := as.integer(MONTH)]

  records = form_data[2:nrow(form_data), 22:23]

  # Might raise the "Warning in FUN(X[[i]], ...) : NAs introduced by coercion" message when catches include non-numeric values...
  num_samples_original = records[3:nrow(records), 1]
  num_fish_original    = records[3:nrow(records), 2]

  num_samples = num_samples_original[, lapply(.SD, function(value) { return(round(as.numeric(value), 2)) })]
  num_fish    = num_fish_original   [, lapply(.SD, function(value) { return(round(as.numeric(value), 2)) })]

  l_info(paste0("IOTCForm4SFMultiple.extract_data: ", Sys.time() - start))

  return(
    list(
      strata = strata,
      records =
        list(
          data = list(
            CE_SF_data_original  = data.table(NUM_SAMPLES = num_samples_original, NUM_FISH = num_fish_original),
            CE_SF_data           = data.table(NUM_SAMPLES = num_samples, NUM_FISH = num_fish),
            num_samples_original = num_samples_original,
            num_samples          = num_samples,
            num_fish_original    = num_fish_original,
            num_fish             = num_fish
          )
        )
    )
  )
})

setMethod("validate_data", list(form = "IOTCForm4SFMultiple", metadata_validation_results = "list"), function(form, metadata_validation_results) {
  start = Sys.time()
  l_info("IOTCForm4SFMultiple.validate_data")

  data_validation_results = callNextMethod(form, metadata_validation_results)

  l_info(paste0("IOTCForm4SFMultiple.validate_data (I): ", Sys.time() - start))
  start = Sys.time()

  strata  = form@data$strata

  strata_empty_rows    = find_empty_rows(strata)
  strata_empty_columns = find_empty_columns(strata[, 1:3]) # Effort values shall not be considered, as some of them (either secondary, or tertiary, or both) might be left all empty

  l_info(paste0("IOTCForm4SFMultiple.validate_data (II): ", Sys.time() - start))
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

  l_info(paste0("IOTCForm4SFMultiple.validate_data (III): ", Sys.time() - start))
  start = Sys.time()

  grid_size = function(code) {
    if(is.na(code) || code == "") return("OTHER")
    else if(str_sub(code, 1, 1) == "5") return("1_DEG")
    else if(str_sub(code, 1, 1) == "6") return("5_DEG")
    return("OTHER")
  }

  fishery_category = memoise(function(value) {
    if(!is.na(value) && is_fishery_valid(value)) {
      return(fisheries_for(value)$FISHERY_CATEGORY)
    }

    return(NA)
  })

  grid_status    = data.table(FISHERY_CATEGORY_CODE = sapply(strata$FISHERY_CODE, fishery_category),
                              GRID_CODE = strata$GRID_CODE,
                              MISSING   = sapply(strata$GRID_CODE, is.na),
                              VALID     = sapply(strata$GRID_CODE, grid_validator(form)),
                              SIZE      = sapply(strata$GRID_CODE, grid_size))
  grid_status[, WRONG_GRID_TYPE := SIZE == "OTHER"]

  wrong_grid_types = which(grid_status$WRONG_GRID_TYPE == TRUE)

  data_validation_results$strata$checks$main$grids$wrong = list(
    number       = length(wrong_grid_types),
    row_indexes  = wrong_grid_types,
    codes        = strata$GRID_CODE[wrong_grid_types],
    codes_unique = unique(strata$GRID_CODE[wrong_grid_types])
  )

  l_info(paste0("IOTCForm4SFMultiple.validate_data (IV): ", Sys.time() - start))
  start = Sys.time()

  non_empty_strata = which(strata$IS_EMPTY == FALSE) #strata[ !1:.N %in% strata_empty_rows ]
  duplicate_strata = which(strata$OCCURRENCES > 1)   #which(strata_duplicated$COUNT > 1)
  duplicate_strata = duplicate_strata[ ! duplicate_strata %in% strata_empty_rows ]
  unique_strata    = non_empty_strata[ ! non_empty_strata %in% duplicate_strata ]

  data_validation_results$strata$duplicate =
    list(
      number = length(duplicate_strata),
      row_indexes = duplicate_strata
    )

  data_validation_results$strata$unique =
    list(
      number = length(unique_strata),
      row_indexes = unique_strata
    )

  l_info(paste0("IOTCForm4SFMultiple.validate_data (V): ", Sys.time() - start))
  start = Sys.time()

  ### Species code

  missing_species  = which( sapply(strata$SPECIES_CODE, is.na))
  invalid_species  = which(!sapply(strata$SPECIES_CODE, is_species_valid))
  invalid_species  = invalid_species[ ! invalid_species %in% missing_species ]
  missing_species  = missing_species[ ! missing_species %in% strata_empty_rows ]

  species_aggregates     = which( sapply(strata$SPECIES_CODE, is_species_aggregate))
  species_aggregates     = species_aggregates[ ! species_aggregates %in% missing_species ]

  l_info(paste0("IOTCForm4SFMultiple.validate_data (VI): ", Sys.time() - start))
  start = Sys.time()

  ### Sex code

  missing_sex = which( sapply(strata$SEX_CODE, is.na))
  invalid_sex = which(!sapply(strata$SEX_CODE, is_sex_valid))
  invalid_sex = invalid_sex[ ! invalid_sex %in% missing_sex ]
  missing_sex = missing_sex[ ! missing_sex %in% strata_empty_rows ]

  l_info(paste0("IOTCForm4SFMultiple.validate_data (VII): ", Sys.time() - start))
  start = Sys.time()

  ### Type of fate / fate code

  missing_fate_type = which( sapply(strata$FATE_TYPE_CODE, is.na))
  invalid_fate_type = which(!sapply(strata$FATE_TYPE_CODE, is_fate_type_valid))
  invalid_fate_type = invalid_fate_type[ ! invalid_fate_type %in% missing_fate_type ]
  missing_fate_type = missing_fate_type[ ! missing_fate_type %in% strata_empty_rows ]

  missing_fate = which( sapply(strata$FATE_CODE, is.na))
  invalid_fate = which(!mapply(is_fate_valid, strata$FATE_TYPE_CODE, strata$FATE_CODE))
  invalid_fate = invalid_fate[ ! invalid_fate %in% missing_fate ]
  missing_fate = missing_fate[ ! missing_fate %in% strata_empty_rows ]

  l_info(paste0("IOTCForm4SFMultiple.validate_data (VIII): ", Sys.time() - start))
  start = Sys.time()

  ### Measurement type / measure / tool code

  missing_measurement_type = which( sapply(strata$MEASUREMENT_TYPE_CODE, is.na))
  invalid_measurement_type = which(!sapply(strata$MEASUREMENT_TYPE_CODE, is_measurement_type_valid))
  invalid_measurement_type = invalid_measurement_type[ ! invalid_measurement_type %in% missing_measurement_type ]
  missing_measurement_type = missing_measurement_type[ ! missing_measurement_type %in% strata_empty_rows ]

  missing_measure = which( sapply(strata$MEASURE_CODE, is.na))
  invalid_measure = which(!mapply(is_measurement_valid, strata$MEASUREMENT_TYPE_CODE, strata$MEASURE_CODE))
  invalid_measure = invalid_measure[ ! invalid_measure %in% missing_measure ]
  missing_measure = missing_measure[ ! missing_measure %in% strata_empty_rows ]

  missing_measuring_tool = which( sapply(strata$MEASURING_TOOL_CODE, is.na))
  invalid_measuring_tool = which(!mapply(is_measuring_tool_valid, strata$MEASUREMENT_TYPE_CODE, strata$MEASURING_TOOL_CODE))
  invalid_measuring_tool = invalid_measuring_tool[ ! invalid_measuring_tool %in% missing_measuring_tool ]
  missing_measuring_tool = missing_measuring_tool[ ! missing_measuring_tool %in% strata_empty_rows ]

  l_info(paste0("IOTCForm4SFMultiple.validate_data (IX): ", Sys.time() - start))
  start = Sys.time()

  ### Class low / high

  missing_size_class_low = which( sapply(strata$SIZE_CLASS_LOW, is.na))
  invalid_size_class_low = which(!sapply(strata$SIZE_CLASS_LOW, is_value_positive))
  invalid_size_class_low = invalid_size_class_low[ ! invalid_size_class_low %in% missing_size_class_low ]
  missing_size_class_low = missing_size_class_low[ ! missing_size_class_low %in% strata_empty_rows ]

  missing_size_class_high = which( sapply(strata$SIZE_CLASS_HIGH, is.na))
  invalid_size_class_high = which(!sapply(strata$SIZE_CLASS_HIGH, is_value_positive))
  invalid_size_class_high = invalid_size_class_low[ ! invalid_size_class_high %in% missing_size_class_high ]
  missing_size_class_high = missing_size_class_low[ ! missing_size_class_high %in% strata_empty_rows ]

  l_info(paste0("IOTCForm4SFMultiple.validate_data (X): ", Sys.time() - start))
  start = Sys.time()

  check_size_classes = function(low, high) { return(low < high) }

  invalid_size_classes = which(!mapply(check_size_classes, strata$SIZE_CLASS_LOW, strata$SIZE_CLASS_HIGH))
  invalid_size_classes = invalid_size_classes[ ! invalid_size_classes %in% missing_size_class_low &
                                               ! invalid_size_classes %in% missing_size_class_high ]

  l_info(paste0("IOTCForm4SFMultiple.validate_data (XI): ", Sys.time() - start))
  start = Sys.time()

  ### TODO: add a check that the size bin is not >> than what expected for the class

  data_validation_results$strata$checks$main$species = list(
    invalid = list(
      number       = length(invalid_species),
      row_indexes  = invalid_species,
      codes        = strata$SPECIES_CODE[invalid_species],
      codes_unique = unique(strata$SPECIES_CODE[invalid_species])
    ),
    missing = list(
      number      = length(missing_species),
      row_indexes = missing_species
    ),
    aggregates = list(
      number       = length(species_aggregates),
      row_indexes  = species_aggregates,
      codes        = strata[species_aggregates]$SPECIES_CODE,
      codes_unique = unique(strata[species_aggregates]$SPECIES_CODE)
    )
  )

  data_validation_results$strata$checks$main$sex = list(
    invalid = list(
      number       = length(invalid_sex),
      row_indexes  = invalid_sex,
      codes        = strata$SEX_CODE[invalid_sex],
      codes_unique = unique(strata$SEX_CODE[invalid_sex])
    ),
    missing = list(
      number      = length(missing_sex),
      row_indexes = missing_sex
    )
  )

  data_validation_results$strata$checks$main$fate = list(
    type = list(
      invalid = list(
        number       = length(invalid_fate_type),
        row_indexes  = invalid_fate_type,
        codes        = strata$FATE_TYPE_CODE[invalid_fate_type],
        codes_unique = unique(strata$FATE_TYPE_CODE[invalid_fate_type])
      ),
      missing = list(
        number      = length(missing_fate_type),
        row_indexes = missing_fate_type
      )
    ),
    fate = list(
      invalid = list(
        number       = length(invalid_fate),
        row_indexes  = invalid_fate,
        codes        = strata$FATE_CODE[invalid_fate],
        codes_unique = unique(strata$FATE_CODEv[invalid_fate])
      ),
      missing = list(
        number      = length(missing_fate),
        row_indexes = missing_fate
      )
    )
  )

  data_validation_results$strata$checks$measurement = list(
    type = list(
      invalid = list(
        number       = length(invalid_measurement_type),
        row_indexes  = invalid_measurement_type,
        codes        = strata$MEASUREMENT_TYPE_CODE[invalid_measurement_type],
        codes_unique = unique(strata$MEASUREMENT_TYPE_CODE[invalid_measurement_type])
      ),
      missing = list(
        number      = length(missing_measurement_type),
        row_indexes = missing_measurement_type
      )
    ),
    measure = list(
      invalid = list(
        number       = length(invalid_measure),
        row_indexes  = invalid_measure,
        codes        = strata$MEASURE_CODE[invalid_measure],
        codes_unique = unique(strata$MEASURE_CODE[invalid_measure])
      ),
      missing = list(
        number      = length(missing_measure),
        row_indexes = missing_measure
      )
    ),
    measuring_tool = list(
      invalid = list(
        number       = length(invalid_measuring_tool),
        row_indexes  = invalid_measuring_tool,
        codes        = strata$MEASURING_TOOL_CODE[invalid_measuring_tool],
        codes_unique = unique(strata$MEASURING_TOOL_CODE[invalid_measuring_tool])
      ),
      missing = list(
        number      = length(missing_measuring_tool),
        row_indexes = missing_measuring_tool
      )
    )
  )

  data_validation_results$strata$checks$size_classes = list(
    low = list(
      missing = list(
        number       = length(missing_size_class_low),
        row_indexes  = missing_size_class_low
      ),
      invalid = list(
        number       = length(invalid_size_class_low),
        row_indexes  = invalid_size_class_low
      )
    ),
    high = list(
      missing = list(
        number       = length(missing_size_class_high),
        row_indexes  = missing_size_class_high
      ),
      invalid = list(
        number       = length(invalid_size_class_high),
        row_indexes  = invalid_size_class_high
      )
    ),
    invalid = list(
      number      = length(invalid_size_classes),
      row_indexes = invalid_size_classes
    )
  )

  start = Sys.time()

  records = form@data$records

  num_samples_original = records$num_samples_original
  num_samples          = records$num_samples
  num_fish_original    = records$num_fish_original
  num_fish             = records$num_fish

  numeric_num_samples     = num_samples_original[, lapply(.SD, function(value) { lapply(value, function(v) { is.na(v) | is_numeric(v) }) })]
  non_numeric_num_samples = sum(numeric_num_samples == FALSE, na.rm = TRUE)

  na_samples       = sum(numeric_num_samples == TRUE & is.na(num_samples), na.rm = TRUE)
  zero_samples     = sum(numeric_num_samples == TRUE & num_samples == 0,   na.rm = TRUE)
  negative_samples = sum(numeric_num_samples == TRUE & num_samples  < 0,   na.rm = TRUE)
  positive_samples = sum(numeric_num_samples == TRUE & num_samples  > 0,   na.rm = TRUE)

  numeric_num_fish     = num_fish_original   [, lapply(.SD, function(value) { lapply(value, function(v) { is.na(v) | is_numeric(v) }) })]
  non_numeric_num_fish = sum(numeric_num_fish == FALSE, na.rm = TRUE)

  na_fish       = sum(numeric_num_fish == TRUE & is.na(num_fish), na.rm = TRUE)
  zero_fish     = sum(numeric_num_fish == TRUE & num_fish == 0,   na.rm = TRUE)
  negative_fish = sum(numeric_num_fish == TRUE & num_fish  < 0,   na.rm = TRUE)
  positive_fish = sum(numeric_num_fish == TRUE & num_fish  > 0,   na.rm = TRUE)

  l_info(paste0("IOTCForm4SFMultiple.validate_data (XII): ", Sys.time() - start))

  data_validation_results$records$checks = list(
    samples = list(
      na       = na_samples,
      zero     = zero_samples,
      positive = positive_samples,
      negative = negative_samples,
      non_num  = non_numeric_num_samples
    ),
    fish = list(
      na       = na_fish,
      zero     = zero_fish,
      positive = positive_fish,
      negative = negative_fish,
      non_num  = non_numeric_num_fish
    )
  )

  l_info(paste0("IOTCForm4SFMultiple.validate_data: ", Sys.time() - start))

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
    validation_messages = add(validation_messages, new("Message", level = "ERROR", source = "Data", text = paste0("Invalid grid code in row(s) #", paste0(checks_strata$main$grids$invalid$row_indexes, collapse = ", "), ". Please refer to ", reference_codes("admin", "IOTCgridsCESF"), " for a list of valid grid codes for this dataset")))
  }

  if(checks_strata$main$grids$wrong$number > 0) {
    validation_messages = add(validation_messages, new("Message", level = "ERROR", source = "Data", text = paste0(checks_strata$main$grids$wrong$number, " grid codes refer to the wrong type of grid for the fishery: see row(s) #", paste0(strata$checks$main$grids$wrong$row_indexes, collapse = ", "))))
  }

  ## Species

  species = checks_strata$main$species

  if(species$missing$number > 0)
    validation_messages = add(validation_messages, new("Message", level = "ERROR", source = "Data", text = paste0("Missing species code in row(s) #", paste0(species$missing$row_indexes, collapse = ", "))))

  if(species$invalid$number > 0)
    validation_messages = add(validation_messages, new("Message", level = "ERROR", source = "Data", text = paste0("Invalid species code in row(s) #", paste0(species$invalid$row_indexes, collapse = ", "), ". Please refer to ", reference_codes("biological", "allSpecies"), " for a list of valid species codes")))

  if(species$aggregates$number > 0)
    validation_messages = add(validation_messages, new("Message", level = "ERROR", source = "Data", text = paste0("Species code refers to a species aggregate in row(s) #", paste0(species$aggregates$row_indexes, collapse = ", "), ". Please refer to ", reference_codes("biological", "allSpecies"), " for a list of valid distinct species codes")))

  ## Sex

  sex = checks_strata$main$sex

  if(sex$missing$number > 0)
    validation_messages = add(validation_messages, new("Message", level = "ERROR", source = "Data", text = paste0("Missing sex code in row(s) #", paste0(sex$missing$row_indexes, collapse = ", "))))

  if(sex$invalid$number > 0)
    validation_messages = add(validation_messages, new("Message", level = "ERROR", source = "Data", text = paste0("Invalid sex code in row(s) #", paste0(sex$invalid$row_indexes, collapse = ", "), ". Please refer to ", reference_codes("biological", "sex"), " for a list of valid sex codes")))

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

  ## Catches

  samples = checks_records$samples

  if(samples$positive > 0)
    validation_messages = add(validation_messages, new("Message", level = "INFO", source = "Data", text = paste0(samples$positive, " positive number(s) of samples reported")))

  if(samples$na > 0)
    validation_messages = add(validation_messages, new("Message", level = "ERROR", source = "Data", text = paste0(samples$na, " number(s) of samples left empty")))

  if(samples$zero > 0)
    validation_messages = add(validation_messages, new("Message", level = "WARN", source = "Data", text = paste0(samples$zero, " number(s) of samples explicitly reported as zero")))

  if(samples$negative > 0)
    validation_messages = add(validation_messages, new("Message", level = "ERROR", source = "Data", text = paste0(samples$negative, " negative number(s) of samples reported")))

  if(samples$non_num > 0)
    validation_messages = add(validation_messages, new("Message", level = "ERROR", source = "Data", text = paste0(samples$non_num, " non-numeric number(s) of samples reported")))

  fish = checks_records$fish

  if(fish$positive > 0)
    validation_messages = add(validation_messages, new("Message", level = "INFO", source = "Data", text = paste0(fish$positive, " positive number(s) of fish reported")))

  if(fish$na > 0)
    validation_messages = add(validation_messages, new("Message", level = "ERROR", source = "Data", text = paste0(fish$na, " number(s) of fish left empty")))

  if(fish$zero > 0)
    validation_messages = add(validation_messages, new("Message", level = "WARN", source = "Data", text = paste0(fish$zero, " number(s) of fish explicitly reported as zero")))

  if(fish$negative > 0)
    validation_messages = add(validation_messages, new("Message", level = "ERROR", source = "Data", text = paste0(fish$negative, " negative number(s) of fish reported")))

  if(fish$non_num > 0)
    validation_messages = add(validation_messages, new("Message", level = "ERROR", source = "Data", text = paste0(fish$non_num, " non-numeric number(s) of fish reported")))

  l_info(paste0("IOTCForm4SFMultiple.validate_months: ", Sys.time() - start))

  return(validation_messages)
})
