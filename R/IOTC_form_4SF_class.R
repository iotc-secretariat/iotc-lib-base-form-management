#' @include IOTC_form_CESF_class.R
#' @export IOTCForm4SF
IOTCForm4SF = setClass(
  "IOTCForm4SF",
  contains = "IOTCFormCESF"
)

setMethod("form_type", "IOTCForm4SF", function(form) {
  return("4-SF")
})

setMethod("form_version", "IOTCForm4SF", function(form) {
  return("1.0.0-legacy")
})

setMethod("form_dataset_code", "IOTCForm4SF", function(form) {
  return("SF")
})

setMethod("extract_metadata", list(form = "IOTCForm4SF", common_metadata = "list"), function(form, common_metadata) {
  l_info("IOTCForm4SF.extract_metadata")

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

setMethod("validate_metadata", list(form = "IOTCForm4SF", common_metadata_validation_results = "list"), function(form, common_metadata_validation_results) {
  l_info("IOTCForm4SF.validate_metadata")

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

setMethod("metadata_validation_summary", list(form = "IOTCForm4SF", metadata_validation_results = "list"), function(form, metadata_validation_results) {
  l_info("IOTCForm4SF.metadata_validation_summary")

  validation_messages = callNextMethod(form, metadata_validation_results) #new("MessageList")

  general_information    = metadata_validation_results$general_information
  data_specifications    = metadata_validation_results$data_specifications

  measurements           = data_specifications$measurements
  fate                   = data_specifications$fate

  # Data specifications

  ## Species

  if(!general_information$species$available)
    validation_messages = add(validation_messages, new("Message", level = "ERROR", source = "Metadata", text = "The species is mandatory"))
  else if(!general_information$species$valid)
    validation_messages = add(validation_messages, new("Message", level = "ERROR", source = "Metadata", text = paste0("The provided species (", general_information$target_species$code, ") is not valid. Please refer to ", reference_codes("legacy", "species"), " for a list of valid species codes")))
  else if(!general_information$species$multiple)
    validation_messages = add(validation_messages, new("Message", level = "ERROR", source = "Metadata", text = paste0("The provided species (", general_information$target_species$code, ") correspond to a species aggregate. Please refer to ", reference_codes("legacy", "species"), " for a list of valid, distinct species codes")))

  ## Measurements

  ### Type

  if(!measurements$type$available)
    validation_messages = add(validation_messages, new("Message", level = "ERROR", source = "Metadata", text = "The measurement type is mandatory"))
  else if(!measurements$type$valid)
    validation_messages = add(validation_messages, new("Message", level = "ERROR", source = "Metadata", text = paste0("The provided measurement type (", measurements$type$code, ") is not valid. Please refer to ", reference_codes("biological", "typesOfMeasurement"), " for a list of valid measurement type codes")))

  if(measurements$type$valid && measurements$type$code == "LN")
    validation_messages = add(validation_messages, new("Message", level = "INFO", source = "Metadata", text = paste0("The provided measurement type (", measurements$type$code, ") refers to individual lengths: the measurement unit will be assumed to be centimeters (cm)")))

  if(measurements$type$valid && measurements$type$code == "WG")
    validation_messages = add(validation_messages, new("Message", level = "INFO", source = "Metadata", text = paste0("The provided measurement type (", measurements$type$code, ") refers to individual weights: the measurement unit will be assumed to be kilograms (kg)")))

  ### Measure

  if(!measurements$measure$available)
    validation_messages = add(validation_messages, new("Message", level = "ERROR", source = "Metadata", text = "The measure is mandatory"))
  else if(!measurements$measure$valid)
    validation_messages = add(validation_messages, new("Message", level = "ERROR", source = "Metadata", text = paste0("The provided measure (", measurements$type$code, ") is not valid. Please refer to ", reference_codes("biological", "allMeasurementTypes"), " for a list of valid measure codes")))

  ### Measuring tool

  if(!measurements$tool$available)
    validation_messages = add(validation_messages, new("Message", level = "ERROR", source = "Metadata", text = "The measuring tool is mandatory"))
  else if(!measurements$tool$valid)
    validation_messages = add(validation_messages, new("Message", level = "ERROR", source = "Metadata", text = paste0("The provided measuring tool (", measurements$tool$code, ") is not valid. Please refer to ", reference_codes("biological", "allMeasurementTools"), " for a list of valid measuring tool codes")))

  ### Size interval

  if(!measurements$interval$available)
    validation_messages = add(validation_messages, new("Message", level = "ERROR", source = "Metadata", text = "The size interval is mandatory"))
  else if(!measurements$interval$valid)
    validation_messages = add(validation_messages, new("Message", level = "ERROR", source = "Metadata", text = paste0("The provided size interval (", measurements$interval$value, ") is not valid. Please ensure to provide a numeric value greater than zero")))

  ## Fate

  ### Type

  if(!fate$type$available)
    validation_messages = add(validation_messages, new("Message", level = "ERROR", source = "Metadata", text = "The type of fate is mandatory"))
  else if(!fate$type$valid)
    validation_messages = add(validation_messages, new("Message", level = "ERROR", source = "Metadata", text = paste0("The provided type of fate (", fate$fate$code, ") is not valid. Please refer to ", reference_codes("biological", "typesOfFate"), " for a list of valid fate type codes")))

  ### Code

  if(!fate$fate$available)
    validation_messages = add(validation_messages, new("Message", level = "ERROR", source = "Metadata", text = "The fate is mandatory"))
  else if(!fate$fate$valid)
    validation_messages = add(validation_messages, new("Message", level = "ERROR", source = "Metadata", text = paste0("The provided fate (", fate$fate$code, ") is not valid. Please refer to ", reference_codes("biological", "fates"), " for a list of valid fate codes")))

  return(validation_messages)
})

setMethod("extract_data", "IOTCForm4SF", function(form) {
  l_info("IOTCForm4SF.extract_data")

  form_metadata = form@original_metadata
  form_data     = form@original_data

  strata = form_data[4:nrow(form_data)][, 2:6]
  colnames(strata) = c("MONTH", "GRID_CODE", "SEX_CODE", "ESTIMATION_CODE", "SIZE_CLASS_LOW")

  strata[, SIZE_CLASS_LOW := round(as.numeric(SIZE_CLASS_LOW), 0)]
  strata[, MONTH    := as.integer(MONTH)]

  records = form_data[5:nrow(form_data), 7:ncol(form_data)]

  colnames(records) = c("NUM_SAMPLES", "NUM_FISH")
  records_original = records

  # Might raise the "Warning in FUN(X[[i]], ...) : NAs introduced by coercion" message when catches include non-numeric values...
  records = records_original[, lapply(.SD, function(value) { return(round(as.numeric(value), 2)) })]

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

setMethod("validate_months",
          list(form = "IOTCForm4SF", strata = "data.table"),
          function(form, strata) {
            l_info("IOTCForm4SF.validate_months")

            valid_months_strata   = strata[MONTH %in% 1:12, .(NUM_MONTHS = .N), keyby = .(GRID_CODE, SEX_CODE)]

            incomplete_months_strata  = valid_months_strata[NUM_MONTHS < 12]

            incomplete_months  = merge(strata, incomplete_months_strata, all.x = TRUE, sort = FALSE, by = c("GRID_CODE", "SEX_CODE"))
            incomplete_months  = which(!is.na(incomplete_months$NUM_MONTHS))

            return(
              list(
                incomplete_months  = incomplete_months
              )
            )
          }
)

setMethod("validate_data",
          list(form = "IOTCForm4SF", metadata_validation_results = "list"),
          function(form, metadata_validation_results) {
            l_info("IOTCForm4SF.validate_data")

            data_validation_results = callNextMethod(form, metadata_validation_results)

            strata  = form@data$strata

            strata_empty_rows    = find_empty_rows(strata)
            strata_empty_columns = find_empty_columns(strata[, c(1:3, 5)])

            strata[, IS_EMPTY := .I %in% strata_empty_rows]
            strata[, OCCURRENCES := .N, by = .(MONTH, GRID_CODE, SEX_CODE, SIZE_CLASS_LOW)]

            # If all months are provided and valid, we check that they're also consistent...
            valid_months_strata   = strata[MONTH %in% 1:12, .(NUM_MONTHS = .N), keyby = .(GRID_CODE, SEX_CODE)]

            incomplete_months_strata  = valid_months_strata[NUM_MONTHS < 12]

            incomplete_months  = merge(strata, incomplete_months_strata, all.x = TRUE, sort = FALSE, by = c("GRID_CODE", "SEX_CODE"))
            incomplete_months  = which(!is.na(incomplete_months$NUM_MONTHS))

            grid_size = function(code) {
              if(is.na(code) || code == "") return("OTHER")
              else if(str_sub(code, 1, 1) == "5") return("1_DEG")
              else if(str_sub(code, 1, 1) == "6") return("5_DEG")
              return("OTHER")
            }

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

            grid_status    = data.table(GRID_CODE = strata$GRID_CODE,
                                        MISSING   = sapply(strata$GRID_CODE, is.na),
                                        VALID     = sapply(strata$GRID_CODE, is_grid_CE_SF_valid),
                                        SIZE      = sapply(strata$GRID_CODE, grid_size))

            wrong_grid_types = which(grid_status$SIZE == "OTHER")
            wrong_grid_types = wrong_grid_types[ which(wrong_grid_types %in% which(grid_status$VALID)) ]

            data_validation_results$strata$checks$main$grids$wrong = list(
              number       = length(wrong_grid_types),
              row_indexes  = wrong_grid_types,
              codes        = strata$GRID_CODE[wrong_grid_types],
              codes_unique = unique(strata$GRID_CODE[wrong_grid_types])
            )

            missing_sex = which(sapply(strata$SEX_CODE, is.na))
            invalid_sex = which(!sapply(strata$SEX_CODE, is_sex_valid))
            invalid_sex = invalid_sex[ ! invalid_sex %in% missing_sex ]
            missing_sex = missing_sex[ ! missing_sex %in% strata_empty_rows ]

            data_validation_results$strata$checks$sex = list(
              missing = list(
                number      = length(missing_sex),
                row_indexes = missing_sex
              ),
              invalid = list(
                number        = length(invalid_sex),
                row_indexes   = invalid_sex,
                values        = strata$SEX_CODE[invalid_sex],
                values_unique = unique(strata$SEX_CODE[invalid_sex])
              )
            )

            is_size_class_valid = function(value) {
              return(
                !is.na(value) && is_numeric(value) && as.numeric(value) > 0
              )
            }

            missing_size_class = which(sapply(strata$SIZE_CLASS_LOW, is.na))
            invalid_size_class = which(!sapply(strata$SIZE_CLASS_LOW, is_size_class_valid))
            invalid_size_class = invalid_size_class[ ! invalid_size_class %in% missing_size_class ]
            missing_size_class = missing_size_class[ ! missing_size_class %in% strata_empty_rows ]

            data_validation_results$strata$checks$size_class = list(
              missing = list(
                number      = length(missing_size_class),
                row_indexes = missing_size_class
              ),
              invalid = list(
                number        = length(invalid_size_class),
                row_indexes   = invalid_size_class,
                values        = strata$SIZE_CLASS_LOW[invalid_size_class],
                values_unique = unique(strata$SIZE_CLASS_LOW[invalid_size_class])
              )
            )

            records = form@data$records

            sizes_original = records$data$CE_SF_data_original
            sizes          = records$data$CE_SF_data

            numeric_sizes =
              sizes_original[, lapply(.SD, function(value) { lapply(value, function(v) { is.na(v) | is_numeric(v) }) })]

            non_num_sizes_samples  = sum(numeric_sizes$NUM_SAMPLES == FALSE, na.rm = TRUE)
            non_num_sizes_fish     = sum(numeric_sizes$NUM_FISH    == FALSE, na.rm = TRUE)

            na_samples       = sum(non_num_sizes_samples == TRUE & is.na(sizes$NUM_SAMPLES), na.rm = TRUE)
            zero_samples     = sum(non_num_sizes_samples == TRUE & sizes$NUM_SAMPLES == 0,   na.rm = TRUE)
            positive_samples = sum(non_num_sizes_samples == TRUE & sizes$NUM_SAMPLES  > 0,   na.rm = TRUE)
            negative_samples = sum(non_num_sizes_samples == TRUE & sizes$NUM_SAMPLES  < 0,   na.rm = TRUE)

            na_fish          = sum(non_num_sizes_fish == TRUE    & is.na(sizes$NUM_FISH), na.rm = TRUE)
            zero_fish        = sum(non_num_sizes_fish == TRUE    & sizes$NUM_FISH == 0,   na.rm = TRUE)
            positive_fish    = sum(non_num_sizes_fish == TRUE    & sizes$NUM_FISH  > 0,   na.rm = TRUE)
            negative_fish    = sum(non_num_sizes_fish == TRUE    & sizes$NUM_FISH  < 0,   na.rm = TRUE)

            data_validation_results$records$checks = list(
              num_samples = list(
                na       = na_samples,
                zero     = zero_samples,
                positive = positive_samples,
                negative = negative_samples,
                non_num  = non_num_sizes_samples
              ),
              num_fish = list(
                na       = na_fish,
                zero     = zero_fish,
                positive = positive_fish,
                negative = negative_fish,
                non_num  = non_num_sizes_fish
              )
            )

            return(data_validation_results)
          }
)

setMethod("data_validation_summary",
          list(form = "IOTCForm4SF", metadata_validation_results = "list", data_validation_results = "list"),
          function(form, metadata_validation_results, data_validation_results) {
            l_info("IOTCForm4SF.data_validation_summary")

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

            if(strata$duplicate$number > 0)
              validation_messages = add(validation_messages, new("Message", level = "FATAL", source = "Data", text = paste0(strata$duplicate$number, " duplicate strata detected: see row(s) #", paste0(strata$duplicate$row_indexes, collapse = ", "))))

            if(strata$checks$main$grids$invalid$number > 0) {
              validation_messages = add(validation_messages, new("Message", level = "ERROR", source = "Data", text = paste0("Invalid grid code in row(s) #", paste0(strata$checks$main$grids$invalid$row_indexes, collapse = ", "), ". Please refer to ", reference_codes("admin", "IOTCgridsAR"), " for a list of valid grid codes for this dataset")))
            }

            if(strata$checks$main$grids$wrong$number > 0) {
              validation_messages = add(validation_messages, new("Message", level = "ERROR", source = "Data", text = paste0(strata$checks$main$grids$wrong$number, " grid codes refer to the wrong type of grid for the fishery: see row(s) #", paste0(strata$checks$main$grids$wrong$row_indexes, collapse = ", "))))
            }

            ## Sex

            sex = checks_strata$sex

            if(sex$missing$number > 0)
              validation_messages = add(validation_messages, new("Message", level = "ERROR", source = "Data", text = paste0("Missing sex in row(s) #", paste0(sex$missing$row_indexes, collapse = ", "))))

            if(sex$invalid$number > 0)
              validation_messages = add(validation_messages, new("Message", level = "ERROR", source = "Data", text = paste0("Invalid sex in row(s) #", paste0(sex$invalid$row_indexes, collapse = ", "), ". Please refer to ", reference_codes("biological", "sex"), " for a list of valid measuring sex codes")))

            ## Size class

            size_class = checks_strata$size_class

            if(size_class$missing$number > 0)
              validation_messages = add(validation_messages, new("Message", level = "ERROR", source = "Data", text = paste0("Missing size class in row(s) #", paste0(size_class$missing$row_indexes, collapse = ", "))))

            if(size_class$invalid$number > 0)
              validation_messages = add(validation_messages, new("Message", level = "ERROR", source = "Data", text = paste0("Invalid size class in row(s) #", paste0(size_class$invalid$row_indexes, collapse = ", "), ". Please ensure that size classes are numeric and greater than zero")))

            # Data issues / summary

            ## Number of samples

            num_samples = checks_records$num_samples

            if(num_samples$positive > 0)
              validation_messages = add(validation_messages, new("Message", level = "INFO", source = "Data", text = paste0(num_samples$positive, " positive value(s) reported as number of samples")))

            if(num_samples$zero > 0)
              validation_messages = add(validation_messages, new("Message", level = "INFO", source = "Data", text = paste0(num_samples$zero, " number of samples explicitly reported as zero")))

            if(num_samples$na > 0)
              validation_messages = add(validation_messages, new("Message", level = "ERROR", source = "Data", text = paste0(num_samples$na, " empty value(s) reported as number of samples for all strata / size class combinations")))

            if(num_samples$negative > 0)
              validation_messages = add(validation_messages, new("Message", level = "ERROR", source = "Data", text = paste0(num_samples$negative, " negative value(s) reported as number of samples")))

            if(num_samples$non_num > 0)
              validation_messages = add(validation_messages, new("Message", level = "ERROR", source = "Data", text = paste0(num_samples$non_num, " non-numeric value(s) reported as number of samples")))


            ## Number of fish

            num_fish = checks_records$num_fish

            if(num_fish$positive > 0)
              validation_messages = add(validation_messages, new("Message", level = "INFO", source = "Data", text = paste0(num_fish$positive, " positive value(s) reported as number of fish")))

            if(num_fish$zero > 0)
              validation_messages = add(validation_messages, new("Message", level = "INFO", source = "Data", text = paste0(num_fish$zero, " number of fish explicitly reported as zero")))

            if(num_fish$na > 0)
              validation_messages = add(validation_messages, new("Message", level = "ERROR", source = "Data", text = paste0(num_fish$na, " empty value(s) reported as number of fish for all strata / size class combinations")))

            if(num_fish$negative > 0)
              validation_messages = add(validation_messages, new("Message", level = "ERROR", source = "Data", text = paste0(num_fish$negative, " negative value(s) reported as number of fish")))

            if(num_fish$non_num > 0)
              validation_messages = add(validation_messages, new("Message", level = "ERROR", source = "Data", text = paste0(num_fish$non_num, " non-numeric value(s) reported as number of fish")))

            return(validation_messages)
          }
)
