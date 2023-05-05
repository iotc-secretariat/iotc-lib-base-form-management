#' @include IOTC_form_class.R
#' @export IOTCFormCESF
IOTCFormCESF = setClass(
  "IOTCFormCESF",
  contains = "IOTCForm"
)

setMethod("form_comment_cell_row", "IOTCFormCESF", function(form) {
  return(34) #Default for CE / SF
})

setMethod("extract_metadata", list(form = "IOTCFormCESF", common_metadata = "list"), function(form, common_metadata) {
  l_info("IOTCFormCESF.extract_metadata")

  metadata_sheet = form@metadata

  common_metadata$general_information$fishery        = trim(as.character(metadata_sheet[16, 5]))
  common_metadata$general_information$target_species = trim(as.character(metadata_sheet[17, 5]))

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

  fishery_available        = is_provided(general_information$fishery)
  fishery_valid            = fishery_available & is_fishery_valid(general_information$fishery)
  fishery_multiple         = fishery_valid & is_multiple_gear_fishery(general_information$fishery)
  fishery_valid            = fishery_valid & !fishery_multiple

  common_metadata_validation_results$general_information$fishery =
    list(
      available = fishery_available,
      code      = general_information$fishery,
      multiple  = fishery_multiple,
      valid     = fishey_valid
    )

  target_species_available = is_provided(general_information$target_species)
  target_species_valid     = target_species_available & is_species_valid(general_information$target_species)
  target_species_multiple  = target_species_valid & is_species_aggregate(general_information$target_species)
  target_species_valid     = target_species_valid & !target_species_multiple

  common_metadata_validation_results$general_information$target_species =
    list(
      available = target_species_available,
      code      = general_information$target_species,
      multiple  = target_species_multiple,
      valid     = target_species_valid
    )

  common_metadata_validation_results$data_specifications = list()

  data_type_available = is_provided(data_specifications$type_of_data)
  data_type_valid     = data_type_available & is_data_type_valid(data_specifications$type_of_data)

  common_metadata_validation_results$data_specifications$type_of_data =
    list(
      available = data_type_available,
      code      = data_specifications$type_of_data,
      valid     = data_type_valid
    )

  data_source_available = is_provided(data_specifications$data_source)
  data_source_valid     = data_source_available & is_data_source_valid(form_dataset_code(form), data_specifications$data_source)

  common_metadata_validation_results$data_specifications$source =
    list(
      available = data_source_available,
      dataset   = form_dataset_code(form),
      code      = data_specifications$data_source,
      valid     = data_source_valid
    )

  data_processing_available = is_provided(data_specifications$data_processing)
  data_processing_valid     = data_processing_available & is_data_processing_valid(form_dataset_code(form), data_specifications$data_processing)

  common_metadata_validation_results$data_specifications$processing =
    list(
      available = data_processing_available,
      dataset   = form_dataset_code(form),
      code      = data_specifications$data_processing,
      valid     = data_processing_valid
    )

  data_raising_available = is_provided(data_specifications$data_raising)
  data_raising_valid     = data_raising_available & is_data_raising_valid(data_specifications$data_raising)

  common_metadata_validation_results$data_specifications$raising =
    list(
      available = data_raising_available,
      code      = data_specifications$data_raising,
      valid     = data_raising_valid
    )

  common_metadata_validation_results$data_specifications$coverage = list()

  coverage_type_available = is_provided(data_specifications$coverage_type)
  coverage_type_valid     = coverage_type_available & is_data_coverage_type_valid(data_specifications$coverage_type)

  coverage_available      = is_provided(data_specifications$coverage_value)
  coverage_valid          = coverage_available & coverage > 0 & coverage <= 100

  common_metadata_validation_results$data_specifications$coverage$type =
    list(
      available = coverage_type_available,
      code      = data_specifications$coverage_type,
      valid     = coverage_type_valid
    )

  common_metadata_validation_results$data_specifications$coverage$value =
    list(
      value = data_specifications$coverage_value,
      valid = coverage_valid
    )

  return(common_metadata_validation_results)
})

setMethod("metadata_validation_summary", list(form = "IOTCFormCESF", metadata_validation_results = "list"), function(form, metadata_validation_results) {
  l_info("IOTCFormCESF.metadata_validation_summary")

  validation_messages = metadata_validation_results #new("MessageList")

  general_information    = metadata_validation_results$general_information
  data_specifications    = metadata_validation_results$data_specifications

  # General information

  ## Fishery

  if(!general_information$fishery$available)
    validation_messages = add(validation_messages, new("Message", level = "ERROR", source = "Metadata", text = "The fishery is mandatory"))
  else {
    if(general_information$fishery$multiple)
      validation_messages = add(validation_messages, new("Message", level = "ERROR", source = "Metadata", text = paste0("The provided fishery (", general_information$fishery$code, ") is a fishery aggregate")))

    if(!general_information$fishery$valid)
      validation_messages = add(validation_messages, new("Message", level = "ERROR", source = "Metadata", text = paste0("The provided fishery (", general_information$fishery$code, ") is not valid. Please refer to ", reference_codes("fisheries", "fisheries"), " for a list of valid fishery codes")))
  }

  ## Species

  if(!general_information$target_species$available)
    validation_messages = add(validation_messages, new("Message", level = "ERROR", source = "Metadata", text = "The target species is mandatory"))
  else if(!general_information$target_species$valid)
    validation_messages = add(validation_messages, new("Message", level = "ERROR", source = "Metadata", text = paste0("The provided target species (", general_information$target_species$code, ") is not valid. Please refer to ", reference_codes("biological", "allSpecies"), " for a list of valid species codes")))

  # Data specifications

  ## Type of data

  if(!data_specifications$type_of_data$available)
    validation_messages = add(validation_messages, new("Message", level = "ERROR", source = "Metadata", text = "The type of data is mandatory"))
  else if(!data_specifications$type_of_data$valid)
    validation_messages = add(validation_messages, new("Message", level = "ERROR", source = "Metadata", text = paste0("The provided type of data (", data_specifications$type_of_data$code, ") is not valid. Please refer to ", reference_codes("data", "types"), " for a list of valid data type codes")))

  ## Data source

  if(!data_specifications$source$available)
    validation_messages = add(validation_messages, new("Message", level = "ERROR", source = "Metadata", text = "The data source is mandatory"))
  else if(!data_specifications$source$valid)
    validation_messages = add(validation_messages, new("Message", level = "ERROR", source = "Metadata", text = paste0("The provided data source (", data_specifications$source$dataset, " / ", data_specifications$source$code, ") is not valid. Please refer to ", reference_codes("data", "sources"), " for a list of valid data source codes for the ", data_specifications$source$dataset, " dataset")))

  ## Data processing

  if(!data_specifications$processing$available)
    validation_messages = add(validation_messages, new("Message", level = "ERROR", source = "Metadata", text = "The data processing is mandatory"))
  else if(!data_specifications$processing$valid)
    validation_messages = add(validation_messages, new("Message", level = "ERROR", source = "Metadata", text = paste0("The provided data processing (", data_specifications$source$dataset, " / ", data_specifications$processing$code, ") is not valid. Please refer to ", reference_codes("data", "processings"), " for a list of valid data processing codes for the ", data_specifications$processing$dataset, " dataset")))

  ## Raising

  if(!data_specifications$raising$available)
    validation_messages = add(validation_messages, new("Message", level = "ERROR", source = "Metadata", text = "The data raising is mandatory"))
  else if(!data_specifications$raising$valid)
    validation_messages = add(validation_messages, new("Message", level = "ERROR", source = "Metadata", text = paste0("The provided data raising (", data_specifications$raising$code, ") is not valid. Please refer to ", reference_codes("data", "raisings"), " for a list of valid data raising codes")))

  ## Coverage

  if(!data_specifications$coverage$type$available)
    validation_messages = add(validation_messages, new("Message", level = "ERROR", source = "Metadata", text = "The coverage type is mandatory"))
  else if(!data_specifications$coverage$type$valid)
    validation_messages = add(validation_messages, new("Message", level = "ERROR", source = "Metadata", text = paste0("The provided coverage type (", data_specifications$coverage$type$code, ") is not valid. Please refer to ", reference_codes("data", "coverageTypes"), " for a list of valid data coverage type codes")))

  if(!data_specifications$coverage$value$available)
    validation_messages = add(validation_messages, new("Message", level = "ERROR", source = "Metadata", text = "The coverage value is mandatory"))
  else if(!data_specifications$coverage$value$valid)
    validation_messages = add(validation_messages, new("Message", level = "ERROR", source = "Metadata", text = paste0("The provided coverage value (", data_specifications$coverage$value$value, ") is not valid, as it should be numeric, higher than 0 and less than (or equal) to 100%")))

  return(validation_messages)
})

setGeneric("validate_months", function(form, strata) {
  standardGeneric("validate_months")
})

setMethod("validate_data",
          "IOTCFormCESF",
          function(form) {
            l_info("IOTCFormCESF.validate_data")

            strata  = form@data$strata
            records = form@data$records

            ### TO BE MOVED TO SUBCLASSES
            catch_data_original = records$data$catches_original
            catch_data          = records$data$catches

            strata_empty_rows    = find_empty_rows(strata)
            strata_empty_columns = find_empty_columns(strata)

            strata[, IS_EMPTY := .I %in% strata_empty_rows]

            total_strata     = nrow(strata)
            non_empty_strata = which(strata$IS_EMPTY == FALSE) #strata[ !1:.N %in% strata_empty_rows ]

            ### TO BE MOVED TO SUBCLASSES
            data_empty_rows    = find_empty_rows(catch_data)
            data_empty_columns = find_empty_columns(catch_data)

            missing_months   = which( sapply(strata$MONTH, is.na))
            invalid_months   = which(!sapply(strata$MONTH, is_month_valid))
            invalid_months   = invalid_months[ ! invalid_months %in% missing_months ]
            missing_months   = missing_months[ ! missing_months %in% strata_empty_rows]

            # If all quarters are provided and valid, we check that they're also consistent...
            months_check = validate_months(form, strata)

            missing_grids  = which( sapply(strata$GRID_CODE, is.na))
            invalid_grids  = which(!sapply(strata$GRID_CODE, is_grid_CE_SF_valid))
            invalid_grids  = invalid_grids[ ! invalid_grids %in% missing_grids ]
            missing_grids  = missing_grids[ ! missing_grids %in% strata_empty_rows]

            missing_estimations = which( sapply(strata$ESTIMATION_CODE, is.na))
            invalid_estimations = which(!sapply(strata$ESTIMATION_CODE, is_data_estimation_valid))
            invalid_estimations = invalid_estimations[ ! invalid_estimations %in% missing_estimations ]
            missing_estimations = missing_estimations[ ! missing_estimations %in% strata_empty_rows]

            return(
              list(
                strata = list(
                  empty_rows = list(
                    number      = length(strata_empty_rows),
                    row_indexes = strata_empty_rows
                  ),
                  empty_columns = list(
                    number      = length(strata_empty_columns),
                    col_indexes = strata_empty_columns
                  ),
                  total = list(
                    number = total_strata
                  ),
                  non_empty = list(
                    number = length(non_empty_strata),
                    row_indexes = non_empty_strata
                  ),
                  checks = list(
                    main = list(
                      months = list(
                        missing = list(
                          number      = length(missing_months),
                          row_indexes = missing_quarters
                        ),
                        invalid = list(
                          number        = length(missing_months),
                          row_indexes   = missing_months,
                          values        = strata$MONTH[missing_months],
                          values_unique = unique(strata$MONTH[missing_months])
                        ),
                        incomplete = list(
                          number = length(quarters_check$incomplete_months),
                          row_indexes = quarters_check$incomplete_months
                        )
                      ),
                      grids = list(
                        invalid = list(
                          number       = length(invalid_grids),
                          row_indexes  = invalid_grids,
                          codes        = strata[invalid_grids]$GRID_CODE,
                          codes_unique = unique(strata[invalid_grids]$GRID_CODE)
                        ),
                        missing = list(
                          number      = length(missing_grids),
                          row_indexes = missing_grids
                        )
                      ),
                      estimations = list(
                        invalid = list(
                          number       = length(invalid_estimations),
                          row_indexes  = invalid_estimations,
                          codes        = strata[invalid_estimations]$ESTIMATION_CODE,
                          codes_unique = unique(strata[invalid_estimations]$ESTIMATION_CODE)
                        ),
                        missing = list(
                          number      = length(missing_estimations),
                          row_indexes = missing_estimations
                        )
                      )
                    )
                  )
                ),
                records = list(
                  total = nrow(catch_data),
                  empty_rows = list(
                    number      = length(data_empty_rows),
                    row_indexes = data_empty_rows
                  ),
                  empty_columns = list(
                    number      = length(data_empty_columns),
                    col_indexes = data_empty_columns
                  ),
                  checks = list(
                    species = list(
                      missing = list(
                        number      = length(missing_species),
                        col_indexes = missing_species
                      ),
                      invalid = list(
                        number       = length(invalid_species),
                        col_indexes  = invalid_species,
                        codes        = records$codes$species[invalid_species],
                        codes_unique = unique(records$codes$species[invalid_species])
                      ),
                      aggregates = list(
                        number      = length(species_aggregates),
                        col_indexes = species_aggregates,
                        codes       = records$codes$species[species_aggregates]
                      )
                    ),
                    catch_values = list(
                      na       = na_catches,
                      zero     = zero_catches,
                      positive = positive_catches,
                      negative = negative_catches,
                      non_num  = non_num_catches
                    )
                  )
                )
              )
            )
          }
)

setMethod("common_data_validation_summary",
          list(form = "IOTCFormCESF", data_validation_results = "list"),
          function(form, data_validation_results) {
            l_info("IOTCFormCESF.common_data_validation_summary")

            validation_messages = new("MessageList")

            ### STRATA AND RECORDS

            strata  = data_validation_results$strata
            records = data_validation_results$records

            checks_strata  = strata$checks
            checks_records = records$checks

            # Strata issues / summary

            validation_messages = add(validation_messages, new("Message", level = "INFO", source = "Data", text = paste0(strata$total$number, " total strata")))
            validation_messages = add(validation_messages, new("Message", level = "INFO", source = "Data", text = paste0(strata$non_empty$number, " non-empty strata")))
            validation_messages = add(validation_messages, new("Message", level = "INFO", source = "Data", text = paste0(strata$unique$number, " unique strata")))

            if(strata$empty_rows$number > 0)
              validation_messages = add(validation_messages, new("Message", level = "FATAL", source = "Data", text = paste0(strata$empty_rows$number, " empty strata detected: see row(s) #", paste0(strata$empty_rows$row_indexes, collapse = ", "))))

            if(strata$empty_columns$number > 0)
              validation_messages = add(validation_messages, new("Message", level = "FATAL", source = "Data", text = paste0(strata$empty_columns$number, " empty strata columns detected: see column(s) #", paste0(strata$empty_columns$col_indexes, collapse = ", "))))

            if(strata$duplicate$number > 0)
              validation_messages = add(validation_messages, new("Message", level = "FATAL", source = "Data", text = paste0(strata$duplicate$number, " duplicate strata detected: see row(s) #", paste0(strata$duplicate$row_indexes, collapse = ", "))))

            # Strata checks

            ## Main strata

            checks_strata_main = checks_strata$main

            quarters = checks_strata_main$quarters

            if(quarters$incomplete$number > 0)
              validation_messages = add(validation_messages, new("Message", level = "WARN", source = "Data", text = paste0("Data is not provided for all quarters within the strata in row(s) #", paste0(quarters$incomplete$row_indexes, collapse = ", "))))

            if(quarters$missing$number > 0)
              validation_messages = add(validation_messages, new("Message", level = "ERROR", source = "Data", text = paste0("Missing quarter in row(s) #", paste0(quarters$missing$row_indexes, collapse = ", "))))

            if(quarters$invalid$number > 0)
              validation_messages = add(validation_messages, new("Message", level = "ERROR", source = "Data", text = paste0("Invalid quarter value in row(s) #", paste0(quarters$invalid$row_indexes, collapse = ", "), ". Please use only 1-4 for Q1-Q4 or 0 for 'entire year'")))

            if(quarters$overlapping$number > 0)
              validation_messages = add(validation_messages, new("Message", level = "ERROR", source = "Data", text = paste0("Data is provided for overlapping quarters within the strata in row(s) #", paste0(quarters$overlapping$row_indexes, collapse = ", "))))

            fisheries = checks_strata_main$fisheries

            if(fisheries$aggregates$number > 0)
              validation_messages = add(validation_messages, new("Message", level = "WARN", source = "Data", text = paste0("Aggregated fisheries in row(s) #", paste0(fisheries$aggregates$row_indexes, collapse = ", "))))

            if(fisheries$missing$number > 0)
              validation_messages = add(validation_messages, new("Message", level = "ERROR", source = "Data", text = paste0("Missing fishery in row(s) #", paste0(fisheries$missing$row_indexes, collapse = ", "))))

            if(fisheries$invalid$number > 0)
              validation_messages = add(validation_messages, new("Message", level = "ERROR", source = "Data", text = paste0("Invalid fishery in row(s) #", paste0(fisheries$invalid$row_indexes, collapse = ", "), ". Please refer to ", reference_codes("legacy", "fisheries"), " for a list of valid legacy fishery codes")))

            target_species = checks_strata_main$target_species

            if(target_species$missing$number > 0)
              validation_messages = add(validation_messages, new("Message", level = "ERROR", source = "Data", text = paste0("Missing target species in row(s) #", paste0(target_species$missing$row_indexes, collapse = ", "))))

            if(target_species$invalid$number > 0)
              validation_messages = add(validation_messages, new("Message", level = "ERROR", source = "Data", text = paste0("Invalid target species in row(s) #", paste0(target_species$invalid$row_indexes, collapse = ", "), ". Please refer to ", reference_codes("legacy", "species"), " for a list of valid legacy species codes")))

            areas = checks_strata_main$IOTC_main_areas

            if(areas$missing$number > 0)
              validation_messages = add(validation_messages, new("Message", level = "ERROR", source = "Data", text = paste0("Missing IOTC main area in row(s) #", paste0(areas$missing$row_indexes, collapse = ", "))))

            if(areas$invalid$number > 0)
              validation_messages = add(validation_messages, new("Message", level = "ERROR", source = "Data", text = paste0("Invalid IOTC main area in row(s) #", paste0(areas$invalid$row_indexes, collapse = ", "), ". Please refer to ", reference_codes("admin", "IOTCareasMain"), " for a list of valid IOTC main area codes")))

            ## Original data

            checks_strata_original_data = checks_strata$original_data

            types = checks_strata_original_data$type # NOT PART OF THE STRATUM

            if(types$missing$number > 0)
              validation_messages = add(validation_messages, new("Message", level = "ERROR", source = "Data", text = paste0("Missing original data type in row(s) #", paste0(types$missing$row_indexes, collapse = ", "))))

            if(types$invalid$number > 0)
              validation_messages = add(validation_messages, new("Message", level = "ERROR", source = "Data", text = paste0("Invalid original data type in row(s) #", paste0(types$invalid$row_indexes, collapse = ", "), ". Please refer to ", reference_codes("data", "types"), " for a list of valid data type codes")))

            sources = checks_strata_original_data$source

            if(sources$missing$number > 0)
              validation_messages = add(validation_messages, new("Message", level = "ERROR", source = "Data", text = paste0("Missing original data source in row(s) #", paste0(sources$missing$row_indexes, collapse = ", "))))

            if(sources$invalid$number > 0)
              validation_messages = add(validation_messages, new("Message", level = "ERROR", source = "Data", text = paste0("Invalid original data source in row(s) #", paste0(sources$invalid$row_indexes, collapse = ", "), ". Please refer to ", reference_codes("data", "sourcesRC"), " for a list of valid data source codes for this dataset")))

            processings = checks_strata_original_data$processing

            if(processings$missing$number > 0)
              validation_messages = add(validation_messages, new("Message", level = "ERROR", source = "Data", text = paste0("Missing original data processing in row(s) #", paste0(processings$missing$row_indexes, collapse = ", "))))

            if(processings$invalid$number > 0)
              validation_messages = add(validation_messages, new("Message", level = "ERROR", source = "Data", text = paste0("Invalid original data processing in row(s) #", paste0(processings$invalid$row_indexes, collapse = ", "), ". Please refer to ", reference_codes("data", "processingsRC"), " for a list of valid data processing codes for this dataset")))

            checks_strata_coverage = checks_strata$coverage # NOT PART OF THE STRATUM

            coverage_types = checks_strata_coverage$type

            if(coverage_types$missing$number > 0)
              validation_messages = add(validation_messages, new("Message", level = "ERROR", source = "Data", text = paste0("Missing coverage type in row(s) #", paste0(coverage_types$missing$row_indexes, collapse = ", "))))

            if(coverage_types$invalid$number > 0)
              validation_messages = add(validation_messages, new("Message", level = "ERROR", source = "Data", text = paste0("Invalid coverage type in row(s) #", paste0(coverage_types$invalid$row_indexes, collapse = ", "), ". Please refer to ", reference_codes("data", "coverageTypes"), " for a list of valid coverage type codes")))

            coverage_values = checks_strata_coverage$value

            if(coverage_values$missing$number > 0)
              validation_messages = add(validation_messages, new("Message", level = "ERROR", source = "Data", text = paste0("Missing coverage type in row(s) #", paste0(coverage_values$missing$row_indexes, collapse = ", "))))

            if(coverage_values$invalid$number > 0)
              validation_messages = add(validation_messages, new("Message", level = "ERROR", source = "Data", text = paste0("Invalid coverage type in row(s) #", paste0(coverage_values$invalid$row_indexes, collapse = ", "))))

            ###

            # Data issues / summary

            ## Empty rows / columns
            empty_rows = records$empty_rows

            if(empty_rows$number > 0)
              validation_messages = add(validation_messages, new("Message", level = "FATAL", source = "Data", text = paste0(empty_rows$number, " empty data records detected: see row(s) #", paste0(empty_rows$row_indexes, collapse = ", "))))

            empty_columns = records$empty_columns

            if(empty_columns$number > 0)
              validation_messages = add(validation_messages, new("Message", level = "FATAL", source = "Data", text = paste0(empty_columns$number, " empty data columns detected: see column(s) #", paste0(empty_columns$col_indexes, collapse = ", "))))

            ## Species

            species = checks_records$species

            if(species$aggregates$number > 0) # Aggregates
              validation_messages = add(validation_messages, new("Message", level = "WARN", source = "Data", text = paste0("Aggregated species in column(s) #", paste0(species$aggregates$col_indexes, collapse = ", "))))

            if(species$missing$number > 0)    # Missing
              validation_messages = add(validation_messages, new("Message", level = "ERROR", source = "Data", text = paste0("Missing species in column(s) #", paste0(species$missing$col_indexes, collapse = ", "))))

            if(species$invalid$number > 0)    # Invalid
              validation_messages = add(validation_messages, new("Message", level = "ERROR", source = "Data", text = paste0("Invalid species in column(s) #", paste0(species$invalid$col_indexes, collapse = ", "), ". Please refer to ", reference_codes("legacy", "speciess"), " for a list of valid legacy species codes")))

            ## Catches

            catches = checks_records$catch_values

            if(catches$positive > 0)
              validation_messages = add(validation_messages, new("Message", level = "INFO", source = "Data", text = paste0(catches$positive, " positive catch value(s) reported")))

            if(catches$na > 0)
              validation_messages = add(validation_messages, new("Message", level = "INFO", source = "Data", text = paste0(catches$na, " empty catch value(s) reported for all strata / species combinations")))

            if(catches$zero > 0)
              validation_messages = add(validation_messages, new("Message", level = "WARN", source = "Data", text = paste0(catches$zero, " catch value(s) explicitly reported as zero: consider leaving the cells empty instead")))

            if(catches$negative > 0)
              validation_messages = add(validation_messages, new("Message", level = "ERROR", source = "Data", text = paste0(catches$negative, " negative catch value(s) reported")))

            if(catches$non_num > 0)
              validation_messages = add(validation_messages, new("Message", level = "ERROR", source = "Data", text = paste0(catches$non_num, " non-numeric catch value(s) reported")))

            return(validation_messages)
          }
)
