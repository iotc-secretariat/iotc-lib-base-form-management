#' @include IOTC_form_class.R
#' @export IOTCFormRCDI
IOTCFormRCDI = setClass(
  "IOTCFormRCDI",
  contains = "IOTCForm"
)

setMethod("form_comment_cell_row", "IOTCFormRCDI", function(form) {
  return(23) #Default for RC / DI
})

setMethod("extract_metadata", list(form = "IOTCFormRCDI", common_metadata = "list"), function(form, common_metadata) {
  l_info("IOTCFormRCDI.extract_metadata")

  return(common_metadata)
})

setMethod("validate_metadata", list(form = "IOTCFormRCDI", common_metadata_validation_results = "list"), function(form, common_metadata_validation_results) {
  l_info("IOTCFormRCDI.validate_metadata")

  return(common_metadata_validation_results)
})

setMethod("metadata_validation_summary", list(form = "IOTCFormRCDI", metadata_validation_results = "list"), function(form, metadata_validation_results) {
  l_info("IOTCFormRCDI.metadata_validation_summary")

  return(new("MessageList"))
})

setGeneric("validate_quarters", function(form, strata) {
  standardGeneric("validate_quarters")
})

setMethod("validate_data",
          list(form = "IOTCFormRCDI", metadata_validation_results = "list"),
          function(form, metadata_validation_results) {
            l_info("IOTCFormRCDI.validate_data")

            strata  = form@data$strata
            records = form@data$records

            catch_data_original = records$data$catches_original
            catch_data          = records$data$catches

            strata_empty_rows    = find_empty_rows(strata)
            strata_empty_columns = find_empty_columns(strata)

            strata[, IS_EMPTY := .I %in% strata_empty_rows]

            total_strata     = nrow(strata)
            non_empty_strata = which(strata$IS_EMPTY == FALSE) #strata[ !1:.N %in% strata_empty_rows ]

            data_empty_rows    = find_empty_rows(catch_data)
            data_empty_columns = find_empty_columns(catch_data)

            missing_quarters   = which( sapply(strata$QUARTER, is.na))
            invalid_quarters   = which(!sapply(strata$QUARTER, is_quarter_valid))
            invalid_quarters   = invalid_quarters[ ! invalid_quarters %in% missing_quarters ]
            missing_quarters   = missing_quarters[ ! missing_quarters %in% strata_empty_rows]

            # If all quarters are provided and valid, we check that they're also consistent...

            quarters_check = validate_quarters(form, strata)

            missing_fisheries  = which( sapply(strata$FISHERY_CODE, is.na))
            invalid_fisheries  = which(!sapply(strata$FISHERY_CODE, is_fishery_valid))
            invalid_fisheries  = invalid_fisheries[ ! invalid_fisheries %in% missing_fisheries ]
            missing_fisheries  = missing_fisheries[ ! missing_fisheries %in% strata_empty_rows]

            fishery_aggregates = which(unlist(sapply(strata$FISHERY_CODE, function(value) { return(ifelse(is.na(value), FALSE, is_multiple_gear_fishery(value))) }, USE.NAMES = FALSE)))

            missing_target_species = which( sapply(strata$TARGET_SPECIES_CODE, is.na))
            invalid_target_species = which(!sapply(strata$TARGET_SPECIES_CODE, is_species_valid))
            invalid_target_species = invalid_target_species[ ! invalid_target_species %in% missing_target_species ]
            missing_target_species = missing_target_species[ ! missing_target_species %in% strata_empty_rows]

            missing_IOTC_areas = which( sapply(strata$IOTC_MAIN_AREA_CODE, is.na))
            invalid_IOTC_areas = which(!sapply(strata$IOTC_MAIN_AREA_CODE, is_IOTC_main_area_valid))
            invalid_IOTC_areas = invalid_IOTC_areas[ ! invalid_IOTC_areas %in% missing_IOTC_areas ]
            missing_IOTC_areas = missing_IOTC_areas[ ! missing_IOTC_areas %in% strata_empty_rows ]

            missing_types_of_data    = which( sapply(strata$DATA_TYPE_CODE, is.na))
            invalid_types_of_data    = which(!sapply(strata$DATA_TYPE_CODE, is_data_type_valid))
            invalid_types_of_data    = invalid_types_of_data[ ! invalid_types_of_data %in% missing_types_of_data ]
            missing_types_of_data    = missing_types_of_data[ ! missing_types_of_data %in% strata_empty_rows ]

            missing_data_sources     = which( sapply(strata$DATA_SOURCE_CODE, is.na))
            invalid_data_sources     = which(!sapply(strata$DATA_SOURCE_CODE, function(code) { return(is_data_source_valid(form_dataset_code(form), code)) }))
            invalid_data_sources     = invalid_data_sources[ ! invalid_data_sources %in% missing_data_sources ]
            missing_data_sources     = missing_data_sources[ ! missing_data_sources %in% strata_empty_rows ]

            missing_data_processings = which( sapply(strata$DATA_PROCESSING_CODE, is.na))
            invalid_data_processings = which(!sapply(strata$DATA_PROCESSING_CODE, function(code) { return(is_data_processing_valid(form_dataset_code(form), code)) }))
            invalid_data_processings = invalid_data_processings[ ! invalid_data_processings %in% missing_data_processings ]
            missing_data_processings = missing_data_processings[ ! missing_data_processings %in% strata_empty_rows ]

            missing_coverage_types   = which( sapply(strata$COVERAGE_TYPE_CODE, is.na))
            invalid_coverage_types   = which(!sapply(strata$COVERAGE_TYPE_CODE, is_data_coverage_type_valid))
            invalid_coverage_types   = invalid_coverage_types[ ! invalid_coverage_types %in% missing_coverage_types ]
            missing_coverage_types   = missing_coverage_types[ ! missing_coverage_types %in% strata_empty_rows ]

            missing_coverages        = which( sapply(strata$COVERAGE, is.na))
            invalid_coverages        = which(!sapply(strata$COVERAGE, is_percentage_valid))
            invalid_coverages        = invalid_coverages[ ! invalid_coverages %in% missing_coverages ]
            missing_coverages        = missing_coverages[ ! missing_coverages %in% strata_empty_rows ]

            missing_species    = which( sapply(records$codes$species, is.na))
            invalid_species    = which(!sapply(records$codes$species, is_species_valid))
            invalid_species    = invalid_species[ ! invalid_species %in% missing_species ]

            species_aggregates = which(unlist(sapply(records$codes$species, function(value) { return(ifelse(is.na(value), FALSE, is_species_aggregate(value))) }, USE.NAMES = FALSE)))

            numeric_catch_data =
              catch_data_original[, lapply(.SD, function(value) { lapply(value, function(v) { is.na(v) | is_numeric(v) }) })]

            non_num_catches  = sum(numeric_catch_data == FALSE, na.rm = TRUE)

            na_catches       = sum(numeric_catch_data == TRUE & is.na(catch_data), na.rm = TRUE)
            zero_catches     = sum(numeric_catch_data == TRUE & catch_data == 0,   na.rm = TRUE)
            negative_catches = sum(numeric_catch_data == TRUE & catch_data  < 0,   na.rm = TRUE)
            positive_catches = sum(numeric_catch_data == TRUE & catch_data  > 0,   na.rm = TRUE)

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
                      quarters = list(
                        missing = list(
                          number      = length(missing_quarters),
                          row_indexes = missing_quarters
                        ),
                        invalid = list(
                          number        = length(invalid_quarters),
                          row_indexes   = invalid_quarters,
                          values        = strata$QUARTER[invalid_quarters],
                          values_unique = unique(strata$QUARTER[invalid_quarters])
                        ),
                        overlapping = list(
                          number = length(quarters_check$overlapping_quarters),
                          row_indexes = quarters_check$overlapping_quarters
                        ),
                        incomplete = list(
                          number = length(quarters_check$incomplete_quarters),
                          row_indexes = quarters_check$incomplete_quarters
                        )
                      ),
                      fisheries = list(
                        invalid = list(
                          number       = length(invalid_fisheries),
                          row_indexes  = invalid_fisheries,
                          codes        = strata$FISHERY_CODE[invalid_fisheries],
                          codes_unique = unique(strata$FISHERY_CODE[invalid_fisheries])
                        ),
                        missing = list(
                          number      = length(missing_fisheries),
                          row_indexes = missing_fisheries
                        ),
                        aggregates = list(
                          number       = length(fishery_aggregates),
                          row_indexes  = fishery_aggregates,
                          codes        = strata[fishery_aggregates]$FISHERY_CODE,
                          codes_unique = unique(strata[fishery_aggregates]$FISHERY_CODE)
                        )
                      ),
                      target_species = list(
                        invalid = list(
                          number       = length(invalid_target_species),
                          row_indexes  = invalid_target_species,
                          codes        = strata$TARGET_SPECIES_CODE[invalid_species],
                          codes_unique = unique(strata$TARGET_SPECIES_CODE[invalid_species])
                        ),
                        missing = list(
                          number      = length(missing_target_species),
                          row_indexes = missing_target_species
                        )
                      ),
                      IOTC_main_areas = list(
                        invalid = list(
                          number       = length(invalid_IOTC_areas),
                          row_indexes  = invalid_IOTC_areas,
                          codes        = strata[invalid_IOTC_areas]$IOTC_MAIN_AREA_CODE,
                          codes_unique = unique(strata[invalid_IOTC_areas]$IOTC_MAIN_AREA_CODE)
                        ),
                        missing = list(
                          number      = length(missing_IOTC_areas),
                          row_indexes = missing_IOTC_areas
                        )
                      )
                    ),
                    original_data = list( # This is still part of the strata (in forms 1-RC and 1-DI)
                      type = list(
                        invalid = list(
                          number       = length(invalid_types_of_data),
                          row_indexes  = invalid_types_of_data,
                          codes        = strata[invalid_types_of_data]$DATA_TYPE_CODE,
                          codes_unique = unique(strata[invalid_types_of_data]$DATA_TYPE_CODE)
                        ),
                        missing = list(
                          number      = length(missing_types_of_data),
                          row_indexes = missing_types_of_data
                        )
                      ),
                      source = list(
                        invalid = list(
                          number       = length(invalid_data_sources),
                          row_indexes  = invalid_data_sources,
                          codes        = strata[invalid_data_sources]$DATA_SOURCE_CODE,
                          codes_unique = unique(strata[invalid_data_sources]$DATA_SOURCE_CODE)
                        ),
                        missing = list(
                          number      = length(missing_data_sources),
                          row_indexes = missing_data_sources
                        )
                      ),
                      processing = list(
                        invalid = list(
                          number       = length(invalid_data_processings),
                          row_indexes  = invalid_data_processings,
                          codes        = strata[invalid_data_processings]$DATA_PROCESSING_CODE,
                          codes_unique = unique(strata[invalid_data_processings]$DATA_PROCESSING_CODE)
                        ),
                        missing = list(
                          number      = length(missing_data_processings),
                          row_indexes = missing_data_processings
                        )
                      )
                    ),
                    coverage = list(
                      type = list(
                        invalid = list(
                          number       = length(invalid_coverage_types),
                          row_indexes  = invalid_coverage_types,
                          codes        = strata[invalid_coverage_types]$COVERAGE_TYPE_CODE,
                          codes_unique = unique(strata[invalid_coverage_types]$COVERAGE_TYPE_CODE)
                        ),
                        missing = list(
                          number      = length(missing_coverage_types),
                          row_indexes = missing_coverage_types
                        )
                      ),
                      value = list(
                        invalid = list(
                          number      = length(invalid_coverages),
                          row_indexes = invalid_coverages,
                          values      = strata[invalid_coverages]$COVERAGE
                        ),
                        missing = list(
                          number      = length(missing_coverages),
                          row_indexes = missing_coverages
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
          list(form = "IOTCFormRCDI", metadata_validation_results = "list", data_validation_results = "list"),
          function(form, metadata_validation_results, data_validation_results) {
            l_info("IOTCFormRCDI.common_data_validation_summary")

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
              validation_messages = add(validation_messages, new("Message", level = "ERROR", source = "Data", text = paste0("Missing coverage value in row(s) #", paste0(coverage_values$missing$row_indexes, collapse = ", "))))

            if(coverage_values$invalid$number > 0)
              validation_messages = add(validation_messages, new("Message", level = "ERROR", source = "Data", text = paste0("Invalid coverage value in row(s) #", paste0(coverage_values$invalid$row_indexes, collapse = ", "))))

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
