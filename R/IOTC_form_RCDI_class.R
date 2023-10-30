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

setGeneric("get_all_species_references_domain_and_codelist", function(form) {
  standardGeneric("get_all_species_references_domain_and_codelist")
})

setGeneric("get_all_species_references", function(form) {
  standardGeneric("get_all_species_references")
})

setMethod("validate_data",
          list(form = "IOTCFormRCDI", metadata_validation_results = "list"),
          function(form, metadata_validation_results) {
            l_info("IOTCFormRCDI.validate_data")

            strata_orig = form@data$strata
            strata_orig$QUARTER  = strata_orig$QUARTER_ORIGINAL
            strata_orig$COVERAGE = strata_orig$COVERAGE_ORIGINAL
            strata_orig$QUARTER_ORIGINAL  = NULL
            strata_orig$COVERAGE_ORIGINAL = NULL

            strata  = form@data$strata
            records = form@data$records

            catch_data_original = records$data$catches_original
            catch_data          = records$data$catches

            strata_empty_rows    = find_empty_rows(strata_orig)
            strata_empty_columns = find_empty_columns(strata_orig)

            strata[, IS_EMPTY := .I %in% strata_empty_rows]

            total_strata     = nrow(strata)
            non_empty_strata = which(strata$IS_EMPTY == FALSE) #strata[ !1:.N %in% strata_empty_rows ]

            data_empty_rows    = find_empty_rows(catch_data)
            data_empty_columns = find_empty_columns(catch_data)

            missing_quarters   = which( is.na(strata$QUARTER_ORIGINAL))
            invalid_quarters   = which(!is.na(strata$QUARTER_ORIGINAL) & !is_quarter_valid(strata$QUARTER))
            invalid_quarters   = invalid_quarters[ ! invalid_quarters %in% missing_quarters ]
            missing_quarters   = missing_quarters[ ! missing_quarters %in% strata_empty_rows]

            # If all quarters are provided and valid, we check that they're also consistent...

            quarters_check = validate_quarters(form, strata)

            missing_fisheries  = which( is.na(strata$FISHERY_CODE))
            invalid_fisheries  = which(!is_fishery_valid(strata$FISHERY_CODE))
            invalid_fisheries  = invalid_fisheries[ ! invalid_fisheries %in% missing_fisheries ]
            missing_fisheries  = missing_fisheries[ ! missing_fisheries %in% strata_empty_rows]

            fishery_types = fishery_type_for(strata$FISHERY_CODE)
            fishery_aggregates = which(is_multiple_gear_fishery(strata$FISHERY_CODE))

            missing_target_species = which( is.na(strata$TARGET_SPECIES_CODE))
            invalid_target_species = which(!is_species_valid(strata$TARGET_SPECIES_CODE, get_all_species_references(form)))
            invalid_target_species = invalid_target_species[ ! invalid_target_species %in% missing_target_species ]
            missing_target_species = missing_target_species[ ! missing_target_species %in% strata_empty_rows]

            missing_IOTC_areas = which( is.na(strata$IOTC_MAIN_AREA_CODE))
            invalid_IOTC_areas = which(!is_IOTC_main_area_valid(strata$IOTC_MAIN_AREA_CODE))
            invalid_IOTC_areas = invalid_IOTC_areas[ ! invalid_IOTC_areas %in% missing_IOTC_areas ]
            missing_IOTC_areas = missing_IOTC_areas[ ! missing_IOTC_areas %in% strata_empty_rows ]

            valid_IOTC_areas   = strata$IOTC_MAIN_AREA_CODE
            valid_IOTC_areas   = which(!is.na(strata$IOTC_MAIN_AREA_CODE))

            missing_types_of_data    = which( is.na(strata$DATA_TYPE_CODE))
            invalid_types_of_data    = which(!is_data_type_valid(strata$DATA_TYPE_CODE))
            invalid_types_of_data    = invalid_types_of_data[ ! invalid_types_of_data %in% missing_types_of_data ]
            missing_types_of_data    = missing_types_of_data[ ! missing_types_of_data %in% strata_empty_rows ]

            missing_data_sources     = which( is.na(strata$DATA_SOURCE_CODE))
            invalid_data_sources     = which(!is.na(strata$DATA_SOURCE_CODE) & !is_data_source_valid(form_dataset_code(form), strata$DATA_SOURCE_CODE))
            invalid_data_sources     = invalid_data_sources[ ! invalid_data_sources %in% missing_data_sources ]
            missing_data_sources     = missing_data_sources[ ! missing_data_sources %in% strata_empty_rows ]

            missing_data_processings = which( is.na(strata$DATA_PROCESSING_CODE))
            invalid_data_processings = which(!is.na(strata$DATA_PROCESSING_CODE) & !is_data_processing_valid(form_dataset_code(form), strata$DATA_PROCESSING_CODE))
            invalid_data_processings = invalid_data_processings[ ! invalid_data_processings %in% missing_data_processings ]
            missing_data_processings = missing_data_processings[ ! missing_data_processings %in% strata_empty_rows ]

            missing_coverage_types   = which( is.na(strata$COVERAGE_TYPE_CODE))
            invalid_coverage_types   = which(!is_data_coverage_type_valid(strata$COVERAGE_TYPE_CODE))
            invalid_coverage_types   = invalid_coverage_types[ ! invalid_coverage_types %in% missing_coverage_types ]
            missing_coverage_types   = missing_coverage_types[ ! missing_coverage_types %in% strata_empty_rows ]

            missing_coverages        = which( is.na(strata$COVERAGE))
            invalid_coverages        = which(!is_percentage_valid(strata$COVERAGE))
            invalid_coverages        = invalid_coverages[ ! invalid_coverages %in% missing_coverages ]
            missing_coverages        = missing_coverages[ ! missing_coverages %in% strata_empty_rows ]

            missing_species    = which( is.na(records$codes$species))
            invalid_species    = which(!is_species_valid(records$codes$species, get_all_species_references(form)))
            invalid_species    = invalid_species[ ! invalid_species %in% missing_species ]

            species_aggregates = which(is_species_aggregate(records$codes$species, get_all_species_references(form)))

            numeric_catch_data =
              catch_data_original[, lapply(.SD, function(value) { lapply(value, function(v) { is.na(v) | is_numeric(v) }) })]

            non_num_catches  = which(numeric_catch_data == FALSE, arr.ind = TRUE) #sum(numeric_catch_data == FALSE, na.rm = TRUE)

            na_catches       = which(numeric_catch_data == TRUE & is.na(catch_data), arr.ind = TRUE) #sum(numeric_catch_data == TRUE & is.na(catch_data), na.rm = TRUE)
            zero_catches     = which(numeric_catch_data == TRUE & catch_data == 0,   arr.ind = TRUE) #sum(numeric_catch_data == TRUE & catch_data == 0,   na.rm = TRUE)
            negative_catches = which(numeric_catch_data == TRUE & catch_data  < 0,   arr.ind = TRUE) #sum(numeric_catch_data == TRUE & catch_data  < 0,   na.rm = TRUE)
            positive_catches = which(numeric_catch_data == TRUE & catch_data  > 0,   arr.ind = TRUE) #sum(numeric_catch_data == TRUE & catch_data  > 0,   na.rm = TRUE)

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
                      quarters = list(
                        missing = list(
                          number      = length(missing_quarters),
                          row_indexes = spreadsheet_rows_for(form, missing_quarters)
                        ),
                        invalid = list(
                          number        = length(invalid_quarters),
                          row_indexes   = spreadsheet_rows_for(form, invalid_quarters),
                          values        = strata$QUARTER[invalid_quarters],
                          values_unique = unique(strata$QUARTER[invalid_quarters])
                        ),
                        overlapping = list(
                          number = length(quarters_check$overlapping_quarters),
                          row_indexes = spreadsheet_rows_for(form, quarters_check$overlapping_quarters)
                        ),
                        incomplete = list(
                          number = length(quarters_check$incomplete_quarters),
                          row_indexes = spreadsheet_rows_for(form, quarters_check$incomplete_quarters)
                        )
                      ),
                      fisheries = list(
                        invalid = list(
                          number       = length(invalid_fisheries),
                          row_indexes  = spreadsheet_rows_for(form, invalid_fisheries),
                          codes        = strata$FISHERY_CODE[invalid_fisheries],
                          codes_unique = unique(strata$FISHERY_CODE[invalid_fisheries])
                        ),
                        missing = list(
                          number      = length(missing_fisheries),
                          row_indexes = spreadsheet_rows_for(form, missing_fisheries)
                        ),
                        aggregates = list(
                          number       = length(fishery_aggregates),
                          row_indexes  = spreadsheet_rows_for(form, fishery_aggregates),
                          codes        = strata[fishery_aggregates]$FISHERY_CODE,
                          codes_unique = unique(strata[fishery_aggregates]$FISHERY_CODE)
                        ),
                        types = list(
                          artisanal = list(
                            number      = length(which(fishery_types == "AR")),
                            row_indexes = spreadsheet_rows_for(form, which(fishery_types == "AR"))
                          ),
                          semi_industrial = list(
                            number      = length(which(fishery_types == "SI")),
                            row_indexes = spreadsheet_rows_for(form, which(fishery_types == "SI"))
                          ),
                          industrial = list(
                            number      = length(which(fishery_types == "IN")),
                            row_indexes = spreadsheet_rows_for(form, which(fishery_types == "IN"))
                          )
                        )
                      ),
                      target_species = list(
                        invalid = list(
                          number       = length(invalid_target_species),
                          row_indexes  = spreadsheet_rows_for(form, invalid_target_species),
                          codes        = strata$TARGET_SPECIES_CODE[invalid_species],
                          codes_unique = unique(strata$TARGET_SPECIES_CODE[invalid_species])
                        ),
                        missing = list(
                          number      = length(missing_target_species),
                          row_indexes = spreadsheet_rows_for(form, missing_target_species)
                        )
                      ),
                      IOTC_main_areas = list(
                        invalid = list(
                          number       = length(invalid_IOTC_areas),
                          row_indexes  = spreadsheet_rows_for(form, invalid_IOTC_areas),
                          codes        = strata[invalid_IOTC_areas]$IOTC_MAIN_AREA_CODE,
                          codes_unique = unique(strata[invalid_IOTC_areas]$IOTC_MAIN_AREA_CODE)
                        ),
                        missing = list(
                          number      = length(missing_IOTC_areas),
                          row_indexes = spreadsheet_rows_for(form, missing_IOTC_areas)
                        )
                      )
                    ),
                    original_data = list( # This is still part of the strata (in forms 1-RC and 1-DI)
                      type = list(
                        invalid = list(
                          number       = length(invalid_types_of_data),
                          row_indexes  = spreadsheet_rows_for(form, invalid_types_of_data),
                          codes        = strata[invalid_types_of_data]$DATA_TYPE_CODE,
                          codes_unique = unique(strata[invalid_types_of_data]$DATA_TYPE_CODE)
                        ),
                        missing = list(
                          number      = length(missing_types_of_data),
                          row_indexes = spreadsheet_rows_for(form, missing_types_of_data)
                        )
                      ),
                      source = list(
                        invalid = list(
                          number       = length(invalid_data_sources),
                          row_indexes  = spreadsheet_rows_for(form, invalid_data_sources),
                          codes        = strata[invalid_data_sources]$DATA_SOURCE_CODE,
                          codes_unique = unique(strata[invalid_data_sources]$DATA_SOURCE_CODE)
                        ),
                        missing = list(
                          number      = length(missing_data_sources),
                          row_indexes = spreadsheet_rows_for(form, missing_data_sources)
                        )
                      ),
                      processing = list(
                        invalid = list(
                          number       = length(invalid_data_processings),
                          row_indexes  = spreadsheet_rows_for(form, invalid_data_processings),
                          codes        = strata[invalid_data_processings]$DATA_PROCESSING_CODE,
                          codes_unique = unique(strata[invalid_data_processings]$DATA_PROCESSING_CODE)
                        ),
                        missing = list(
                          number      = length(missing_data_processings),
                          row_indexes = spreadsheet_rows_for(form, missing_data_processings)
                        )
                      )
                    ),
                    coverage = list(
                      type = list(
                        invalid = list(
                          number       = length(invalid_coverage_types),
                          row_indexes  = spreadsheet_rows_for(form, invalid_coverage_types),
                          codes        = strata[invalid_coverage_types]$COVERAGE_TYPE_CODE,
                          codes_unique = unique(strata[invalid_coverage_types]$COVERAGE_TYPE_CODE)
                        ),
                        missing = list(
                          number      = length(missing_coverage_types),
                          row_indexes = spreadsheet_rows_for(form, missing_coverage_types)
                        )
                      ),
                      value = list(
                        invalid = list(
                          number      = length(invalid_coverages),
                          row_indexes = spreadsheet_rows_for(form, invalid_coverages),
                          values      = strata[invalid_coverages]$COVERAGE
                        ),
                        missing = list(
                          number      = length(missing_coverages),
                          row_indexes = spreadsheet_rows_for(form, missing_coverages)
                        )
                      )
                    )
                  )
                ),
                records = list(
                  total = nrow(catch_data),
                  empty_rows = list(
                    number      = length(data_empty_rows),
                    row_indexes = spreadsheet_rows_for(form, data_empty_rows)
                  ),
                  empty_columns = list(
                    number      = length(data_empty_columns),
                    col_indexes = spreadsheet_cols_for(form, data_empty_columns)
                  ),
                  checks = list(
                    species = list(
                      missing = list(
                        number      = length(missing_species),
                        col_indexes = spreadsheet_cols_for(form, missing_species)
                      ),
                      invalid = list(
                        number       = length(invalid_species),
                        col_indexes  = spreadsheet_cols_for(form, invalid_species),
                        codes        = records$codes$species[invalid_species],
                        codes_unique = unique(records$codes$species[invalid_species])
                      ),
                      aggregates = list(
                        number      = length(species_aggregates),
                        col_indexes = spreadsheet_cols_for(form, species_aggregates),
                        codes       = records$codes$species[species_aggregates]
                      )
                    ),
                    catch_values = list(
                      na = list(
                        number = nrow(na_catches),
                        cells  = coordinates_to_cells(form, na_catches)
                      ),
                      zero = list(
                        number = nrow(zero_catches),
                        cells  = coordinates_to_cells(form, zero_catches)
                      ),
                      positive = list(
                        number = nrow(positive_catches),
                        cells  = coordinates_to_cells(form, positive_catches)
                      ),
                      negative = list(
                        number = nrow(negative_catches),
                        cells  = coordinates_to_cells(form, negative_catches)
                      ),
                      non_num  = list(
                        number = nrow(non_num_catches),
                        cells  = coordinates_to_cells(form, non_num_catches)
                      )
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

            validation_messages = report_strata(validation_messages, strata)

            # Strata checks

            ## Main strata

            checks_strata_main = checks_strata$main

            # Quarters (1 per row *and* part of the stratum)

            quarters = checks_strata_main$quarters

            if(quarters$incomplete$number > 0)
              validation_messages = add(validation_messages, new("Message", level = "WARN", source = "Data", text = paste0("Data is not provided for all quarters within the strata in row(s) #", paste0(quarters$incomplete$row_indexes, collapse = ", "))))

            if(quarters$missing$number > 0) {
              if(quarters$missing$number > 1) validation_messages = add(validation_messages, new("Message", level = "ERROR", source = "Data", column = "B", text = paste0(quarters$missing$number, " missing quarters")))

              for(row in quarters$missing$row_indexes)
                validation_messages = add(validation_messages, new("Message", level = "ERROR", source = "Data", row = row, column = "B", text = paste0("Missing quarter in row #", row)))
            }

            if(quarters$invalid$number > 0) {
              if(quarters$invalid$number > 1) validation_messages = add(validation_messages, new("Message", level = "ERROR", source = "Data", text = paste0(quarters$invalid$number," invalid quarter values provided. Please use only 1-4 for Q1-Q4 or 0 for 'entire year'")))

              for(row in quarters$invalid$row_indexes)
                validation_messages = add(validation_messages, new("Message", level = "ERROR", source = "Data", row = row, column = "B", text = paste0("Invalid quarter in row #", row)))
            }

            if(quarters$overlapping$number > 0)
              validation_messages = add(validation_messages, new("Message", level = "ERROR", source = "Data", column = "B", text = paste0("Data is provided for overlapping quarters within the strata in row(s) #", paste0(quarters$overlapping$row_indexes, collapse = ", "))))

            # FISHERIES (1 per row *and* part of the stratum)

            validation_messages = report_fisheries(validation_messages, checks_strata_main$fisheries, "C")

            # TARGET SPECIES (1 per row *and* part of the stratum)

            validation_messages = report_target_species(validation_messages, checks_strata_main$target_species, "D")

            # MAIN AREAS (1 per row *and* part of the stratum)

            validation_messages = report_IOTC_area(validation_messages, checks_strata_main$IOTC_main_areas, "E")

            ## Original data

            checks_strata_original_data = checks_strata$original_data

            # DATA TYPES (1 per row, although not part of the stratum)

            validation_messages = report_data_type(validation_messages, checks_strata_original_data$type, "G")

            # DATA SOURCES (1 per row *and* part of the stratum)

            validation_messages = report_data_source(validation_messages, checks_strata_original_data$source, "H")

            # DATA PROCESSINGS (1 per row *and* part of the stratum)

            validation_messages = report_data_processing(validation_messages, checks_strata_original_data$processing, "I")

            # COVERAGE TYPES (1 per row, although not part of the stratum)

            validation_messages = report_coverage_type(validation_messages, checks_strata$coverage$type, "J")

            # COVERAGE VALUES (1 per row, although not part of the stratum)

            validation_messages = report_coverage_value(validation_messages, checks_strata$coverage$type, "J")

            ###

            # Data issues / summary

            ## Empty rows / columns

            validation_messages = report_data(validation_messages, records, FALSE)

            ## Species

            validation_messages = report_species(validation_messages, checks_records$species, spreadsheet_rows_for(form, 2),
                                                 species_domain   = get_all_species_references_domain_and_codelist(form)$domain,
                                                 species_codelist = get_all_species_references_domain_and_codelist(form)$codelist)

            ## Catches

            validation_messages = report_catches(validation_messages, checks_records$catch_values)

            return(validation_messages)
          }
)
