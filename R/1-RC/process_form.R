verify_form_type_1RC = function(form) {
  form_details = extract_metadata_1RC(form)$form_details

  form_type    = form_details$type
  form_version = form_details$version

  if(check_mandatory(form_type, "Form type") != "1-RC")
    stop(call. = FALSE, paste0("Please provide a valid form 1-RC (current form type: ", form_type, " - required: 1-RC)"))

  if(check_mandatory(form_version, "Form version") != "1.0.0")
    stop(call. = FALSE, paste0("Please provide a valid form 1-RC (current form version: ", form_version, " - required: 1.0.0)"))
}

extract_metadata_1RC = function(form) {
  metadata = common_metadata(form$form_metadata)
  metadata$comment = comments(form$form_metadata, 23)

  return(metadata)
}

extract_strata_and_records_1RC = function(form) {
  form_metadata = form$form_metadata
  form_data     = form$form_data

  strata = form_data[4:nrow(form_data)][, 2:10]
  colnames(strata) = c("QUARTER", "FISHERY_CODE", "IOTC_MAIN_AREA_CODE", "RETAIN_REASON_CODE",
                       "DATA_TYPE_CODE", "DATA_SOURCE_CODE", "DATA_PROCESSING_CODE",
                       "COVERAGE_TYPE_CODE", "COVERAGE")

  strata[, QUARTER    := as.integer(QUARTER)]
  strata[, COVERAGE   := round(as.numeric(COVERAGE),   0)]

  records = form_data[3:nrow(form_data), 11:ncol(form_data)]

  species_codes = unlist(lapply(records[1], trim), use.names = FALSE)

  records = records[2:nrow(records), lapply(.SD, function(value) { return(round(as.numeric(value), 2)) })]

  return(
    list(
      strata  = strata,
      records = list(
        codes = list(
          species = species_codes
        ),
        data = list(
          catches = records
        )
      )
    )
  )
}

validate_form_1RC_metadata = function(metadata) {
  submission_information = metadata$submission_information
  focal_point     = submission_information$focal_point
  organization    = submission_information$organization
  reference_dates = submission_information$reference_dates

  general_information    = metadata$general_information

  check_mandatory(focal_point$full_name,  "Metadata / Focal point full name")
  check_mandatory(focal_point$e_mail,     "Metadata / Focal point e-mail")

  check_mandatory(organization$full_name, "Metadata / Organization full name")

  check_mandatory(reference_dates$finalization, "Metadata / Finalization date")
  check_mandatory(reference_dates$submission,   "Metadata / Submission date")

  if(reference_dates$submission < reference_dates$finalization) stop(paste0("Metadata / Submission date (", reference_dates$submission, ") must follow the Metadata / Finalization date (", reference_dates$finalization, ")"))

  validate_year   (check_mandatory(general_information$reporting_year,   "Reporting year"))
  validate_entity (check_mandatory(general_information$reporting_entity, "Reporting entity"))
  validate_country(check_mandatory(general_information$flag_country,     "Flag country"))

  validate_fleet(general_information$reporting_entity,
                 general_information$flag_country)
}

validate_form_1RC_data = function(strata_and_records) {
  strata  = strata_and_records$strata
  records = strata_and_records$records

  catch_data = records$data$catches

  strata_empty_rows    = find_empty_rows(strata)
  strata_empty_columns = find_empty_columns(strata)

  strata[, IS_EMPTY := .I %in% strata_empty_rows]
  strata[, OCCURRENCES := .N, by = .(QUARTER, FISHERY_CODE, IOTC_MAIN_AREA_CODE, RETAIN_REASON_CODE, DATA_SOURCE_CODE, DATA_PROCESSING_CODE)]

  #strata_duplicated  = strata[, .(QUARTER, FISHERY_CODE, IOTC_MAIN_AREA_CODE, RETAIN_REASON_CODE, DATA_SOURCE_CODE, DATA_PROCESSING_CODE)]
  #strata_duplicated  = strata_duplicated[, COUNT := .N, by = .(QUARTER, FISHERY_CODE, IOTC_MAIN_AREA_CODE, RETAIN_REASON_CODE, DATA_SOURCE_CODE, DATA_PROCESSING_CODE)]

  total_strata     = nrow(strata)
  non_empty_strata = which(strata$IS_EMPTY == FALSE) #strata[ !1:.N %in% strata_empty_rows ]
  duplicate_strata = which(strata$OCCURRENCES > 1)   #which(strata_duplicated$COUNT > 1)
  duplicate_strata = duplicate_strata[ ! duplicate_strata %in% strata_empty_rows ]
  unique_strata    = non_empty_strata[ ! non_empty_strata %in% duplicate_strata ]

  data_empty_rows    = find_empty_rows(catch_data)
  data_empty_columns = find_empty_columns(catch_data)

  missing_quarters   = which( sapply(strata$QUARTER, is.na))
  invalid_quarters   = which(!sapply(strata$QUARTER, is_quarter_valid))
  invalid_quarters   = invalid_quarters[ ! invalid_quarters %in% missing_quarters ]
  missing_quarters   = missing_quarters[ ! missing_quarters %in% strata_empty_rows]

  missing_fisheries  = which( sapply(strata$FISHERY_CODE, is.na))
  invalid_fisheries  = which(!sapply(strata$FISHERY_CODE, is_fishery_valid))
  invalid_fisheries  = invalid_fisheries[ ! invalid_fisheries %in% missing_fisheries ]
  missing_fisheries  = missing_fisheries[ ! missing_fisheries %in% strata_empty_rows]

  fishery_aggregates = which(unlist(sapply(strata$FISHERY_CODE, function(value) { return(ifelse(is.na(value), FALSE, is_multiple_gear_fishery(value))) }, USE.NAMES = FALSE)))

  missing_IOTC_areas = which( sapply(strata$IOTC_MAIN_AREA_CODE, is.na))
  invalid_IOTC_areas = which(!sapply(strata$IOTC_MAIN_AREA_CODE, is_IOTC_main_area_valid))
  invalid_IOTC_areas = invalid_IOTC_areas[ ! invalid_IOTC_areas %in% missing_IOTC_areas ]
  missing_IOTC_areas = missing_IOTC_areas[ ! missing_IOTC_areas %in% strata_empty_rows ]

  missing_retain_reasons = which(sapply(strata$RETAIN_REASON_CODE, is.na))
  invalid_retain_reasons = which(!sapply(strata$RETAIN_REASON_CODE, is_retain_reason_valid))
  invalid_retain_reasons = invalid_retain_reasons[ ! invalid_retain_reasons %in% missing_retain_reasons ]
  missing_retain_reasons = missing_retain_reasons[ ! missing_retain_reasons %in% strata_empty_rows ]

  missing_types_of_data    = which( sapply(strata$DATA_TYPE_CODE, is.na))
  invalid_types_of_data    = which(!sapply(strata$DATA_TYPE_CODE, is_data_type_valid))
  invalid_types_of_data    = invalid_types_of_data[ ! invalid_types_of_data %in% missing_types_of_data ]
  missing_types_of_data    = missing_types_of_data[ ! missing_types_of_data %in% strata_empty_rows ]

  missing_data_sources     = which( sapply(strata$DATA_SOURCE_CODE, is.na))
  invalid_data_sources     = which(!sapply(strata$DATA_SOURCE_CODE, function(code) { return(is_data_source_valid("RC", code)) }))
  invalid_data_sources     = invalid_data_sources[ ! invalid_data_sources %in% missing_data_sources ]
  missing_data_sources     = missing_data_sources[ ! missing_data_sources %in% strata_empty_rows ]

  missing_data_processings = which( sapply(strata$DATA_PROCESSING_CODE, is.na))
  invalid_data_processings = which(!sapply(strata$DATA_PROCESSING_CODE, function(code) { return(is_data_processing_valid("RC", code)) }))
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

  species_occurrences = as.data.table(table(records$codes$species))
  colnames(species_occurrences) = c("SPECIES_CODE", "NUM_OCCURRENCES")

  species_occurrences_multiple = species_occurrences[NUM_OCCURRENCES > 1]

  zero_catches     = sum(catch_data == 0,   na.rm = TRUE)
  na_catches       = sum(is.na(catch_data), na.rm = TRUE)
  negative_catches = sum(catch_data < 0,    na.rm = TRUE)
  positive_catches = sum(catch_data > 0,    na.rm = TRUE)

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
        duplicate = list(
          number = length(duplicate_strata),
          row_indexes = duplicate_strata
        ),
        unique = list(
          number = length(unique_strata),
          row_indexes = unique_strata
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
            ),
            retain_reasons = list(
              invalid = list(
                number       = length(invalid_retain_reasons),
                row_indexes  = invalid_retain_reasons,
                codes        = strata[invalid_retain_reasons]$RETAIN_REASON_CODE,
                codes_unique = unique(strata[invalid_retain_reasons]$RETAIN_REASON_CODE)
              ),
              missing = list(
                number      = length(missing_retain_reasons),
                row_indexes = missing_retain_reasons
              )
            )
          ),
          original_data = list(
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
            ),
            multiple = list(
              species_codes    = species_occurrences_multiple$SPECIES_CODE,
              num_occurrencies = species_occurrences_multiple$NUM_OCCURRENCES
            )
          ),
          catch_values = list(
            negative = negative_catches,
            zero     = zero_catches,
            na       = na_catches,
            positive = positive_catches
          )
        )
      )
    )
  )
}

validate_form_1RC = function(form) {
  verify_form_type_1RC(form)

  metadata           = extract_metadata_1RC(form)
  strata_and_records = extract_strata_and_records_1RC(form)

  validate_form_1RC_metadata(metadata)         # Raises an error if metadata is inconsistent or plain wrong

  return(
    validate_form_1RC_data(strata_and_records) # Records validation results
  )
}

summary_form_1RC = function(form) {
  validation_messages = initialize_messages()

  validation_results = NULL

  tryCatch({
    validation_results = validate_form_1RC(form)
  }, error = function(e) {
    validation_messages <<- add_fatal(validation_messages, message = create_message(sheet = "Metadata", message = e$message))
  })

  if(!is.null(validation_results)) {
    strata  = validation_results$strata
    records = validation_results$records

    checks_strata  = strata$checks
    checks_records = records$checks

    # Strata issues / summary

    validation_messages = add_info(validation_messages, message = create_message(sheet = "Data", message = paste0("Total strata: ",     strata$total$number)))
    validation_messages = add_info(validation_messages, message = create_message(sheet = "Data", message = paste0("Non-empty strata: ", strata$non_empty$number)))
    validation_messages = add_info(validation_messages, message = create_message(sheet = "Data", message = paste0("Unique strata: ",    strata$unique$number)))

    if(strata$empty_rows$number > 0)
      validation_messages = add_fatal(validation_messages, message = create_message(sheet = "Data", message = paste0(strata$empty_rows$number, " empty strata detected: see rows ", paste0(strata$empty_rows$row_indexes, collapse = ", "))))

    if(strata$empty_columns$number > 0)
      validation_messages = add_fatal(validation_messages, message = create_message(sheet = "Data", message = paste0(strata$empty_columns$number, " empty strata columns detected: see columns ", paste0(strata$empty_columns$column_indexes, collapse = ", "))))

    if(strata$duplicate$number > 0)
      validation_messages = add_fatal(validation_messages, message = create_message(sheet = "Data", message = paste0(strata$duplicate$number, " duplicate strata detected: see rows ", paste0(strata$duplicate$row_indexes, collapse = ", "))))

    # Strata checks

    ## Main strata

    checks_strata_main = checks_strata$main

    quarters = checks_strata_main$quarters

    if(quarters$missing$number > 0)
      validation_messages = add_error(validation_messages, message = create_message(sheet = "Data", message = paste0("Missing quarter value in rows ", paste0(quarters$missing$row_indexes, collapse = ", "))))

    if(quarters$invalid$number > 0)
      validation_messages = add_error(validation_messages, message = create_message(sheet = "Data", message = paste0("Invalid quarter value in rows ", paste0(quarters$invalid$row_indexes, collapse = ", "))))

    fisheries = checks_strata_main$fisheries

    if(fisheries$missing$number > 0)
      validation_messages = add_error(validation_messages, message = create_message(sheet = "Data", message = paste0("Missing fishery in rows ", paste0(fisheries$missing$row_indexes, collapse = ", "))))

    if(fisheries$invalid$number > 0)
      validation_messages = add_error(validation_messages, message = create_message(sheet = "Data", message = paste0("Invalid fishery in rows ", paste0(fisheries$invalid$row_indexes, collapse = ", "))))

    if(fisheries$aggregates$number > 0)
      validation_messages = add_warning(validation_messages, message = create_message(sheet = "Data", message = paste0("Aggregated fisheries in rows ", paste0(fisheries$aggregates$row_indexes, collapse = ", "))))
  }

  return(
    validation_messages
  )
}
