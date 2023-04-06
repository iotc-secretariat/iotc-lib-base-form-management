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

  # Might raise the "Warning in FUN(X[[i]], ...) : NAs introduced by coercion" message when catches include non-numeric values...
  records_original = records[2:nrow(records)]
  records          = records_original[, lapply(.SD, function(value) { return(round(as.numeric(value), 2)) })]

  return(
    list(
      strata  = strata,
      records = list(
        codes = list(
          species = species_codes
        ),
        data = list(
          catches_original = records_original,
          catches          = records
        )
      )
    )
  )
}

validate_form_1RC_metadata = function(metadata) {
  submission_information = metadata$submission_information
  general_information    = metadata$general_information

  focal_point     = submission_information$focal_point
  organization    = submission_information$organization

  focal_point_available_full_name = is_provided(focal_point$full_name)
  focal_point_available_e_mail    = is_provided(focal_point$e_mail)

  organization_available_full_name = is_provided(organization$full_name)
  organization_available_e_mail    = is_provided(organization$e_mail)

  reference_dates = submission_information$reference_dates

  reference_dates_available_finalization = is_provided(reference_dates$finalization)
  reference_dates_available_submission   = is_provided(reference_dates$submission)

  reference_dates_valid_finalization = reference_dates_available_finalization & reference_dates$finalization <= format(Sys.time())
  reference_dates_valid_submission   = reference_dates_available_submission   & reference_dates$submission   <= format(Sys.time())

  reference_dates_coherent = reference_dates_valid_finalization &
                             reference_dates_valid_submission &
                             reference_dates$finalization >= reference_dates$submission

  reporting_year_available   = is_provided(general_information$reporting_year)
  reporting_entity_available = is_provided(general_information$reporting_entity)
  flag_country_available     = is_provided(general_information$flag_country)

  reporting_year_valid   = reporting_year_available & is_year_valid(general_information$reporting_year)
  reporting_entity_valid = reporting_entity_available & is_entity_valid(general_information$reporting_entity)
  flag_country_valid     = flag_country_available & is_country_valid(general_information$flag_country)

  fleet_valid = reporting_entity_valid &
                flag_country_valid &
                is_fleet_valid(general_information$reporting_entity,
                               general_information$flag_country)

  fleet = NA

  if(fleet_valid)
    fleet = fleets_for(general_information$reporting_entity,
                       general_information$flag_country)

  return(
    list(
      submission_information = list(
        focal_point = list(
          available = list(
            full_name = focal_point_available_full_name,
            e_mail    = organization_available_e_mail
          )
        ),
        organization = list(
          available = list(
            full_name = organization_available_full_name,
            e_mail    = organization_available_e_mail
          )
        ),
        reference_dates = list(
          finalization = list(
            available = is_provided(reference_dates$finalization),
            value     = reference_dates$finalization,
            valid     = reference_dates_valid_finalization
          ),
          submission = list(
            available = is_provided(reference_dates$submission),
            value     = reference_dates$submission,
            valid     = reference_dates_valid_submission
          ),
          checks = list(
            dates_are_coherent = reference_dates_coherent
          )
        )
      ),
      general_information = list(
        reporting_year = list(
          available = reporting_year_available,
          value     = general_information$reporting_year,
          valid     = reporting_year_valid
        ),
        reporting_entity = list(
          available = reporting_entity_available,
          code      = general_information$reporting_entity,
          valid     = reporting_entity_valid
        ),
        flag_country = list(
          available = flag_country_available,
          code      = general_information$flag_country,
          valid     = flag_country_valid
        ),
        fleet = list(
          valid     = fleet_valid,
          code      = fleet
        )
      )
    )
  )

  # TO BE REMOVED
  if(FALSE) {
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
}

validate_form_1RC_data = function(strata_and_records) {
  strata  = strata_and_records$strata
  records = strata_and_records$records

  catch_data_original = records$data$catches_original
  catch_data          = records$data$catches

  strata_empty_rows    = find_empty_rows(strata)
  strata_empty_columns = find_empty_columns(strata)

  strata[, IS_EMPTY := .I %in% strata_empty_rows]
  strata[, OCCURRENCES := .N, by = .(QUARTER, FISHERY_CODE, IOTC_MAIN_AREA_CODE, RETAIN_REASON_CODE, DATA_SOURCE_CODE, DATA_PROCESSING_CODE)]

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

  # If all quarters are provided and valid, we check that they're also consistent...

  #if(length(invalid_quarters) + length(missing_quarters) == 0) {
  all_year_quarter_strata = unique(strata[QUARTER == 0, .(FISHERY_CODE, IOTC_MAIN_AREA_CODE, RETAIN_REASON_CODE, DATA_SOURCE_CODE, DATA_PROCESSING_CODE)])
  valid_quarters_strata   = strata[QUARTER %in% 1:4, .(NUM_QUARTERS = .N), keyby = .(FISHERY_CODE, IOTC_MAIN_AREA_CODE, RETAIN_REASON_CODE, DATA_SOURCE_CODE, DATA_PROCESSING_CODE)]

  overlapping_quarters_strata = merge(all_year_quarter_strata, valid_quarters_strata, sort = FALSE, by = c("FISHERY_CODE", "IOTC_MAIN_AREA_CODE", "RETAIN_REASON_CODE", "DATA_SOURCE_CODE", "DATA_PROCESSING_CODE"))
  incomplete_quarters_strata  = valid_quarters_strata[NUM_QUARTERS < 4]

  overlapping_quarters = merge(strata, overlapping_quarters_strata, all.x = TRUE, sort = FALSE, by = c("FISHERY_CODE", "IOTC_MAIN_AREA_CODE", "RETAIN_REASON_CODE", "DATA_SOURCE_CODE", "DATA_PROCESSING_CODE"))
  overlapping_quarters = which(!is.na(overlapping_quarters$NUM_QUARTERS))

  incomplete_quarters  = merge(strata, incomplete_quarters_strata, all.x = TRUE, sort = FALSE, by = c("FISHERY_CODE", "IOTC_MAIN_AREA_CODE", "RETAIN_REASON_CODE", "DATA_SOURCE_CODE", "DATA_PROCESSING_CODE"))
  incomplete_quarters  = which(!is.na(incomplete_quarters$NUM_QUARTERS))
  #}

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

  species_multiple = which(records$codes$species %in% species_occurrences_multiple$SPECIES_CODE)

  non_num_catches  = sum(!is.numeric(catch_data_original), na.rm = TRUE)

  na_catches       = sum(is.numeric(catch_data) & is.na(catch_data), na.rm = TRUE)
  zero_catches     = sum(is.numeric(catch_data) & catch_data == 0,   na.rm = TRUE)
  negative_catches = sum(is.numeric(catch_data) & catch_data < 0,    na.rm = TRUE)
  positive_catches = sum(is.numeric(catch_data) & catch_data > 0,    na.rm = TRUE)

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
              ),
              overlapping = list(
                number = length(overlapping_quarters),
                row_indexes = overlapping_quarters
              ),
              incomplete = list(
                number = length(incomplete_quarters),
                row_indexes = incomplete_quarters
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
              number       = length(species_multiple),
              col_indexes  = species_multiple,
              codes_unique = species_occurrences_multiple$SPECIES_CODE
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

validate_form_1RC = function(form) {
  verify_form_type_1RC(form)

  metadata           = extract_metadata_1RC(form)
  strata_and_records = extract_strata_and_records_1RC(form)

  metadata_validation = validate_form_1RC_metadata(metadata)
  data_validation     = validate_form_1RC_data(strata_and_records)

  return(
    list(
      metadata = metadata_validation, # validate_form_1RC_metadata(metadata),
      data     = data_validation     # validate_form_1RC_data(strata_and_records)
    )
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
    metadata_validation_results = validation_results$metadata
    data_validation_results     = validation_results$data

    if(!is.null(metadata_validation_results)) {
      metadata_validation_messages = summary_form_1RC_metadata(metadata_validation_results)

      validation_messages$INFO   = rbind(validation_messages$INFO, metadata_validation_messages$INFO)
      validation_messages$WARN   = rbind(validation_messages$WARN, metadata_validation_messages$WARN)
      validation_messages$ERROR  = rbind(validation_messages$ERROR, metadata_validation_messages$ERROR)
      validation_messages$FATAL = rbind(validation_messages$FATAL, metadata_validation_messages$FATAL)
    }

    if(!is.null(data_validation_results)) {
      data_validation_messages = summary_form_1RC_data(data_validation_results)

      validation_messages$INFO   = rbind(validation_messages$INFO, data_validation_messages$INFO)
      validation_messages$WARN   = rbind(validation_messages$WARN, data_validation_messages$WARN)
      validation_messages$ERROR  = rbind(validation_messages$ERROR, data_validation_messages$ERROR)
      validation_messages$FATAL = rbind(validation_messages$FATAL, data_validation_messages$FATAL)
    }
  }

  msg_info    = nrow(validation_messages$INFO)
  msg_warning = nrow(validation_messages$WARN)
  msg_error   = nrow(validation_messages$ERROR)
  msg_fatal   = nrow(validation_messages$FATAL)

  summary =
    ifelse(
      msg_fatal > 0, "Fatal issues encountered: check file consistency with respect to official IOTC forms, ensure that all metadata are correct, and verify that no empty or duplicate strata is found in the 'Data' worksheet",
      ifelse(msg_error > 0, "Errors encountered: check that all codes correspond to official reference codes and that the semantics of the provided data is enforced",
             ifelse(msg_warn > 0, "Warnings encountered: the file can be processed, but it is recommended to identify and fix the source of the identified warnings",
                    "The file can be successfully processed"
             )
      )
    )

  return(
    list(
      summary             = summary,
      can_be_processed    = msg_fatal == 0 & msg_error == 0,
      info_messages       = msg_info,
      warning_messages    = msg_warning,
      error_messages      = msg_error,
      fatal_messages      = msg_fatal,
      validation_messages = validation_messages
    )
  )
}

summary_form_1RC_metadata = function(metadata_validation_results) {
  validation_messages = initialize_messages()

  submission_information = metadata_validation_results$submission_information
  general_information    = metadata_validation_results$general_information

  # Submission information

  ## Focal point

  if(!submission_information$focal_point$available$full_name)
    validation_messages = add_error(validation_messages, message = create_message(sheet = "Metadata", message = "The focal point full name is mandatory"))

  if(!submission_information$focal_point$available$e_mail)
    validation_messages = add_error(validation_messages, message = create_message(sheet = "Metadata", message = "The focal point e-mail is mandatory"))

  ## Organization

  if(!submission_information$organization$available$full_name)
    validation_messages = add_error(validation_messages, message = create_message(sheet = "Metadata", message = "The organization name is mandatory"))

  if(!submission_information$organization$available$e_mail)
    validation_messages = add_warning(validation_messages, message = create_message(sheet = "Metadata", message = "The organization e-mail is not available"))

  ## Reference dates

  if(!submission_information$reference_dates$finalization$available)
    validation_messages = add_error(validation_messages, message = create_message(sheet = "Metadata", message = "The finalization date is mandatory"))
  else if(!submission_information$reference_dates$finalization$valid)
    validation_messages = add_error(validation_messages, message = create_message(sheet = "Metadata", message = paste0("The finalization date (", submission_information$reference_dates$finalization$value, ") is not valid")))

  if(!submission_information$reference_dates$submission$available)
    validation_messages = add_error(validation_messages, message = create_message(sheet = "Metadata", message = "The submission date is mandatory"))
  else if(!submission_information$reference_dates$submission$valid)
    validation_messages = add_error(validation_messages, message = create_message(sheet = "Metadata", message = paste0("The submission date (", submission_information$reference_dates$submission$value, ") is not valid")))

  if( submission_information$reference_dates$finalization$available &
      submission_information$reference_dates$submission$available &
      !submission_information$reference_dates$checks$dates_are_coherent)
    validation_messages = add_error(validation_messages, message = create_message(sheet = "Metadata", message = "The submission date preceeds the finalization date"))

  # General information

  ## Reporting year

  if(!general_information$reporting_year$available)
    validation_messages = add_fatal(validation_messages, message = create_message(sheet = "Metadata", message = "The reporting year is mandatory"))
  else if(!general_information$reporting_year$valid)
    validation_messages = add_fatal(validation_messages, message = create_message(sheet = "Metadata", message = paste0("The reporting year (", general_information$reporting_year$value, ") must not be in the future")))

  ## Reporting entity

  if(!general_information$reporting_entity$available)
    validation_messages = add_fatal(validation_messages, message = create_message(sheet = "Metadata", message = "The reporting entity is mandatory"))
  else if(!general_information$reporting_entity$valid)
    validation_messages = add_fatal(validation_messages, message = create_message(sheet = "Metadata", message = paste0("The provided reporting entity (", general_information$reporting_entity$code, ") is not valid. Please refer to ", reference_codes("admin", "entities"), " for a list of valid entity codes")))

  ## Flag country

  if(!general_information$flag_country$available)
    validation_messages = add_fatal(validation_messages, message = create_message(sheet = "Metadata", message = "The flag country is mandatory"))
  else if(!general_information$flag_country$valid)
    validation_messages = add_fatal(validation_messages, message = create_message(sheet = "Metadata", message = paste0("The provided flag country (", general_information$flag_country$code, ") is not valid. Please refer to ", reference_codes("admin", "countries"), " for a list of valid country codes")))

  if( general_information$reporting_entit$valid &
      general_information$flag_country$valid &
      !general_information$fleet$valid)
    validation_messages = add_fatal(validation_messages, message = create_message(sheet = "Metadata", message = paste0("The provided reporting entity (", general_information$reporting_entity$code, ") and flag country (", general_information$flag_country$code, ") do not identify any valid fleet")))
  else
    validation_messages = add_info(validation_messages, message = create_message(sheet = "Metadata", message = paste0("The provided reporting entity (", general_information$reporting_entity$code, ") and flag country (", general_information$flag_country$code, ") identify ", general_information$fleet$code, " as fleet")))
  return(
    validation_messages
  )
}

summary_form_1RC_data = function(data_validation_results) {
  validation_messages = initialize_messages()

  ### STRATA AND RECORDS

  validation_messages = add_info(validation_messages, message = create_message(sheet = "Data", message = "Provided catch data is assumed to be raised to total annual catches and reported in live-weight equivalent by default"))

  strata  = data_validation_results$strata
  records = data_validation_results$records

  checks_strata  = strata$checks
  checks_records = records$checks

  # Strata issues / summary

  validation_messages = add_info(validation_messages, message = create_message(sheet = "Data", message = paste0("Total strata: ",     strata$total$number)))
  validation_messages = add_info(validation_messages, message = create_message(sheet = "Data", message = paste0("Non-empty strata: ", strata$non_empty$number)))
  validation_messages = add_info(validation_messages, message = create_message(sheet = "Data", message = paste0("Unique strata: ",    strata$unique$number)))

  if(strata$empty_rows$number > 0)
    validation_messages = add_fatal(validation_messages, message = create_message(sheet = "Data", message = paste0(strata$empty_rows$number, " empty strata detected: see row(s) ", paste0(strata$empty_rows$row_indexes, collapse = ", "))))

  if(strata$empty_columns$number > 0)
    validation_messages = add_fatal(validation_messages, message = create_message(sheet = "Data", message = paste0(strata$empty_columns$number, " empty strata columns detected: see columns ", paste0(strata$empty_columns$column_indexes, collapse = ", "))))

  if(strata$duplicate$number > 0)
    validation_messages = add_fatal(validation_messages, message = create_message(sheet = "Data", message = paste0(strata$duplicate$number, " duplicate strata detected: see row(s) ", paste0(strata$duplicate$row_indexes, collapse = ", "))))

  # Strata checks

  ## Main strata

  checks_strata_main = checks_strata$main

  quarters = checks_strata_main$quarters

  if(quarters$incomplete$number > 0)
    validation_messages = add_warning(validation_messages, message = create_message(sheet = "Data", message = paste0("Data is not provided for all quarters within the strata in row(s) ", paste0(quarters$incomplete$row_indexes, collapse = ", "))))

  if(quarters$missing$number > 0)
    validation_messages = add_error(validation_messages, message = create_message(sheet = "Data", message = paste0("Missing quarter value in row(s) ", paste0(quarters$missing$row_indexes, collapse = ", "))))

  if(quarters$invalid$number > 0)
    validation_messages = add_error(validation_messages, message = create_message(sheet = "Data", message = paste0("Invalid quarter value in row(s) ", paste0(quarters$invalid$row_indexes, collapse = ", "), ". Please use only 1-4 for Q1-Q4 or 0 for 'entire year'")))

  if(quarters$overlapping$number > 0)
    validation_messages = add_error(validation_messages, message = create_message(sheet = "Data", message = paste0("Data is provided for overlapping quarters within the strata in row(s) ", paste0(quarters$overlapping$row_indexes, collapse = ", "))))

  fisheries = checks_strata_main$fisheries

  if(fisheries$aggregates$number > 0)
    validation_messages = add_warning(validation_messages, message = create_message(sheet = "Data", message = paste0("Aggregated fisheries in row(s) ", paste0(fisheries$aggregates$row_indexes, collapse = ", "))))

  if(fisheries$missing$number > 0)
    validation_messages = add_error(validation_messages, message = create_message(sheet = "Data", message = paste0("Missing fishery in row(s) ", paste0(fisheries$missing$row_indexes, collapse = ", "))))

  if(fisheries$invalid$number > 0)
    validation_messages = add_error(validation_messages, message = create_message(sheet = "Data", message = paste0("Invalid fishery in row(s) ", paste0(fisheries$invalid$row_indexes, collapse = ", "), ". Please refer to ", reference_codes("fisheries", "fisheries"), " for a list of valid fishery codes")))

  areas = checks_strata_main$IOTC_main_areas

  if(areas$missing$number > 0)
    validation_messages = add_error(validation_messages, message = create_message(sheet = "Data", message = paste0("Missing IOTC main area in row(s) ", paste0(areas$missing$row_indexes, collapse = ", "))))

  if(areas$invalid$number > 0)
    validation_messages = add_error(validation_messages, message = create_message(sheet = "Data", message = paste0("Invalid IOTC main area in row(s) ", paste0(areas$invalid$row_indexes, collapse = ", "), ". Please refer to ", reference_codes("admin", "IOTCareasMain"), " for a list of valid IOTC main area codes")))

  retain_reasons = checks_strata_main$retain_reasons

  if(retain_reasons$missing$number > 0)
    validation_messages = add_error(validation_messages, message = create_message(sheet = "Data", message = paste0("Missing retain reason in row(s) ", paste0(retain_reasons$missing$row_indexes, collapse = ", "))))

  if(retain_reasons$invalid$number > 0)
    validation_messages = add_error(validation_messages, message = create_message(sheet = "Data", message = paste0("Invalid retain reason in row(s) ", paste0(retain_reasons$invalid$row_indexes, collapse = ", "), ". Please refer to ", reference_codes("biological", "retainReasons"), " for a list of valid retain reason codes")))

  ## Original data

  checks_strata_original_data = checks_strata$original_data

  types = checks_strata_original_data$type # NOT PART OF THE STRATUM

  if(types$missing$number > 0)
    validation_messages = add_error(validation_messages, message = create_message(sheet = "Data", message = paste0("Missing original data type in row(s) ", paste0(types$missing$row_indexes, collapse = ", "))))

  if(types$invalid$number > 0)
    validation_messages = add_error(validation_messages, message = create_message(sheet = "Data", message = paste0("Invalid original data type in row(s) ", paste0(types$invalid$row_indexes, collapse = ", "), ". Please refer to ", reference_codes("data", "types"), " for a list of valid data type codes")))

  sources = checks_strata_original_data$source

  if(sources$missing$number > 0)
    validation_messages = add_error(validation_messages, message = create_message(sheet = "Data", message = paste0("Missing original data source in row(s) ", paste0(sources$missing$row_indexes, collapse = ", "))))

  if(sources$invalid$number > 0)
    validation_messages = add_error(validation_messages, message = create_message(sheet = "Data", message = paste0("Invalid original data source in row(s) ", paste0(sources$invalid$row_indexes, collapse = ", "), ". Please refer to ", reference_codes("data", "sourcesRC"), " for a list of valid data source codes for this dataset")))

  processings = checks_strata_original_data$processing

  if(processings$missing$number > 0)
    validation_messages = add_error(validation_messages, message = create_message(sheet = "Data", message = paste0("Missing original data processing in row(s) ", paste0(processings$missing$row_indexes, collapse = ", "))))

  if(processings$invalid$number > 0)
    validation_messages = add_error(validation_messages, message = create_message(sheet = "Data", message = paste0("Invalid original data processing in row(s) ", paste0(processings$invalid$row_indexes, collapse = ", "), ". Please refer to ", reference_codes("data", "processingsRC"), " for a list of valid data processing codes for this dataset")))

  checks_strata_coverage = checks_strata$coverage # NOT PART OF THE STRATUM

  coverage_types = checks_strata_coverage$type

  if(coverage_types$missing$number > 0)
    validation_messages = add_error(validation_messages, message = create_message(sheet = "Data", message = paste0("Missing coverage type in row(s) ", paste0(coverage_types$missing$row_indexes, collapse = ", "))))

  if(coverage_types$invalid$number > 0)
    validation_messages = add_error(validation_messages, message = create_message(sheet = "Data", message = paste0("Invalid coverage type in row(s) ", paste0(coverage_types$invalid$row_indexes, collapse = ", "), ". Please refer to ", reference_codes("data", "coverageTypes"), " for a list of valid coverage type codes")))

  coverage_values = checks_strata_coverage$value

  if(coverage_values$missing$number > 0)
    validation_messages = add_error(validation_messages, message = create_message(sheet = "Data", message = paste0("Missing coverage type in row(s) ", paste0(coverage_values$missing$row_indexes, collapse = ", "))))

  if(coverage_values$invalid$number > 0)
    validation_messages = add_error(validation_messages, message = create_message(sheet = "Data", message = paste0("Invalid coverage type in row(s) ", paste0(coverage_values$invalid$row_indexes, collapse = ", "))))

  ###

  # Data issues / summary

  ## Species

  species = checks_records$species

  if(species$aggregates$number > 0) # Aggregates
    validation_messages = add_warning(validation_messages, message = create_message(sheet = "Data", message = paste0("Aggregated species in column(s) ", paste0(species$aggregates$col_indexes, collapse = ", "))))

  if(species$missing$number > 0)    # Missing
    validation_messages = add_error(validation_messages, message = create_message(sheet = "Data", message = paste0("Missing species in column(s) ", paste0(species$missing$col_indexes, collapse = ", "))))

  if(species$invalid$number > 0)    # Invalid
    validation_messages = add_error(validation_messages, message = create_message(sheet = "Data", message = paste0("Invalid species in column(s) ", paste0(species$invalid$col_indexes, collapse = ", "), ". Please refer to ", reference_codes("biological", "allSpecies"), " for a list of valid species codes")))

  if(species$multiple$number > 0)    # Multiple
    validation_messages = add_error(validation_messages, message = create_message(sheet = "Data", message = paste0("Repeated species in column(s) ", paste0(species$multiple$col_indexes, collapse = ", "))))

  ## Catches

  catches = checks_records$catch_values

  if(catches$positive > 0)
    validation_messages = add_info(validation_messages, message = create_message(sheet = "Data", message = paste0(catches$positive, " positive catch value(s) reported")))

  if(catches$na > 0)
    validation_messages = add_info(validation_messages, message = create_message(sheet = "Data", message = paste0(catches$na, " empty catch value(s) reported for all strata / species combinations")))

  if(catches$zero > 0)
    validation_messages = add_warning(validation_messages, message = create_message(sheet = "Data", message = paste0(catches$zero, " catch value(s) explicitly reported as zero: consider not providing these values at all")))

  if(catches$negative > 0)
    validation_messages = add_error(validation_messages, message = create_message(sheet = "Data", message = paste0(catches$negative, " negative catch value(s) reported")))

  if(catches$non_num > 0)
    validation_messages = add_error(validation_messages, message = create_message(sheet = "Data", message = paste0(catches$non_num, " non-numeric catch value(s) reported")))
}
