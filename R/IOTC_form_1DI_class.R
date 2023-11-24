#' @include IOTC_form_RCDI_class.R
#' @export IOTCForm1DI
IOTCForm1DI = setClass(
  "IOTCForm1DI",
  contains = "IOTCFormRCDI"
)

setMethod("form_type", "IOTCForm1DI", function(form) {
  return("1-DI")
})

setMethod("form_version", "IOTCForm1DI", function(form) {
  return("1.0.0-interim")
})

setMethod("form_dataset_code", "IOTCForm1DI", function(form) {
  return("DI")
})

setMethod("first_data_column", "IOTCForm1DI", function(form) {
  return(which(EXCEL_COLUMNS == "M"))
})

setMethod("first_data_row", "IOTCForm1DI", function(form) {
  return(8)
})

setMethod("first_strata_column", "IOTCForm1DI", function(form) {
  return(which(EXCEL_COLUMNS == "B"))
})

setMethod("last_strata_column", "IOTCForm1DI", function(form) {
  return(which(EXCEL_COLUMNS == "K"))
})

setMethod("extract_data", "IOTCForm1DI", function(form) {
  # Based on the same method from IOTCForm1RC

  form_metadata = form@original_metadata
  form_data     = form@original_data

  has_data = nrow(form_data) >= 6

  strata = form_data[6:ifelse(has_data, nrow(form_data), 6)][, first_strata_column(form):last_strata_column(form)]

  if(!has_data) {
    strata = as.data.table(matrix(nrow = 0, ncol = length(colnames(strata))))
  }

  colnames(strata) = c("QUARTER", "FISHERY_CODE", "TARGET_SPECIES_CODE", "IOTC_MAIN_AREA_CODE", "DISCARD_REASON_CODE",
                       "DATA_TYPE_CODE", "DATA_SOURCE_CODE", "DATA_PROCESSING_CODE",
                       "COVERAGE_TYPE_CODE", "COVERAGE")

  strata[, QUARTER_ORIGINAL  := QUARTER]
  strata[, COVERAGE_ORIGINAL := COVERAGE]

  strata[, QUARTER    := as.integer(QUARTER)]
  strata[, COVERAGE   := round(as.numeric(COVERAGE),   0)]

  records = form_data[2:nrow(form_data), first_data_column(form):ncol(form_data)]

  species_codes   = unlist(lapply(records[1], trim), use.names = FALSE)
  condition_codes = unlist(lapply(records[2], trim), use.names = FALSE)
  raising_codes   = unlist(lapply(records[3], trim), use.names = FALSE)
  catch_unit_codes= unlist(lapply(records[4], trim), use.names = FALSE)

  if(length(species_codes)    == 1 && is.na(species_codes[1]))    species_codes    = NA
  if(length(condition_codes)  == 1 && is.na(condition_codes[1]))  condition_codes  = NA
  if(length(raising_codes)    == 1 && is.na(raising_codes[1]))    raising_codes    = NA
  if(length(catch_unit_codes) == 1 && is.na(catch_unit_codes[1])) catch_unit_codes = NA

  if(has_data) {
    # Might raise the "Warning in FUN(X[[i]], ...) : NAs introduced by coercion" message when catches include non-numeric values...
    records_original = records[5:nrow(records)]
    records          = records_original[, lapply(.SD, function(value) { return(round(as.numeric(value), 2)) })]
  } else {
    records_original = as.data.table(matrix(nrow = 0, ncol = ifelse(is_available(species_codes), 0, length(species_codes))))
    if(!is_available(species_codes)) colnames(records_original) = species_codes
    records          = records_original
  }

  return(
    list(
      strata = strata,
      records =
        list(
          codes = list(
            species     = species_codes,
            conditions  = condition_codes,
            raisings    = raising_codes,
            catch_units = catch_unit_codes
          ),
          data = list(
            catches_original = records_original,
            catches          = records
          )
        )
    )
  )
})

setMethod("validate_quarters",
          list(form = "IOTCForm1DI", strata = "data.table"),
          function(form, strata) {
            l_info("IOTCForm1DI.validate_quarters")

            all_year_quarter_strata = unique(strata[!is.na(QUARTER_ORIGINAL) & QUARTER == 0, .(FISHERY_CODE, TARGET_SPECIES_CODE, IOTC_MAIN_AREA_CODE, DISCARD_REASON_CODE, DATA_SOURCE_CODE, DATA_PROCESSING_CODE)])
            valid_quarters_strata   = strata[!is.na(QUARTER_ORIGINAL) & QUARTER %in% 1:4, .(NUM_QUARTERS = .N), keyby = .(FISHERY_CODE, TARGET_SPECIES_CODE, IOTC_MAIN_AREA_CODE, DISCARD_REASON_CODE, DATA_SOURCE_CODE, DATA_PROCESSING_CODE)]

            overlapping_quarters_strata = merge(all_year_quarter_strata, valid_quarters_strata, sort = FALSE, by = c("FISHERY_CODE", "TARGET_SPECIES_CODE", "IOTC_MAIN_AREA_CODE", "DISCARD_REASON_CODE", "DATA_SOURCE_CODE", "DATA_PROCESSING_CODE"))
            incomplete_quarters_strata  = valid_quarters_strata[NUM_QUARTERS < 4]

            overlapping_quarters = merge(strata, overlapping_quarters_strata, all.x = TRUE, sort = FALSE, by = c("FISHERY_CODE", "TARGET_SPECIES_CODE", "IOTC_MAIN_AREA_CODE", "DISCARD_REASON_CODE", "DATA_SOURCE_CODE", "DATA_PROCESSING_CODE"))
            overlapping_quarters = which(!is.na(overlapping_quarters$NUM_QUARTERS))

            incomplete_quarters  = merge(strata, incomplete_quarters_strata, all.x = TRUE, sort = FALSE, by = c("FISHERY_CODE", "TARGET_SPECIES_CODE", "IOTC_MAIN_AREA_CODE", "DISCARD_REASON_CODE", "DATA_SOURCE_CODE", "DATA_PROCESSING_CODE"))
            incomplete_quarters  = which(!is.na(incomplete_quarters$NUM_QUARTERS))

            return(
              list(
                overlapping_quarters = overlapping_quarters,
                incomplete_quarters  = incomplete_quarters
              )
            )
          }
)

setMethod("get_all_species_references_domain_and_codelist", "IOTCForm1DI", function(form) {
  l_debug("1-DI.get_all_species_references_domain_and_codelist")
  return(list(domain = "biological", codelist = "allSpecies"))
})

setMethod("get_all_species_references", "IOTCForm1DI", function(form) {
  l_debug("1-DI.get_all_species_references")
  return(iotc.data.reference.codelists::SPECIES)
})

setMethod("validate_data",
          list(form = "IOTCForm1DI", metadata_validation_results = "list"),
          function(form, metadata_validation_results) {
            l_info("IOTCForm1DI.validate_data")

            data_validation_results = callNextMethod(form, metadata_validation_results)

            strata  = form@data$strata
            strata$IS_EMPTY = NULL # Otherwise the 'find_empty_rows' call below will never return anything meaningful...

            records = form@data$records

            catch_data_original = records$data$catches_original

            strata_empty_rows    = find_empty_rows(strata)
            strata_empty_columns = find_empty_columns(strata)

            strata[, IS_EMPTY := .I %in% strata_empty_rows]
            strata[, OCCURRENCES := .N, by = .(QUARTER, FISHERY_CODE, TARGET_SPECIES_CODE, IOTC_MAIN_AREA_CODE, DISCARD_REASON_CODE, DATA_SOURCE_CODE, DATA_PROCESSING_CODE)]

            non_empty_strata = which(strata$IS_EMPTY == FALSE) #strata[ !1:.N %in% strata_empty_rows ]
            duplicate_strata = which(strata$OCCURRENCES > 1)   #which(strata_duplicated$COUNT > 1)
            duplicate_strata = duplicate_strata[ ! duplicate_strata %in% strata_empty_rows ]
            unique_strata    = non_empty_strata[ ! non_empty_strata %in% duplicate_strata ]

            missing_discard_reasons = which( is.na(strata$DISCARD_REASON_CODE))
            invalid_discard_reasons = which(!is.na(strata$DISCARD_REASON_CODE) & !is_discard_reason_valid(strata$DISCARD_REASON_CODE))
            invalid_discard_reasons = invalid_discard_reasons[ ! invalid_discard_reasons %in% missing_discard_reasons ]
            missing_discard_reasons = missing_discard_reasons[ ! missing_discard_reasons %in% strata_empty_rows ]

            missing_conditions   = which( is.na(records$codes$conditions))
            invalid_conditions   = which(!is_condition_valid(records$codes$conditions))
            invalid_conditions   = invalid_conditions[ ! invalid_conditions %in% missing_conditions ]

            missing_data_raisings   = which( is.na(records$codes$raisings))
            invalid_data_raisings   = which(!is_data_raising_valid(records$codes$raisings))
            invalid_data_raisings   = invalid_data_raisings[ ! invalid_data_raisings %in% missing_data_raisings ]

            missing_catch_units   = which( is.na(records$codes$catch_units))
            invalid_catch_units   = which(!is_catch_unit_valid(records$codes$catch_units))
            invalid_catch_units   = invalid_catch_units[ ! invalid_catch_units %in% missing_catch_units ]

            max_length = max(length(records$codes$species),
                             length(records$codes$conditions),
                             length(records$codes$raisings),
                             length(records$codes$catch_units))

            species     = pad(records$codes$species,     max_length)
            conditions  = pad(records$codes$conditions,  max_length)
            raisings    = pad(records$codes$raisings,    max_length)
            catch_units = pad(records$codes$catch_units, max_length)

            data_stratification = paste(species, conditions, raisings, catch_units, sep = "-")

            data_stratification_occurrences = as.data.table(table(data_stratification))

            if(nrow(data_stratification_occurrences) > 0) {
              colnames(data_stratification_occurrences) = c("STRATIFICATION_CODE", "NUM_OCCURRENCES")

              data_stratification_occurrences_multiple = data_stratification_occurrences[NUM_OCCURRENCES > 1]
              data_stratifications_multiple = which(data_stratification %in% data_stratification_occurrences_multiple$STRATIFICATION_CODE)
            } else {
              data_stratification_occurrences_multiple = data.table(STRATIFICATION_CODE = character(), NUM_OCCURRENCES = integer())
              data_stratifications_multiple = as.integer(array())
            }

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

            data_validation_results$strata$checks$main$discard_reasons = list(
              invalid = list(
                number       = length(invalid_discard_reasons),
                row_indexes  = spreadsheet_rows_for(form, invalid_discard_reasons),
                codes        = strata[invalid_discard_reasons]$DISCARD_REASON_CODE,
                codes_unique = unique(strata[invalid_discard_reasons]$DISCARD_REASON_CODE)
              ),
              missing = list(
                number      = length(missing_discard_reasons),
                row_indexes = spreadsheet_rows_for(form, missing_discard_reasons)
              )
            )

            data_validation_results$records$checks$conditions =
              list(
                missing = list(
                  number      = length(missing_conditions),
                  col_indexes = spreadsheet_cols_for(form, missing_conditions)
                ),
                invalid = list(
                  number       = length(invalid_conditions),
                  col_indexes  = spreadsheet_cols_for(form, invalid_conditions),
                  codes        = records$codes$conditions[invalid_conditions],
                  codes_unique = unique(records$codes$conditions[invalid_conditions])
                )
              )

            data_validation_results$records$checks$data_raisings =
              list(
                missing = list(
                  number      = length(missing_data_raisings),
                  col_indexes = spreadsheet_cols_for(form, missing_data_raisings)
                ),
                invalid = list(
                  number       = length(invalid_data_raisings),
                  col_indexes  = spreadsheet_cols_for(form, invalid_data_raisings),
                  codes        = records$codes$raisings[invalid_data_raisings],
                  codes_unique = unique(records$codes$raisings[invalid_data_raisings])
                )
              )

            data_validation_results$records$checks$catch_units =
              list(
                missing = list(
                  number      = length(missing_catch_units),
                  col_indexes = spreadsheet_cols_for(form, missing_catch_units)
                ),
                invalid = list(
                  number       = length(invalid_catch_units),
                  col_indexes  = spreadsheet_cols_for(form, invalid_catch_units),
                  codes        = records$codes$catch_units[invalid_catch_units],
                  codes_unique = unique(records$codes$catch_units[invalid_catch_units])
                )
              )

            data_validation_results$records$checks$stratifications =
              list(
                multiple = list(
                  number       = length(data_stratifications_multiple),
                  col_indexes  = spreadsheet_cols_for(form, data_stratifications_multiple),
                  codes_unique = data_stratification_occurrences_multiple$STRATIFICATION_CODE
                )
              )

            return(data_validation_results)
          }
)

setMethod("data_validation_summary",
          list(form = "IOTCForm1DI", metadata_validation_results = "list", data_validation_results = "list"),
          function(form, metadata_validation_results, data_validation_results) {
            l_info("IOTCForm1DI.data_validation_summary")

            validation_messages = common_data_validation_summary(form,
                                                                 metadata_validation_results,
                                                                 data_validation_results)

            strata  = data_validation_results$strata
            records = data_validation_results$records

            checks_strata  = strata$checks
            checks_records = records$checks

            ## Main strata

            checks_strata_main = checks_strata$main

            # Part of the validation comes from the superclass

            validation_messages = report_discard_reasons(validation_messages, checks_strata_main$discard_reasons)

            ## Original data

            # Validation comes from the superclass

            ###

            # Data issues / summary

            # Conditions at release
            validation_messages = report_conditions   (validation_messages, checks_records$conditions,    conditions_row    = 5)

            # Raising
            validation_messages = report_raisings     (validation_messages, checks_records$data_raisings, raisings_row      = 6)

            # Discard units
            validation_messages = report_discard_units(validation_messages, checks_records$catch_units,   discard_units_row = 7)

            # Species
            species = checks_records$species

            # Part of the validation comes from the superclass

            #if(species$multiple$number > 0)   # Multiple
            #  validation_messages = add(validation_messages, new("Message", level = "ERROR", source = "Data", text = paste0("Repeated species in column(s) #", paste0(species$multiple$col_indexes, collapse = ", "))))

            # Stratifications
            stratifications = checks_records$stratifications

            if(stratifications$multiple$number > 0)   # Multiple
              validation_messages = add(validation_messages, new("Message", level = "ERROR", source = "Data", text = paste0("Repeated species-condition-raising-catch units in column(s) ", paste0(stratifications$multiple$col_indexes, collapse = ", "))))

            ## Discards

            # Validation comes from the superclass

            return(validation_messages)
          }
)

## OUTPUT

setMethod("extract_output", list(form = "IOTCForm1DI", wide = "logical"),
          function(form, wide) {
            form = read(form)

            form_metadata = extract_metadata(form, common_metadata(form@original_metadata))
            form_data     = extract_data(form)

            strata = form_data$strata
            data   = form_data$records$data$catches

            species_qualifiers = paste0(form_data$records$codes$species, "_",
                                        form_data$records$codes$conditions, "_",
                                        form_data$records$codes$raisings, "_",
                                        form_data$records$codes$catch_units)

            colnames(data) = species_qualifiers


            year = form_metadata$general_information$reporting_year
            fleet = fleets_for(form_metadata$general_information$reporting_entity,
                               form_metadata$general_information$flag_country)

            strata$YEAR                   = year
            strata$REPORTING_ENTITY_CODE  = form_metadata$general_information$reporting_entity
            strata$FLAG_COUNTRY_CODE      = form_metadata$general_information$flag_country
            strata$FLEET_CODE             = fleet$FLEET_CODE

            strata = merge(strata, FISHERY_MAPPINGS, by = "FISHERY_CODE", all.x = TRUE, sort = FALSE)
            strata = strata[, .(REPORTING_ENTITY_CODE, FLAG_COUNTRY_CODE, FLEET_CODE,
                                YEAR, QUARTER,
                                FISHERY_CODE, TARGET_SPECIES_CODE,
                                GEAR_CODE, MAIN_GEAR_CODE, SCHOOL_TYPE_CODE,
                                DATA_TYPE_CODE, DATA_SOURCE_CODE, DATA_PROCESSING_CODE, COVERAGE_TYPE_CODE, COVERAGE,
                                IOTC_MAIN_AREA_CODE, DISCARD_REASON_CODE)]

            data = data[, lapply(.SD, function(value) { return(ifelse(is.na(value) | as.numeric(value) == 0, NA_real_, round(as.numeric(value), 2))) })]

            output_data = cbind(strata, data)

            if(!wide) {
              output_data = melt.data.table(output_data,
                                            id.vars = 1:17,
                                            value.name = "CATCH",
                                            variable.name = "SPECIES_QUALIFIER_CODE")

              species_qualifiers = str_split(string = output_data$SPECIES_QUALIFIER_CODE,
                                             pattern = "\\_",
                                             simplify = TRUE)

              output_data[, SPECIES_CODE    := species_qualifiers[, 1]]
              output_data[, CONDITION_CODE  := species_qualifiers[, 2]]
              output_data[, RAISING_CODE    := species_qualifiers[, 3]]
              output_data[, CATCH_UNIT_CODE := species_qualifiers[, 4]]

              output_data$SPECIES_QUALIFIER_CODE = NULL

              output_data = output_data[!is.na(CATCH) & CATCH > 0, .(REPORTING_ENTITY_CODE, FLAG_COUNTRY_CODE, FLEET_CODE,
                                                                     YEAR, QUARTER,
                                                                     FISHERY_CODE, TARGET_SPECIES_CODE,
                                                                     GEAR_CODE, MAIN_GEAR_CODE, SCHOOL_TYPE_CODE,
                                                                     DATA_TYPE_CODE, DATA_SOURCE_CODE, DATA_PROCESSING_CODE, COVERAGE_TYPE_CODE, COVERAGE,
                                                                     IOTC_MAIN_AREA_CODE, DISCARD_REASON_CODE,
                                                                     SPECIES_CODE, CONDITION_CODE, RAISING_CODE, CATCH, CATCH_UNIT_CODE)]
            }

            return(output_data)
          }
)
