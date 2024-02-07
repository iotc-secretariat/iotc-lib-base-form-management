#' @include IOTC_form_RCDI_class.R
#' @export IOTCForm1RC
IOTCForm1RC = setClass(
  "IOTCForm1RC",
  contains = "IOTCFormRCDI"
)

setMethod("form_type", "IOTCForm1RC", function(form) {
  return(c("1-RC", "1RC")) # For backwards compatibility
})

setMethod("form_version", "IOTCForm1RC", function(form) {
  return("1.0.0")
})

setMethod("form_dataset_code", "IOTCForm1RC", function(form) {
  return("RC")
})

setMethod("first_data_column", "IOTCForm1RC", function(form) {
  return(which(EXCEL_COLUMNS == "L"))
})

setMethod("first_data_row", "IOTCForm1RC", function(form) {
  return(6)
})

setMethod("first_strata_column", "IOTCForm1RC", function(form) {
  return(which(EXCEL_COLUMNS == "B"))
})

setMethod("last_strata_column", "IOTCForm1RC", function(form) {
  return(which(EXCEL_COLUMNS == "K"))
})

setMethod("extract_data", "IOTCForm1RC", function(form) {
  l_info("IOTCForm1RC.extract_data")

  form_metadata = form@original_metadata
  form_data     = form@original_data

  has_data = nrow(form_data) >= 4

  strata = form_data[4:ifelse(has_data, nrow(form_data), 4)][, first_strata_column(form):last_strata_column(form)]

  if(!has_data) {
    strata = as.data.table(matrix(nrow = 0, ncol = length(colnames(strata))))
  }

  colnames(strata) = c("QUARTER", "FISHERY_CODE", "TARGET_SPECIES_CODE", "IOTC_MAIN_AREA_CODE", "RETAIN_REASON_CODE",
                       "DATA_TYPE_CODE", "DATA_SOURCE_CODE", "DATA_PROCESSING_CODE",
                       "COVERAGE_TYPE_CODE", "COVERAGE")

  strata[, QUARTER_ORIGINAL  := QUARTER]
  strata[, COVERAGE_ORIGINAL := COVERAGE]

  strata[, QUARTER    := as.integer(QUARTER)]
  strata[, COVERAGE   := round(as.numeric(COVERAGE),   0)]

  records = form_data[3:ifelse(has_data, nrow(form_data), 3), first_data_column(form):ncol(form_data)]

  species_codes = unlist(lapply(records[1], trim), use.names = FALSE)

  if(length(species_codes) == 1 && is.na(species_codes[1])) species_codes = NA

  if(has_data) {
    # Might raise the "Warning in FUN(X[[i]], ...) : NAs introduced by coercion" message when catches include non-numeric values...
    records_original = records[2:nrow(records)]
    records          = records_original[, lapply(.SD, function(value) { return(round(as.numeric(value), 2)) })]
  } else {
    records_original = as.data.table(matrix(nrow = 0, ncol = ifelse(is_available(species_codes), 0, length(species_codes))))

    #Was !is_available...
    if(is_available(species_codes)) colnames(records_original) = species_codes
    records          = records_original
  }

  return(
    list(
      strata = strata,
      records =
        list(
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
})

setMethod("validate_quarters",
          list(form = "IOTCForm1RC", strata = "data.table"),
          function(form, strata) {
            l_info("IOTCForm1RC.validate_quarters")

            all_year_quarter_strata = unique(strata[!is.na(QUARTER_ORIGINAL) & QUARTER == 0, .(FISHERY_CODE, TARGET_SPECIES_CODE, IOTC_MAIN_AREA_CODE, RETAIN_REASON_CODE, DATA_SOURCE_CODE, DATA_PROCESSING_CODE)])
            valid_quarters_strata   = strata[!is.na(QUARTER_ORIGINAL) & QUARTER %in% 1:4, .(NUM_QUARTERS = .N), keyby = .(FISHERY_CODE, TARGET_SPECIES_CODE, IOTC_MAIN_AREA_CODE, RETAIN_REASON_CODE, DATA_SOURCE_CODE, DATA_PROCESSING_CODE)]

            overlapping_quarters_strata = merge(all_year_quarter_strata, valid_quarters_strata, sort = FALSE, by = c("FISHERY_CODE", "TARGET_SPECIES_CODE", "IOTC_MAIN_AREA_CODE", "RETAIN_REASON_CODE", "DATA_SOURCE_CODE", "DATA_PROCESSING_CODE"))
            incomplete_quarters_strata  = valid_quarters_strata[NUM_QUARTERS < 4]

            overlapping_quarters = merge(strata, overlapping_quarters_strata, all.x = TRUE, sort = FALSE, by = c("FISHERY_CODE", "TARGET_SPECIES_CODE", "IOTC_MAIN_AREA_CODE", "RETAIN_REASON_CODE", "DATA_SOURCE_CODE", "DATA_PROCESSING_CODE"))
            overlapping_quarters = which(!is.na(overlapping_quarters$NUM_QUARTERS))

            incomplete_quarters  = merge(strata, incomplete_quarters_strata, all.x = TRUE, sort = FALSE, by = c("FISHERY_CODE", "TARGET_SPECIES_CODE", "IOTC_MAIN_AREA_CODE", "RETAIN_REASON_CODE", "DATA_SOURCE_CODE", "DATA_PROCESSING_CODE"))
            incomplete_quarters  = which(!is.na(incomplete_quarters$NUM_QUARTERS))

            return(
              list(
                overlapping_quarters = overlapping_quarters,
                incomplete_quarters  = incomplete_quarters
              )
            )
          }
)

setMethod("get_all_species_references_domain_and_codelist", "IOTCForm1RC", function(form) {
  l_debug("1-RC.get_all_species_references_domain_and_codelist")
  return(list(domain = "legacy", codelist = "species"))
})

setMethod("get_all_species_references", "IOTCForm1RC", function(form) {
  l_debug("1-RC.get_all_species_references")
  return(iotc.data.reference.codelists::LEGACY_SPECIES)
})

setMethod("validate_data",
          list(form = "IOTCForm1RC", metadata_validation_results = "list"),
          function(form, metadata_validation_results) {
            l_info("IOTCForm1RC.validate_data")

            data_validation_results = callNextMethod(form, metadata_validation_results)

            strata  = form@data$strata
            strata$IS_EMPTY = NULL # Otherwise the 'find_empty_rows' call below will never return anything meaningful...

            records = form@data$records

            catch_data_original = records$data$catches_original

            strata_empty_rows    = find_empty_rows(strata)
            strata_empty_columns = find_empty_columns(strata)

            strata[, IS_EMPTY := .I %in% strata_empty_rows]
            strata[, OCCURRENCES := .N, by = .(QUARTER, FISHERY_CODE, TARGET_SPECIES_CODE, IOTC_MAIN_AREA_CODE, RETAIN_REASON_CODE, DATA_SOURCE_CODE, DATA_PROCESSING_CODE)]

            # If all quarters are provided and valid, we check that they're also consistent...
            all_year_quarter_strata = unique(strata[!is.na(QUARTER_ORIGINAL) & QUARTER == 0, .(FISHERY_CODE, TARGET_SPECIES_CODE, IOTC_MAIN_AREA_CODE, RETAIN_REASON_CODE, DATA_SOURCE_CODE, DATA_PROCESSING_CODE)])
            valid_quarters_strata   = strata[!is.na(QUARTER_ORIGINAL) & QUARTER %in% 1:4, .(NUM_QUARTERS = .N), keyby = .(FISHERY_CODE, TARGET_SPECIES_CODE, IOTC_MAIN_AREA_CODE, RETAIN_REASON_CODE, DATA_SOURCE_CODE, DATA_PROCESSING_CODE)]

            overlapping_quarters_strata = merge(all_year_quarter_strata, valid_quarters_strata, sort = FALSE, by = c("FISHERY_CODE", "TARGET_SPECIES_CODE", "IOTC_MAIN_AREA_CODE", "RETAIN_REASON_CODE", "DATA_SOURCE_CODE", "DATA_PROCESSING_CODE"))
            incomplete_quarters_strata  = valid_quarters_strata[NUM_QUARTERS < 4]

            overlapping_quarters = merge(strata, overlapping_quarters_strata, all.x = TRUE, sort = FALSE, by = c("FISHERY_CODE", "TARGET_SPECIES_CODE", "IOTC_MAIN_AREA_CODE", "RETAIN_REASON_CODE", "DATA_SOURCE_CODE", "DATA_PROCESSING_CODE"))
            overlapping_quarters = which(!is.na(overlapping_quarters$NUM_QUARTERS))

            incomplete_quarters  = merge(strata, incomplete_quarters_strata, all.x = TRUE, sort = FALSE, by = c("FISHERY_CODE", "TARGET_SPECIES_CODE", "IOTC_MAIN_AREA_CODE", "RETAIN_REASON_CODE", "DATA_SOURCE_CODE", "DATA_PROCESSING_CODE"))
            incomplete_quarters  = which(!is.na(incomplete_quarters$NUM_QUARTERS))

            non_empty_strata = which(strata$IS_EMPTY == FALSE) #strata[ !1:.N %in% strata_empty_rows ]
            duplicate_strata = which(strata$OCCURRENCES > 1)   #which(strata_duplicated$COUNT > 1)
            duplicate_strata = duplicate_strata[ ! duplicate_strata %in% strata_empty_rows ]
            unique_strata    = non_empty_strata[ ! non_empty_strata %in% duplicate_strata ]

            missing_retain_reasons = which( is.na(strata$RETAIN_REASON_CODE))
            invalid_retain_reasons = which(!is.na(strata$RETAIN_REASON_CODE) & !is_retain_reason_valid(strata$RETAIN_REASON_CODE))
            invalid_retain_reasons = invalid_retain_reasons[ ! invalid_retain_reasons %in% missing_retain_reasons ]
            missing_retain_reasons = missing_retain_reasons[ ! missing_retain_reasons %in% strata_empty_rows ]

            species_table       = table(records$codes$species)

            if(nrow(species_table) > 0) {
              species_occurrences = as.data.table(species_table)
              colnames(species_occurrences) = c("SPECIES_CODE", "NUM_OCCURRENCES")

              species_occurrences_multiple = species_occurrences[NUM_OCCURRENCES > 1]
              species_multiple = which(records$codes$species %in% species_occurrences_multiple$SPECIES_CODE)
            } else {
              species_occurrences_multiple = data.table(SPECIES_CODE = character(), NUM_OCCURRENCIES = integer())
              species_multiple = as.integer(array())
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

            data_validation_results$strata$checks$main$retain_reasons =
              list(
                invalid = list(
                  number       = length(invalid_retain_reasons),
                  row_indexes  = spreadsheet_rows_for(form, invalid_retain_reasons),
                  codes        = strata[invalid_retain_reasons]$RETAIN_REASON_CODE,
                  codes_unique = unique(strata[invalid_retain_reasons]$RETAIN_REASON_CODE)
                ),
                missing = list(
                  number      = length(missing_retain_reasons),
                  row_indexes = spreadsheet_rows_for(form, missing_retain_reasons)
                )
              )

            data_validation_results$records$checks$species$multiple =
              list(
                  number       = length(species_multiple),
                  col_indexes  = spreadsheet_cols_for(form, species_multiple),
                  codes_unique = species_occurrences_multiple$SPECIES_CODE
              )

            return(data_validation_results)
          }
)

setMethod("data_validation_summary",
          list(form = "IOTCForm1RC", metadata_validation_results = "list", data_validation_results = "list"),
          function(form, metadata_validation_results, data_validation_results) {
            l_info("IOTCForm1RC.data_validation_summary")

            validation_messages = common_data_validation_summary(form,
                                                                 metadata_validation_results,
                                                                 data_validation_results)

            ### STRATA AND RECORDS

            # This is only true for 1RC, whereas for 1DI the total should be raised, but can be expressed also as total numbers
            validation_messages = add(validation_messages, new("Message", level = "INFO", source = "Data", text = "Provided catch data is assumed to be raised to total annual catches and reported in live-weight equivalent by default"))

            strata  = data_validation_results$strata
            records = data_validation_results$records

            checks_strata  = strata$checks
            checks_records = records$checks

            # Strata issues / summary

            # Validation comes from the superclass

            # Strata checks

            ## Main strata

            checks_strata_main = checks_strata$main

            # Part of the validation comes from the superclass

            validation_messages = report_retain_reasons(validation_messages, checks_strata_main$retain_reasons)

            ## Original data

            # Validation comes from the superclass

            ###

            # Data issues / summary

            ## Species

            species = checks_records$species

            # Part of the validation comes from the superclass

            if(species$multiple$number > 0)   # Multiple
              validation_messages = add(validation_messages, new("Message", level = "ERROR", source = "Data", text = paste0("Repeated species in column(s) #", paste0(species$multiple$col_indexes, collapse = ", "))))

            ## Catches

            # Validation comes from the superclass

            return(validation_messages)
          }
)

## OUTPUT

setMethod("extract_output", list(form = "IOTCForm1RC", wide = "logical"),
  function(form, wide) {
    form = read(form)

    form_metadata = extract_metadata(form, common_metadata(form@original_metadata))
    form_data     = extract_data(form)

    strata = form_data$strata
    data   = form_data$records$data$catches
    colnames(data) = form_data$records$codes$species

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
                        IOTC_MAIN_AREA_CODE, RETAIN_REASON_CODE,
                        CATCH_UNIT_CODE = "T")]

    data = data[, lapply(.SD, function(value) { return(ifelse(is.na(value) | value == 0, NA_real_, round(as.numeric(value), 2))) })]

    output_data = cbind(strata, data)

    if(!wide) {
      output_data = melt.data.table(output_data,
                                    id.vars = 1:18,
                                    value.name = "CATCH",
                                    variable.name = "SPECIES_CODE")[!is.na(CATCH) & CATCH > 0]
    }

    return(output_data)
  }
)
